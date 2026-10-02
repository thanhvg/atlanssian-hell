;;; bridge.el --- Shared client for the browser bridge server -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Transport and HTML rendering shared by jira.el (and, if you like,
;; confluence.el).  Talks to bridge_server.py over its REST API.
;; Requests are synchronous, so Emacs blocks for the round trip.

;;; Code:

(require 'json)
(require 'url)
(require 'url-expand)
(require 'shr)
(require 'dom)
(require 'subr-x)
(require 'seq)

(defgroup bridge nil "Browser bridge client." :group 'applications)

(defcustom bridge-url "http://127.0.0.1:3979"
  "Base URL of the bridge server's REST API."
  :type 'string)

(defcustom bridge-token-file "~/.config/browser-bridge/token"
  "File holding the bridge's secret token."
  :type 'file)

(defcustom bridge-timeout 40
  "Seconds to wait for the bridge."
  :type 'integer)

;;;; Transport

(defun bridge--token ()
  (let ((f (expand-file-name bridge-token-file)))
    (unless (file-readable-p f)
      (user-error "Bridge token not found at %s; start bridge_server.py first" f))
    (string-trim (with-temp-buffer (insert-file-contents f) (buffer-string)))))

(defun bridge--request (method path &optional body)
  "Send METHOD to the bridge at PATH with optional JSON BODY; return `result'."
  (let* ((url-request-method method)
         (url-request-extra-headers
          `(("X-Bridge-Token" . ,(bridge--token))
            ("Content-Type" . "application/json")))
         (url-request-data (and body (encode-coding-string (json-encode body) 'utf-8)))
         (buf (condition-case err
                  (url-retrieve-synchronously (concat bridge-url path) t t bridge-timeout)
                (error (user-error "Cannot reach bridge: %s" (error-message-string err))))))
    (unless buf (user-error "Bridge did not answer within %ds" bridge-timeout))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (unless (re-search-forward "\r?\n\r?\n" nil t)
            (user-error "Malformed response from bridge"))
          (let ((data (json-parse-string
                       (decode-coding-string
                        (buffer-substring-no-properties (point) (point-max)) 'utf-8)
                       :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))
            (if-let ((err (alist-get 'error data)))
                (user-error "Bridge: %s" err)
              (alist-get 'result data))))
      (kill-buffer buf))))

(defun bridge-call (site action &optional params)
  "Call ACTION with PARAMS (an alist) on the most recent tab for SITE."
  (bridge--request "POST" "/call"
                   `((site . ,site) (action . ,action) (params . ,(or params '())))))

(defun bridge-tabs ()
  "List connected tabs."
  (bridge--request "GET" "/tabs"))

(defun bridge-site-base (site)
  "Origin of the connected tab for SITE, e.g. https://acme.atlassian.net."
  (let* ((tab (seq-find (lambda (x) (equal (alist-get 'site x) site)) (bridge-tabs)))
         (url (and tab (alist-get 'url tab))))
    (if (and url (string-match "\\`\\(https://[^/]+\\)" url))
        (match-string 1 url)
      (user-error "No %s tab connected" site))))

(defun bridge-absolute (site url)
  "Make URL absolute against SITE's origin."
  (if (string-match-p "\\`https?://" url)
      url
    (url-expand-file-name url (concat (bridge-site-base site) "/"))))

;;;; Rendering

(defun bridge-show-image (site src)
  "Fetch SRC through SITE's tab and display it."
  (let* ((uri (bridge-call site "blob" `((path . ,src))))
         (b64 (and (stringp uri)
                   (string-match "\\`data:[^,]*;base64,\\(.*\\)\\'" uri)
                   (match-string 1 uri))))
    (unless b64 (user-error "Could not load image"))
    (with-current-buffer (get-buffer-create "*bridge-image*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-image (create-image (base64-decode-string b64) nil t))
        (special-mode))
      (pop-to-buffer (current-buffer)))))

(defun bridge--render-img (site dom)
  "Render an <img> as a button that fetches the image through SITE's tab."
  (let ((src (dom-attr dom 'src))
        (alt (or (dom-attr dom 'alt) "image")))
    (if (not src)
        (insert (format "[%s]" alt))
      (insert-text-button
       (format "[image: %s]" alt)
       'follow-link t
       'help-echo "RET: show image"
       'action (lambda (_) (bridge-show-image site src))))))

(defun bridge-insert-html (html site link-map)
  "Insert HTML at the end of the buffer using shr.
Images become buttons fetched through SITE's tab; every link gets
LINK-MAP as its keymap (so RET can be rerouted)."
  (goto-char (point-max))
  (let* ((start (point))
         (dom (with-temp-buffer
                (insert (or html ""))
                (libxml-parse-html-region (point-min) (point-max))))
         (shr-external-rendering-functions
          `((img . ,(lambda (d) (bridge--render-img site d))))))
    (shr-insert-document dom)
    (goto-char (point-max))
    (let ((pos start) end)
      (while (< pos (point-max))
        (setq end (or (next-single-property-change pos 'shr-url) (point-max)))
        (when (get-text-property pos 'shr-url)
          (put-text-property pos end 'keymap link-map))
        (setq pos end)))))

(defun bridge-link-map (follow-command)
  "A keymap like `shr-map' whose RET runs FOLLOW-COMMAND."
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m shr-map)
    (define-key m (kbd "RET") follow-command)
    (define-key m [mouse-2] follow-command)
    m))

(provide 'bridge)
;;; bridge.el ends here
