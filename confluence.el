;;; confluence.el --- Read Confluence Cloud through a browser bridge -*- lexical-binding: t; -*-

;; Requires Emacs 27+ (json-parse-string, libxml), and the bridge server
;; plus the confluence-bridge userscript running in a logged-in tab.

;;; Commentary:
;;
;;   M-x confluence-open-current   open the page your browser tab is on
;;   M-x confluence-search         search (plain text, or raw CQL if it has = or ~)
;;   M-x confluence-open-page      open a page by numeric id
;;
;; In a page buffer: RET follows a link or breadcrumb entry (Confluence page
;; links stay in Emacs), ^ opens the parent page, v opens the page in the
;; browser, g reloads, q quits.
;;
;; Requests are synchronous, so Emacs blocks for the round trip.

;;; Code:

(require 'json)
(require 'url)
(require 'shr)
(require 'dom)
(require 'subr-x)

(defgroup confluence nil "Confluence reader." :group 'applications)

(defcustom confluence-bridge-url "http://127.0.0.1:1979"
  "Base URL of the bridge server's REST API."
  :type 'string)

(defcustom confluence-bridge-token-file "~/.config/browser-bridge/token"
  "File holding the bridge's secret token."
  :type 'file)

(defcustom confluence-timeout 40
  "Seconds to wait for the bridge."
  :type 'integer)

(defvar-local confluence--page nil "Page data shown in this buffer.")

;;;; Bridge transport

(defun confluence--token ()
  (let ((f (expand-file-name confluence-bridge-token-file)))
    (unless (file-readable-p f)
      (user-error "Bridge token not found at %s; start bridge_server.py first" f))
    (string-trim (with-temp-buffer (insert-file-contents f) (buffer-string)))))

(defun confluence--request (method path &optional body)
  "Send METHOD to the bridge at PATH with optional JSON BODY; return `result'."
  (let* ((url-request-method method)
         (url-request-extra-headers
          `(("X-Bridge-Token" . ,(confluence--token))
            ("Content-Type" . "application/json")))
         (url-request-data (and body (encode-coding-string (json-encode body) 'utf-8)))
         (buf (condition-case err
                  (url-retrieve-synchronously
                   (concat confluence-bridge-url path) t t confluence-timeout)
                (error (user-error "Cannot reach bridge: %s" (error-message-string err))))))
    (unless buf (user-error "Bridge did not answer within %ds" confluence-timeout))
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

(defun confluence--call (action &optional params)
  (confluence--request "POST" "/call"
                       `((site . "confluence") (action . ,action) (params . ,(or params '())))))

(defun confluence--base ()
  "Origin of the connected Confluence tab, e.g. https://acme.atlassian.net."
  (let* ((tabs (confluence--request "GET" "/tabs"))
         (tab (seq-find (lambda (x) (equal (alist-get 'site x) "confluence")) tabs))
         (url (and tab (alist-get 'url tab))))
    (if (and url (string-match "\\`\\(https://[^/]+\\)" url))
        (match-string 1 url)
      (user-error "No Confluence tab connected"))))

;;;; Rendering

(defvar confluence-link-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m shr-map)
    (define-key m (kbd "RET") #'confluence-follow-link)
    (define-key m [mouse-2] #'confluence-follow-link)
    m)
  "Keymap placed on links so RET stays inside Emacs for Confluence pages.")

(defvar confluence-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "v") #'confluence-browse-page)
    (define-key m (kbd "g") #'confluence-reload)
    (define-key m (kbd "^") #'confluence-open-parent)
    m))

(define-derived-mode confluence-mode special-mode "Confluence"
  "Major mode for reading Confluence pages."
  (setq-local truncate-lines nil))

(defun confluence--page-id-from-url (url)
  (cond ((string-match "/pages/\\([0-9]+\\)" url) (match-string 1 url))
        ((string-match "[?&]pageId=\\([0-9]+\\)" url) (match-string 1 url))))

(defun confluence--absolute (url)
  (if (string-match-p "\\`https?://" url)
      url
    (url-expand-file-name url (concat (confluence--base) "/"))))

(defun confluence--show-image (src)
  (let* ((uri (confluence--call "blob" `((path . ,src))))
         (b64 (and (string-match "\\`data:[^,]*;base64,\\(.*\\)\\'" uri)
                   (match-string 1 uri))))
    (unless b64 (user-error "Could not load image"))
    (with-current-buffer (get-buffer-create "*confluence-image*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-image (create-image (base64-decode-string b64) nil t))
        (special-mode))
      (pop-to-buffer (current-buffer)))))

(defun confluence--render-img (dom)
  "Render an <img> as a button that fetches the image through the bridge."
  (let ((src (dom-attr dom 'src))
        (alt (or (dom-attr dom 'alt) "image")))
    (if (not src)
        (insert (format "[%s]" alt))
      (insert-text-button
       (format "[image: %s]" alt)
       'follow-link t
       'help-echo "RET: show image"
       'action (lambda (_) (confluence--show-image src))))))

(defun confluence--hijack-links ()
  "Put `confluence-link-map' on every shr link in the buffer."
  (let ((pos (point-min)) end)
    (while (< pos (point-max))
      (setq end (or (next-single-property-change pos 'shr-url) (point-max)))
      (when (get-text-property pos 'shr-url)
        (put-text-property pos end 'keymap confluence-link-map))
      (setq pos end))))

;; Folders and other non-page ancestors can't be opened as pages, so only
;; "page" ancestors (or ones with no type given) become buttons.
(defun confluence--ancestor-page-p (a)
  (and (alist-get 'id a) (member (alist-get 'type a) '("page" nil))))

(defun confluence--insert-breadcrumb (ancestors)
  "Insert ANCESTORS as a \"A > B > C\" trail of buttons that open each page."
  (let ((first t))
    (dolist (a ancestors)
      (unless first (insert " > "))
      (setq first nil)
      (let ((id (alist-get 'id a))
            (title (or (alist-get 'title a) "?")))
        (if (confluence--ancestor-page-p a)
            (insert-text-button
             title
             'face 'button          ; explicit, so the header's `shadow' is merged behind it
             'follow-link t
             'help-echo (format "RET: open %s" title)
             'action (lambda (_) (confluence-open-page id)))
          (insert title))))))

(defun confluence--render (page)
  "Render PAGE (alist from the API) into the current buffer."
  (let* ((title (alist-get 'title page))
         (space (alist-get 'name (alist-get 'space page)))
         (ancestors (alist-get 'ancestors page))
         (when-str (alist-get 'when (alist-get 'version page)))
         (labels (mapcar (lambda (l) (alist-get 'name l))
                         (alist-get 'results (alist-get 'labels (alist-get 'metadata page)))))
         (html (or (alist-get 'value (alist-get 'view (alist-get 'body page))) ""))
         (dom (with-temp-buffer
                (insert html)
                (libxml-parse-html-region (point-min) (point-max))))
         (shr-external-rendering-functions '((img . confluence--render-img)))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize title 'face 'bold) "\n")
    (let ((parts
           (delq nil
                 (list (and space (lambda () (insert (format "Space: %s" space))))
                       (and ancestors (lambda () (confluence--insert-breadcrumb ancestors)))
                       (and when-str (lambda () (insert (format "Updated: %s" when-str))))
                       (and labels (lambda ()
                                     (insert (format "Labels: %s" (string-join labels ", "))))))))
          (start (point))
          (first t))
      (dolist (part parts)
        (unless first (insert "  |  "))
        (setq first nil)
        (funcall part))
      ;; append, so the buttons keep their own face
      (add-face-text-property start (point) 'shadow t))
    (insert "\n\n")
    (shr-insert-document dom)
    (confluence--hijack-links)
    (goto-char (point-min))))

;;;; Commands

;;;###autoload
(defun confluence-open-page (id)
  "Open the Confluence page with numeric ID."
  (interactive "sPage id: ")
  (let* ((page (confluence--call "page" `((id . ,id))))
         (buf (get-buffer-create (format "*confluence: %s*" (alist-get 'title page)))))
    (with-current-buffer buf
      (confluence-mode)
      (setq confluence--page page)
      (confluence--render page))
    (pop-to-buffer buf)))

;;;###autoload
(defun confluence-open-current ()
  "Open, in Emacs, the page the connected browser tab is showing."
  (interactive)
  (let ((id (alist-get 'id (confluence--call "current"))))
    (unless id (user-error "The current tab is not a Confluence page"))
    (confluence-open-page id)))

(defun confluence--strip-highlight (s)
  (replace-regexp-in-string "@@@\\(?:end\\)?hl@@@" "" (or s "")))

;;;###autoload
(defun confluence-search (query)
  "Search Confluence for QUERY.  Raw CQL is used if QUERY contains = or ~."
  (interactive "sConfluence search: ")
  (let* ((cql (if (string-match-p "[=~]" query)
                  query
                (format "type = page AND text ~ \"%s\""
                        (replace-regexp-in-string "\"" "\\\\\"" query))))
         (data (confluence--call "search" `((cql . ,cql) (limit . 25))))
         (cands
          (delq nil
                (mapcar
                 (lambda (r)
                   (let* ((c (alist-get 'content r))
                          (id (alist-get 'id c)))
                     (when id
                       (cons (format "%s  [%s]"
                                     (confluence--strip-highlight (alist-get 'title r))
                                     (or (alist-get 'title (alist-get 'resultGlobalContainer r))
                                         "?"))
                             id))))
                 (alist-get 'results data)))))
    (unless cands (user-error "No results for: %s" cql))
    (let ((choice (completing-read "Page: " cands nil t)))
      (confluence-open-page (cdr (assoc choice cands))))))

(defun confluence-follow-link ()
  "Follow the link at point; Confluence pages open in Emacs."
  (interactive)
  (let ((url (get-text-property (point) 'shr-url)))
    (unless url (user-error "No link at point"))
    (let ((abs (confluence--absolute url))
          (base (confluence--base)))
      (if-let ((id (and (string-prefix-p base abs)
                        (confluence--page-id-from-url abs))))
          (confluence-open-page id)
        (browse-url abs)))))

(defun confluence-browse-page ()
  "Open the current page in the browser."
  (interactive)
  (let* ((links (alist-get '_links confluence--page))
         (base (alist-get 'base links))
         (webui (alist-get 'webui links)))
    (unless (and base webui) (user-error "No URL known for this page"))
    (browse-url (concat base webui))))

(defun confluence-open-parent ()
  "Open the parent of the current page."
  (interactive)
  (let ((parent (car (last (seq-filter #'confluence--ancestor-page-p
                                       (alist-get 'ancestors confluence--page))))))
    (unless parent (user-error "This page has no parent page"))
    (confluence-open-page (alist-get 'id parent))))

(defun confluence-reload ()
  "Reload the page in this buffer."
  (interactive)
  (let ((id (alist-get 'id confluence--page)))
    (unless id (user-error "Not a Confluence page buffer"))
    (let ((page (confluence--call "page" `((id . ,id))))
          (pos (point)))
      (setq confluence--page page)
      (confluence--render page)
      (goto-char (min pos (point-max))))))

(provide 'confluence)
;;; confluence.el ends here
