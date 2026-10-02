;;; jira.el --- Read Jira Cloud through a browser bridge -*- lexical-binding: t; -*-

;; Requires Emacs 27+, bridge.el, the bridge server, and the
;; jira-bridge userscript running in a logged-in Jira tab.

;;; Commentary:
;;
;;   M-x jira-my-issues       your unresolved issues, newest activity first
;;   M-x jira-search          plain text, or raw JQL if it contains = ~ or ORDER BY
;;   M-x jira-open-issue      open an issue by key (e.g. PROJ-123)
;;   M-x jira-open-current    open the issue your browser tab is on
;;
;; In a list buffer:  RET open issue, M load more, g refresh.
;; In an issue buffer: RET follows a link (Jira issue links stay in
;; Emacs), v opens in the browser, w copies the key, g reloads.
;;
;; Read-only.  The search uses /rest/api/3/search/jql, which pages with
;; a token and has no total count.

;;; Code:

(require 'cl-lib)
(require 'bridge)
(require 'shr)
(require 'dom)
(require 'subr-x)
(require 'tabulated-list)

(defun jira--call (action &optional params)
  (bridge-call "jira" action params))

(defun jira--get (obj &rest keys)
  "Walk nested alists by KEYS, e.g. fields, then status, then name."
  (dolist (k keys obj)
    (setq obj (and (consp obj) (alist-get k obj)))))

(defconst jira--key-regexp "[A-Z][A-Z0-9_]*-[0-9]+")

;;;; Issue view

(defvar-local jira--issue nil "Issue data shown in this buffer.")

(defvar jira-link-map nil "Keymap on links in issue buffers.")

(defvar jira-issue-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "v") #'jira-browse-issue)
    (define-key m (kbd "w") #'jira-copy-key)
    (define-key m (kbd "g") #'jira-reload)
    m))

(define-derived-mode jira-issue-mode special-mode "Jira"
  "Major mode for reading a Jira issue."
  (setq-local truncate-lines nil))

(defun jira--heading (text)
  (insert "\n" (propertize text 'face 'bold) "\n"))

(defun jira--render-issue (issue comments)
  "Render ISSUE and its COMMENTS (a list) into the current buffer."
  (let* ((key (alist-get 'key issue))
         (f (alist-get 'fields issue))
         (inhibit-read-only t)
         (line
          (lambda (&rest pairs)
            (string-join
             (delq nil
                   (cl-loop for (label val) on pairs by #'cddr
                            when (and val (not (equal val "")))
                            collect (format "%s: %s" label val)))
             "  |  "))))
    (erase-buffer)
    (setq jira-link-map (or jira-link-map (bridge-link-map #'jira-follow-link)))
    (insert (propertize (format "%s  %s" key (alist-get 'summary f)) 'face 'bold) "\n")
    (insert (propertize
             (funcall line
                      "Type" (jira--get f 'issuetype 'name)
                      "Status" (jira--get f 'status 'name)
                      "Priority" (jira--get f 'priority 'name)
                      "Project" (jira--get f 'project 'name))
             'face 'shadow)
            "\n")
    (insert (propertize
             (funcall line
                      "Assignee" (or (jira--get f 'assignee 'displayName) "Unassigned")
                      "Reporter" (jira--get f 'reporter 'displayName)
                      "Updated" (alist-get 'updated f))
             'face 'shadow)
            "\n")
    (let ((labels (alist-get 'labels f))
          (parent (jira--get f 'parent 'key))
          (atts (mapcar (lambda (a) (alist-get 'filename a)) (alist-get 'attachment f))))
      (when (or labels parent atts)
        (insert (propertize
                 (funcall line
                          "Parent" parent
                          "Labels" (and labels (string-join labels ", "))
                          "Attachments" (and atts (string-join atts ", ")))
                 'face 'shadow)
                "\n")))
    (jira--heading "Description")
    (let ((html (jira--get issue 'renderedFields 'description)))
      (if (and html (not (string-empty-p html)))
          (bridge-insert-html html "jira" jira-link-map)
        (insert (propertize "(none)" 'face 'shadow) "\n")))
    (jira--heading (format "Comments (%d)" (length comments)))
    (dolist (c comments)
      (insert "\n"
              (propertize (format "%s, %s"
                                  (or (jira--get c 'author 'displayName) "?")
                                  (or (alist-get 'created c) ""))
                          'face 'italic)
              "\n")
      (bridge-insert-html (alist-get 'renderedBody c) "jira" jira-link-map))
    (goto-char (point-min))))

;;;###autoload
(defun jira-open-issue (key)
  "Open Jira issue KEY in Emacs."
  (interactive "sIssue key: ")
  (let* ((key (upcase (string-trim key)))
         (issue (jira--call "issue" `((key . ,key))))
         (comments (alist-get 'comments (jira--call "comments" `((key . ,key)))))
         (buf (get-buffer-create (format "*jira: %s*" key))))
    (with-current-buffer buf
      (jira-issue-mode)
      (setq jira--issue issue)
      (jira--render-issue issue comments))
    (pop-to-buffer buf)))

;;;###autoload
(defun jira-open-current ()
  "Open, in Emacs, the issue the connected browser tab is showing."
  (interactive)
  (let ((key (alist-get 'key (jira--call "current"))))
    (unless key (user-error "The current tab is not showing an issue"))
    (jira-open-issue key)))

(defun jira-follow-link ()
  "Follow the link at point; Jira issues open in Emacs."
  (interactive)
  (let ((url (get-text-property (point) 'shr-url)))
    (unless url (user-error "No link at point"))
    (let* ((abs (bridge-absolute "jira" url))
           (base (bridge-site-base "jira")))
      (if (and (string-prefix-p base abs)
               (string-match (concat "/browse/\\(" jira--key-regexp "\\)") abs))
          (jira-open-issue (match-string 1 abs))
        (browse-url abs)))))

(defun jira-browse-issue ()
  "Open the current issue in the browser."
  (interactive)
  (let ((key (alist-get 'key jira--issue)))
    (unless key (user-error "Not an issue buffer"))
    (browse-url (concat (bridge-site-base "jira") "/browse/" key))))

(defun jira-copy-key ()
  "Copy the current issue key."
  (interactive)
  (let ((key (alist-get 'key jira--issue)))
    (unless key (user-error "Not an issue buffer"))
    (kill-new key)
    (message "Copied %s" key)))

(defun jira-reload ()
  "Reload the issue in this buffer."
  (interactive)
  (let ((key (alist-get 'key jira--issue)))
    (unless key (user-error "Not an issue buffer"))
    (let ((issue (jira--call "issue" `((key . ,key))))
          (comments (alist-get 'comments (jira--call "comments" `((key . ,key)))))
          (pos (point)))
      (setq jira--issue issue)
      (jira--render-issue issue comments)
      (goto-char (min pos (point-max))))))

;;;; Issue lists

(defvar-local jira--list-jql nil)
(defvar-local jira--list-title nil)
(defvar-local jira--list-token nil "nextPageToken for the next page, or nil when exhausted.")

(defvar jira-list-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m tabulated-list-mode-map)
    (define-key m (kbd "RET") #'jira-list-open)
    (define-key m (kbd "M") #'jira-list-more)
    (define-key m (kbd "g") #'jira-list-refresh)
    m))

(define-derived-mode jira-list-mode tabulated-list-mode "Jira-List"
  "Major mode for lists of Jira issues."
  (setq tabulated-list-format
        [("Key" 12 t) ("Status" 14 t) ("Type" 10 t) ("Assignee" 16 t) ("Summary" 0 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun jira--entry (issue)
  (let ((f (alist-get 'fields issue)))
    (list (alist-get 'key issue)
          (vector (alist-get 'key issue)
                  (or (jira--get f 'status 'name) "")
                  (or (jira--get f 'issuetype 'name) "")
                  (or (jira--get f 'assignee 'displayName) "-")
                  (or (alist-get 'summary f) "")))))

(defun jira--list-load (&optional more)
  "Fetch a page of results into the current list buffer.
With MORE, append the next page instead of starting over."
  (let* ((params `((jql . ,jira--list-jql) (limit . 50)))
         (params (if (and more jira--list-token)
                     (append params `((nextPageToken . ,jira--list-token)))
                   params))
         (data (jira--call "search" params))
         (issues (alist-get 'issues data)))
    (setq jira--list-token (and (not (alist-get 'isLast data))
                                (alist-get 'nextPageToken data)))
    (setq tabulated-list-entries
          (append (and more tabulated-list-entries) (mapcar #'jira--entry issues)))
    (tabulated-list-print t)
    (message "%d issues%s" (length tabulated-list-entries)
             (if jira--list-token " (M for more)" ""))))

(defun jira--show-list (title jql)
  (let ((buf (get-buffer-create (format "*jira: %s*" title))))
    (with-current-buffer buf
      (jira-list-mode)
      (setq jira--list-title title jira--list-jql jql jira--list-token nil)
      (jira--list-load))
    (pop-to-buffer buf)))

(defun jira-list-open ()
  "Open the issue on this line."
  (interactive)
  (let ((key (tabulated-list-get-id)))
    (unless key (user-error "No issue on this line"))
    (jira-open-issue key)))

(defun jira-list-more ()
  "Load the next page of results."
  (interactive)
  (unless jira--list-token (user-error "No more results"))
  (jira--list-load t))

(defun jira-list-refresh ()
  "Re-run the query."
  (interactive)
  (setq jira--list-token nil)
  (jira--list-load))

(defun jira--escape (s)
  (replace-regexp-in-string "\"" "\\\\\"" s))

;;;###autoload
(defun jira-my-issues ()
  "List your unresolved issues."
  (interactive)
  (jira--show-list "my issues"
                   "assignee = currentUser() AND resolution = Unresolved ORDER BY updated DESC"))

;;;###autoload
(defun jira-search (query)
  "Search Jira for QUERY.
Raw JQL is used if QUERY contains = or ~ or ORDER BY; a bare issue
key opens that issue; anything else is a text search."
  (interactive "sJira search: ")
  (let ((q (string-trim query)))
    (cond
     ((string-match-p (concat "\\`" jira--key-regexp "\\'") (upcase q))
      (jira-open-issue q))
     ((let ((case-fold-search t)) (string-match-p "[=~]\\|order by" q))
      (jira--show-list q q))
     (t
      (jira--show-list
       q (format "text ~ \"%s\" ORDER BY updated DESC" (jira--escape q)))))))

(provide 'jira)
;;; jira.el ends here
