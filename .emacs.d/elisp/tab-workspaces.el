;;; tab-workspaces.el --- Workspace management via tab-bar -*- lexical-binding: t -*-

;; Workspace management using built-in tab-bar-mode.
;; The tab bar itself is hidden; workspaces are managed via keybindings
;; and displayed in the echo area.
;;
;; Keybindings (under C-x t prefix):
;;   C-x t t   - display workspaces in echo area
;;   C-x t N   - new blank workspace
;;   C-x t P   - switch to project in dedicated workspace
;;   C-x t b   - switch buffer (workspace-scoped)
;;   C-x t K   - kill all buffers in current workspace
;;   C-x t 1-9 - jump directly to workspace by number

;;; Code:

(require 'cl-lib)

;;; Faces

(defface rt-workspace-tab-face
  '((t (:inherit default)))
  "Face for inactive workspace tabs in the echo area.")

(defface rt-workspace-tab-selected-face
  '((t (:inherit highlight)))
  "Face for the active workspace tab in the echo area.")

;;; Core helpers

(defun rt-workspace--fallback-buffer ()
  "Return the scratch buffer, creating it if necessary."
  (get-buffer-create "*scratch*"))

(defun rt-workspace--current-name ()
  "Return the name of the current workspace."
  (alist-get 'name (tab-bar--current-tab)))

(defun rt-workspace--get-names ()
  "Return a list of all workspace names."
  (mapcar (lambda (tab) (alist-get 'name tab))
          (funcall tab-bar-tabs-function)))

;;; Echo area display

(defun rt-workspace--format-tab (index name is-current)
  "Format a single workspace tab for echo area display.
INDEX is 0-based position, NAME is the workspace name, IS-CURRENT
indicates whether this is the active workspace."
  (let* ((num (1+ index))
         (label (if (and name (not (string-empty-p name)))
                    (format "[%d] %s" num name)
                  (number-to-string num)))
         (text (if is-current
                   (format "(%s) " label)
                 (format " %s " label)))
         (face (if is-current
                   'rt-workspace-tab-selected-face
                 'rt-workspace-tab-face)))
    (propertize text 'face face)))

(defun rt-workspace--tabline ()
  "Return a formatted workspace tabline string for the echo area.
Wraps to multiple lines when tabs exceed the frame width."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-index (tab-bar--current-tab-index tabs))
         (max-width (frame-width))
         (line-len 0)
         parts)
    (cl-loop for tab in tabs
             for idx from 0
             do (let* ((segment (rt-workspace--format-tab
                                 idx
                                 (alist-get 'name tab)
                                 (eq idx current-index)))
                       (seg-len (length segment)))
                  (when (and (> line-len 0)
                             (> (+ line-len seg-len) max-width))
                    (push "\n" parts)
                    (setq line-len 0))
                  (push segment parts)
                  (cl-incf line-len seg-len)))
    (apply #'concat (nreverse parts))))

(defun rt-workspace-display ()
  "Display all workspaces in the echo area."
  (interactive)
  (let (message-log-max)
    (message "%s" (rt-workspace--tabline))))

;; Show workspace list after tab operations.
(defun rt-workspace--display-after (&rest _)
  (rt-workspace-display))

(defvar rt-workspace--last-kill-count nil
  "Temporary storage for buffer kill count during workspace close.")

(defun rt-workspace--display-after-close (&rest _)
  (let ((count rt-workspace--last-kill-count))
    (setq rt-workspace--last-kill-count nil)
    (let (message-log-max)
      (message "%s | Closed workspace, killed %d buffer(s)"
               (rt-workspace--tabline) (or count 0)))))

(dolist (fn '(tab-bar-new-tab
              tab-bar-switch-to-tab
              tab-bar-switch-to-next-tab
              tab-bar-switch-to-prev-tab
              tab-bar-select-tab
              tab-bar-rename-tab))
  (advice-add fn :after #'rt-workspace--display-after))

(advice-add 'tab-bar-close-tab :after #'rt-workspace--display-after-close)

;;; Workspace commands

(defun rt-workspace-new ()
  "Create a new blank workspace.
Opens with a single window showing the scratch buffer, rooted at home."
  (interactive)
  (let ((tab-bar-new-tab-choice #'rt-workspace--fallback-buffer))
    (tab-bar-new-tab)
    (delete-other-windows)
    (switch-to-buffer (rt-workspace--fallback-buffer))
    (setq-local default-directory "~/")))

(defun rt-workspace-switch-to-project ()
  "Switch to a project in a dedicated workspace.
Reuses an existing workspace if one already exists for that project."
  (interactive)
  (let* ((dir (project-prompt-project-dir))
         (name (file-name-nondirectory (directory-file-name dir)))
         (existing (member name (rt-workspace--get-names))))
    (if existing
        (tab-bar-switch-to-tab name)
      (let* ((tab-bar-new-tab-choice #'rt-workspace--fallback-buffer)
             (pr (project-current nil dir))
             (root (if pr (project-root pr) dir)))
        (tab-bar-new-tab)
        (tab-bar-rename-tab name)
        (delete-other-windows)
        (setq default-directory root)
        (project-find-file)))))

;;; Buffer tracking

(defvar rt-workspace-buffer-alist nil
  "Alist mapping workspace names to their buffer lists.")

(defun rt-workspace--get-buffers ()
  "Return the buffer list for the current workspace."
  (alist-get (rt-workspace--current-name)
             rt-workspace-buffer-alist nil nil #'equal))

(defun rt-workspace--add-buffer (buffer)
  "Add BUFFER to the current workspace's buffer list."
  (let* ((name (rt-workspace--current-name))
         (buffers (alist-get name rt-workspace-buffer-alist nil nil #'equal)))
    (unless (memq buffer buffers)
      (setf (alist-get name rt-workspace-buffer-alist nil nil #'equal)
            (cons buffer buffers)))))

(defun rt-workspace--remove-killed-buffer ()
  "Remove the current buffer from all workspace buffer lists."
  (let ((buf (current-buffer)))
    (dolist (entry rt-workspace-buffer-alist)
      (setcdr entry (delq buf (cdr entry))))))

(add-hook 'kill-buffer-hook #'rt-workspace--remove-killed-buffer)

(defun rt-workspace--track-buffer (&rest _)
  "Track the current buffer in the workspace's buffer list."
  (when-let* ((buf (current-buffer)))
    (unless (minibufferp buf)
      (rt-workspace--add-buffer buf))))

(add-hook 'window-buffer-change-functions #'rt-workspace--track-buffer)

(defun rt-workspace-buffer-list ()
  "Return the list of live buffers for the current workspace."
  (seq-filter #'buffer-live-p (rt-workspace--get-buffers)))

(defun rt-workspace--kill-buffers (buffers)
  "Kill BUFFERS, skipping the scratch buffer. Return count killed."
  (let ((count 0))
    (dolist (buf buffers)
      (when (and (buffer-live-p buf)
                 (not (eq buf (rt-workspace--fallback-buffer))))
        (kill-buffer buf)
        (cl-incf count)))
    count))

(defun rt-workspace--kill-buffers-on-close (&optional tab-number &rest _)
  "Kill tracked buffers for the workspace being closed."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (idx (if tab-number
                  (1- tab-number)
                (tab-bar--current-tab-index tabs)))
         (name (alist-get 'name (nth idx tabs)))
         (buffers (alist-get name rt-workspace-buffer-alist nil nil #'equal)))
    (setq rt-workspace--last-kill-count (rt-workspace--kill-buffers buffers))
    (setq rt-workspace-buffer-alist
          (assoc-delete-all name rt-workspace-buffer-alist #'equal))))

(advice-add 'tab-bar-close-tab :before #'rt-workspace--kill-buffers-on-close)

(defun rt-workspace-kill-all-buffers ()
  "Kill all tracked buffers in the current workspace."
  (interactive)
  (let ((buffers (rt-workspace-buffer-list)))
    (when (yes-or-no-p (format "Kill %d buffer(s) in this workspace? "
                               (length buffers)))
      (let ((count (rt-workspace--kill-buffers buffers)))
        (switch-to-buffer (rt-workspace--fallback-buffer))
        (delete-other-windows)
        (message "Killed %d buffer(s)" count)))))

;;; Workspace-scoped buffer switching

(defun rt-workspace-switch-buffer ()
  "Switch to a buffer in the current workspace using consult."
  (interactive)
  (let ((workspace-buffers (rt-workspace-buffer-list)))
    (if workspace-buffers
        (switch-to-buffer
         (completing-read "Switch to buffer: "
                          (mapcar #'buffer-name workspace-buffers)
                          nil t))
      (call-interactively #'consult-buffer))))

;;; Numbered workspace switchers 1-9

(defmacro rt-workspace--define-switchers ()
  "Define rt-workspace-switch-to-N commands for N in 1..9."
  `(progn
     ,@(mapcar (lambda (n)
                 `(defun ,(intern (format "rt-workspace-switch-to-%d" n)) ()
                    ,(format "Switch to workspace %d." n)
                    (interactive)
                    (if (<= ,n (length (tab-bar-tabs)))
                        (tab-bar-select-tab ,n)
                      (message "Workspace %d does not exist" ,n))))
               (number-sequence 1 9))))

(rt-workspace--define-switchers)

(provide 'tab-workspaces)
;;; tab-workspaces.el ends here
