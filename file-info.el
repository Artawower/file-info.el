;;; file-info.el --- Show pretty information about current file                     -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/file-info.el
;; Package-Requires: ((emacs "28.1") (hydra "0.15.0") (browse-at-remote "0.15.0"))
;; Version: 0.3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Show pretty information about current opened file.
;; You can copy any information via hydra
;;

;;; Code:

(require 'subr-x)
(require 'hydra)
(require 'browse-at-remote)

(defcustom file-info-buffer-name "*pretty-file-info*"
  "Name of buffer for file info."
  :group 'file-info
  :type 'string)

(defcustom file-info-min-properties-length 26
  "Minimal length of properties."
  :group 'file-info
  :type 'integer)

(defcustom file-info-properties-color "#FF3399"
  "Color of properties."
  :group 'file-info
  :type 'string)

(defcustom file-info-bind-color "#61AFEF"
  "Color of properties."
  :group 'file-info
  :type 'string)

(defcustom file-info-max-value-length 80
  "Max length of value."
  :group 'file-info
  :type 'integer)

(defcustom file-info-show-binding-p t
  "Show binding in hydra."
  :group 'file-info
  :type 'boolean)

(defcustom file-info--include-headlines-p nil
  "Include headlines for groups."
  :group 'file-info
  :type 'boolean)

(defcustom file-info-headline-underline-symbol ?-
  "Symbol for underline headline."
  :group 'file-info
  :type 'character)

(defcustom file-info--max-contributer-count 5
  "Max count of contributors."
  :group 'file-info
  :type 'integer)


(defconst file-info-handlers `(
                               (:handler (file-info--get-headline "File info") :face match)
                               (:name "File name" :prefix (file-info--file-icon) :handler (file-info--get-file-name) :face font-lock-string-face :bind "n")
                               (:name "Project name" :handler (file-info--get-project-name) :face font-lock-string-face :bind "P")
                               (:name "Project related path" :handler (file-info--get-project-related-path) :face font-lock-string-face :bind "D")
                               (:name "File path" :handler (buffer-file-name) :face font-lock-string-face :bind "p")
                               (:name "File dir"
                                      :handler (when (buffer-file-name)
                                                 (replace-regexp-in-string " " "\\\\\  " (file-name-directory (buffer-file-name))))
                                      :face font-lock-string-face
                                      :bind "d")
                               (:name "File mode" :handler (format-mode-line mode-name) :face font-lock-string-face :bind "m")
                               (:name "File size" :handler (file-size-human-readable (buffer-size)) :face font-lock-string-face :bind "s")
                               (:name "Last modified date" :handler (file-info--get-last-modified-date) :face font-lock-string-face :bind "u")
                               (:name "Enabled LSP" :handler (file-info--get-active-lsp-servers) :face font-lock-string-face :bind "L")
                               (:handler (file-info--separator))
                               (:handler (file-info--get-headline "GIT") :face font-lock-comment-face)
                               (:name "Remote GIT" :handler (vc-git-repository-url (file-info--get-file-name)) :face font-lock-builtin-face :bind "r")
                               (:name "Branch" :handler (file-info--get-current-branch) :face font-lock-builtin-face :bind "b")
                               (:name "Remote url" :handler (file-info--get-remote-url) :face font-lock-builtin-face :bind "R")
                               (:name "File author" :handler (file-info--get-first-commit-author) :face font-lock-builtin-face :bind "a")
                               (:name "First commit hash" :handler (file-info--get-first-commit-hash) :face font-lock-builtin-face :bind "H")
                               (:name "Current commit hash" :handler (file-info--get-last-commit-hash) :face font-lock-builtin-face :bind "h")
                               (:name "Contributors"
                                      :handler (file-info--slice-list-by-length (file-info--get-all-file-contributors) file-info--max-contributer-count)
                                      :face font-lock-builtin-face
                                      :bind "C")
                               (:name "First commit date" :handler (file-info--get-first-commit-date) :face font-lock-builtin-face :bind "t")
                               (:name "Modified/deleted lines" :handler (file-info--get-git-file-changes) :bind "w")
                               (:handler (file-info--separator))
                               (:handler (file-info--get-headline "Analytics") :face font-lock-comment-face)
                               (:name "Line count" :handler (number-to-string (count-lines (point-min) (point-max))) :face font-lock-number-face :bind "l")
                               (:name "Words count" :handler (number-to-string (count-words (point-min) (point-max))) :face font-lock-number-face :bind "w")
                               (:name "Errors/info count" :handler (file-info--get-flycheck-errors-count) :bind "e")))

(defun file-info--get-first-commit-info ()
  "Get first commit hash and user name via VC."
  (let ((file-name (file-info--get-file-name)))
    (when file-name
      (with-temp-buffer
        (vc-git-command t 0 file-name "log" "--pretty=format|%H|%an (%ae)|%aD" "--reverse")
        (cdr (split-string (car-safe (split-string (buffer-string) "\n")) "|"))))))

(defun file-info--get-last-commit-info ()
  "Get last commit hash and user name via VC."
  (let ((file-name (file-info--get-file-name)))
    (when file-name
      (with-temp-buffer
        (vc-git-command t 0 file-name "log" "--pretty=format|%H|%an|%cr" "-n" "1")
        (split-string (car-safe (split-string (buffer-string) "\n")) "|")))))

(defun file-info--get-first-commit-author ()
  "Return author of first commit."
  (when (buffer-file-name)
    (car (cdr (file-info--get-first-commit-info)))))

(defun file-info--get-first-commit-hash ()
  "Return hash of first commit."
  (when (buffer-file-name)
    (car (file-info--get-first-commit-info))))

(defun file-info--get-last-commit-hash ()
  "Return hash of last commit."
  (when (buffer-file-name)
    (car (cdr (file-info--get-last-commit-info)))))

(defun file-info--get-first-commit-date ()
  "Return date of first commit."
  (when (buffer-file-name)
    (car (cdr (cdr (file-info--get-first-commit-info))))))

(defun file-info--file-icon ()
  "Return icon for current file."
  (when (fboundp 'all-the-icons-icon-for-mode)
    (concat (all-the-icons-icon-for-mode major-mode :height 1.0 :v-adjust -0.05) " ")))

(defun file-info--get-current-branch ()
  "Return current branch via VC."
  (when (buffer-file-name)
    (vc-git--symbolic-ref (file-info--get-file-name))))

(defun file-info--get-git-file-changes ()
  "Get count of modified and removed lines of code from VC."
  (let ((file-name (file-info--get-file-name)))
    (when (buffer-file-name)
      (with-temp-buffer
        (vc-git-command t 0 file-name "diff" "--numstat")
        (let ((file-changes (split-string (buffer-string))))
          (when file-changes
            (concat (propertize (nth 0 file-changes) 'face 'font-lock-string-face) "/"
                    (propertize (nth 1 file-changes) 'face 'font-lock-keyword-face))))))))

(defun file-info--get-last-modified-date ()
  "Return last modified date."
  (when (buffer-file-name)
    (format-time-string "%Y-%m-%d %H:%M:%S" (nth 5 (file-attributes (buffer-file-name))))))

(defun file-info--get-remote-url ()
  "Return remote url via VC."
  (condition-case nil
      (when (buffer-file-name)
        (browse-at-remote-get-url))
    (error "Not found")))

(defun file-info--get-wakatime-spent-time-for-current-file ()
  "Return spent time for current file from wakatime binary."
  (when (and (executable-find "wakatime")
             (buffer-file-name))
    (with-temp-buffer
      (call-process "wakatime" nil t nil "--entity" (buffer-file-name) "--today")
      (let ((json-object-type 'hash-table)
            (json-key-type 'string)
            (json-array-type 'list)
            (json (json-read)))
        (when (and json (gethash "data" json))
          (let ((data (gethash "data" json)))
            (when (and data (gethash "grand_total" data))
              (let ((grand-total (gethash "grand_total" data)))
                (when (and grand-total (gethash "human_readable_total" grand-total))
                  (gethash "human_readable_total" grand-total))))))))))
  
  

(defun file-info--get-flycheck-errors-count ()
  "Return count of flycheck errors."
  (when (fboundp 'flycheck-count-errors)
    (let ((flycheck-info (flycheck-count-errors flycheck-current-errors)))
      (concat
       (propertize (number-to-string (or (cdr (assq 'error flycheck-info)) 0)) 'face 'flycheck-fringe-error)
       "/"
       (propertize (number-to-string (or (cdr (assq 'warning flycheck-info)) 0)) 'face 'flycheck-error-list-warning)))))

(defun file-info--get-project-name ()
  "Return project name."
  (cond ((fboundp 'projectile-project-name)
         (projectile-project-name))
        ((fboundp 'project-name)
         (project-name))
        t nil))

(defun file-info--get-all-file-contributors ()
  "Return list of all file contributers sorted by commits count via VC."
  (let ((file-name (file-info--get-file-name)))
    (when file-name
      (with-temp-buffer
        (vc-git-command t 0 file-name "log" "--pretty=%ae")
        (let ((committers (sort (mapcar (lambda (x) (cons (car x) (length x)))
                                        (seq-group-by 'identity (butlast (split-string (buffer-string) "\n"))))
                                (lambda (x y) (> (cdr x) (cdr y))))))
          (mapcar (lambda (x) (format "%s (%s)\n" (car x) (cdr x))) committers))))))



(defun file-info--slice-list-by-length (list length)
  "Slice LIST by LENGTH."
  (car (let ((result '()))
         (while list
           (push (seq-take list length) result)
           (setq list (seq-drop list length)))
         (reverse result))))

(defun file-info--get-project-related-path ()
  "Get project related path."
  (when (buffer-file-name)
    (let ((file-name (file-info--get-file-name)))
      (when file-name
        (concat "/" (replace-regexp-in-string " " "\\\\\  " (file-relative-name file-name (projectile-project-root))))))))

(defun file-info--get-headline (text)
  "Return headline for TEXT."
  (when file-info--include-headlines-p
    (let* ((max-length (+ file-info-max-value-length file-info-min-properties-length (if file-info-show-binding-p 4 0) 2))
           (left-right-length (/ (- max-length (length text)) 2)))
      (concat (make-string left-right-length file-info-headline-underline-symbol)
              " "
              text
              " "
              (make-string left-right-length file-info-headline-underline-symbol) "\n"))))


(defun file-info--separator ()
  "Return separator between groups of file info."
  "\n")

(defun file-info--get-file-name ()
  "Get file name."
  (if (buffer-file-name)
      (file-name-nondirectory (buffer-file-name))
    (buffer-name)))

(defun file-info--set-min-text-length (text)
  "Set minimal length of TEXT."
  (if (< (length text) file-info-min-properties-length)
      (concat text (make-string (- file-info-min-properties-length (length text)) ?\ ))
    text))

(defun file-info--split-text-by-max-length-with-new-line (text max-length)
  "Split TEXT by MAX-LENGTH with new line."
  (when text
    (if (< (length text) max-length)
        text
      (let ((result "")
            (current-length 0))
        (dolist (char (string-to-list text))
          (setq current-length (+ current-length 1))
          (if (> current-length max-length)
              (progn
                (setq result (concat result "\n" (make-string (+ file-info-min-properties-length (if file-info-show-binding-p 4 0)) ?\ )))
                (setq current-length 0))
            (setq result (concat result (char-to-string char)))))
        result))))

(defun file-info--align-list-of-items (items)
  "Align list of value ITEMS to the right."
  (concat (car items)
          (string-join (mapcar (lambda (x)
                                 (concat (make-string (+ file-info-min-properties-length (if file-info-show-binding-p 4 0)) ?\ ) x))
                               (cdr items)))))

(defun file-info--get-pretty-information ()
  "Get pretty information about file."
  (concat
   "\n\n"
   (string-join
    (mapcar
     (lambda (file-info-handler)
       (let* ((name (plist-get file-info-handler :name))
              (handler (plist-get file-info-handler :handler))
              (face (plist-get file-info-handler :face))
              (bind (plist-get file-info-handler :bind))
              (prefix (plist-get file-info-handler :prefix))
              (raw-handler-value (eval handler))
              (handler-value (cond ((listp raw-handler-value) (file-info--align-list-of-items raw-handler-value))
                                   (name (file-info--split-text-by-max-length-with-new-line (eval handler) file-info-max-value-length))
                                   (t raw-handler-value))))
         (when handler-value
           (if name
               (concat (when (and bind file-info-show-binding-p) (format "[%s] " (propertize bind 'face `(:foreground ,file-info-bind-color))))
                       (propertize (file-info--set-min-text-length (concat name ": ")) 'face `(:foreground ,file-info-properties-color))
                       (when prefix (eval prefix))
                       (if face (propertize handler-value 'face face) handler-value) "\n")
             handler-value))))
     file-info-handlers))
   "\n"))

(defun file-info--get-hydra-bindings ()
  "Get hydra bindings."
  (let ((binding-functions '()))
    (dolist (file-info-handler file-info-handlers)
      (when-let* ((bind (plist-get file-info-handler :bind))
                  (copy-raw-val (eval (plist-get file-info-handler :handler)))
                  (copy-val (if (listp copy-raw-val) (string-join copy-raw-val) copy-raw-val)))
        (push (list bind
                    (lambda () (interactive) (kill-new copy-val)))
              binding-functions)))
    binding-functions))


(defun file-info--get-active-lsp-servers ()
  "Get list of active lsp servers."
  (when (bound-and-true-p lsp-mode)
    (string-join (mapcar (lambda (server)
              (lsp--workspace-print server))
            (lsp-workspaces)) ", ")))

(defun file-info--show-hydra ()
  "Show info about file inside via hydra."
  (call-interactively
   (eval `(defhydra file-name--hydra-menu (:hint nil :exit t)
            ,(file-info--get-pretty-information)
            ,@(file-info--get-hydra-bindings)
            ("q" posframe-hide-all :color blue)))))

;;;###autoload
(defun file-info-show ()
  "Show information about current file."
  (file-info--show-hydra)
  (interactive))

(provide 'file-info)
;;; file-info.el ends here
