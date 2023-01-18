;;; file-info.el --- Show pretty information about current file                     -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/file-info.el
;; Package-Requires: ((emacs "24.4") (hydra "0.15.0"))
;; Version: 0.1

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

(require 'hydra)
(require 'subr-x)

(defcustom file-info-buffer-name "*pretty-file-info*"
  "Name of buffer for file info."
  :group 'file-info
  :type 'string)

(defcustom file-info-min-properties-length 20
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

(defconst file-info-handlers `(
                               (:name "File name" :prefix (file-info--file-icon) :handler (file-info--get-file-name) :face font-lock-string-face :bind "n")
                               (:name "File size" :handler (file-size-human-readable (buffer-size)) :face font-lock-string-face :bind "s")
                               (:name "File path" :handler (buffer-file-name) :face font-lock-string-face :bind "p")
                               (:name "File mode" :handler (format-mode-line mode-name) :face (:foreground ,file-info-properties-color) :bind "m")
                               (:handler (file-info--separator))
                               (:name "Line count" :handler (number-to-string (count-lines (point-min) (point-max))) :bind "l")
                               (:name "Words count" :handler (number-to-string (count-words (point-min) (point-max))) :bind "w")))

(defun file-info--file-icon ()
  "Return icon for current file."
  (when (fboundp 'all-the-icons-icon-for-mode)
    (concat (all-the-icons-icon-for-mode major-mode :height 1.0 :v-adjust -0.05) " ")))

(defun file-info--separator ()
  "Return separator between groups of file info."
  "\n")

(defun file-info--get-file-name ()
  "Get file name."
  (file-name-nondirectory (buffer-file-name)))

(defun file-info--set-min-text-length (text)
  "Set minimal length of TEXT."
  (if (< (length text) file-info-min-properties-length)
      (concat text (make-string (- file-info-min-properties-length (length text)) ?\ ))
    text))

(defun file-info--get-pretty-information ()
  "Get pretty information about file."
  (concat
   "\n\n"
   (string-join
    (mapcar (lambda (file-info-handler)
              (let ((name (plist-get file-info-handler :name))
                    (handler (plist-get file-info-handler :handler))
                    (face (plist-get file-info-handler :face))
                    (bind (plist-get file-info-handler :bind))
                    (prefix (plist-get file-info-handler :prefix)))
                (if name
                    (concat (when bind (format "[%s] " (propertize bind 'face `(:foreground ,file-info-bind-color))))
                            (propertize (file-info--set-min-text-length (concat name ": ")) 'face `(:foreground ,file-info-properties-color))
                            (when prefix (eval prefix))
                            (propertize (eval handler) 'face face) "\n")
                  (eval handler)
                  )))
            file-info-handlers))
   "\n"))

(defun file-name--get-hydra-bindings ()
  "Get hydra bindings."
  (let ((binding-functions '()))
    (dolist (file-info-handler file-info-handlers)
      (when (plist-get file-info-handler :bind)
        (push (list (plist-get file-info-handler :bind)
                    (lambda () (interactive) (kill-new (eval (plist-get file-info-handler :handler)))))
              binding-functions)))
    binding-functions))

(defun file-name--show-hydra ()
  "Show info about file inside via hydra."
  (call-interactively
   (eval `(defhydra file-name--hydra-menu (:color pink :hint nil :exit t)
            ,(file-info--get-pretty-information)
            ,@(file-name--get-hydra-bindings)
            ("q" posframe-hide-all :color blue)))))

;;;###autoload
(defun file-info-show ()
  "Show information about current file."
  (file-name--show-hydra)
  (interactive))

(provide 'file-info)
;;; file-info.el ends here
