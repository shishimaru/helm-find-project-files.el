;;; helm-find-project-files.el --- Extends helm-find to search files of the current working project.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Hitoshi Uchida <hitoshi.uchida@gmail.com>

;; Author: Hitoshi Uchida <hitoshi.uchida@gmail.com>
;; Version: 1.0
;; Package-Requires: ((helm "3.6.2") (emacs "24.4"))
;; URL: https://github.com/shishimaru/helm-find-project-files

;; This program is free software: you can redistribute it and/or modify
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
;;
;; helm-find-project-files.el is an Emacs package to extend helm-find to
;; recursively find files of the current working project identified by .git.

;;; Code:

(require 'helm)

(defvar helm-find-project-files-ignored-paths '("*/.git/*" "*.elc")
  "A list of file patterns to ignore with find command.")

(defvar helm-find-project-files-project-root-symbols '(".git" ".svn" ".projectroot")
  "Symbols to be used to identify the project root.")

(defun helm-find-project-files-project-root-p (dir symbols)
  "Return t if DIR is a project root directory identified with SYMBOLS."
  (if symbols
      (if (file-exists-p (format "%s/%s" dir (car symbols)))
          t
        (helm-find-project-files-project-root-p dir (cdr symbols)))))

(defun helm-find-project-files-find-git-dir (dir)
  "Find the project root directory by recursively searching up the directory tree form DIR."
  (cond
   ((not dir) "")
   ((string= dir "~") "")
   ((string= dir "/") "")
   ((helm-find-project-files-project-root-p dir helm-find-project-files-project-root-symbols) dir)
   (t (helm-find-project-files-find-git-dir (directory-file-name (file-name-directory dir))))))

(defun helm-find-project-files-construct-find-option (ignored)
  "Consruct options of find comamnd to exclude directories and files of IGNORED."
  (if ignored
      (format "-not -path '%s' %s"
              (car ignored)
              (helm-find-project-files-construct-find-option (cdr ignored)))
    ""))

(defun helm-find-project-files-normalize-dir (dir)
  "Normalize the specified DIR."
  (if (string= (substring dir 0 2) "~/")
      (setq dir (format "%s/%s" (getenv "HOME") (substring dir 2))))
  (if (not (string= (substring dir -1) "/"))
      (setq dir (format "%s/" dir)))
  dir)

(defun helm-find-project-files-find (dir)
  "Find project files containing DIR and select by helm inteface."
  (let* ((normalized-dir (helm-find-project-files-normalize-dir dir))
         (ignore-option
          (helm-find-project-files-construct-find-option helm-find-project-files-ignored-paths))
         (hidden-path-len (+ (length normalized-dir) 1))
         (shell-find-command
          (format "find %s %s | cut -c %d-" normalized-dir ignore-option hidden-path-len))
         (fpath-relative (helm :sources
                               (helm-build-sync-source "helm-find-project-files"
                                 :candidates (split-string
                                              (shell-command-to-string shell-find-command)
                                              "\n" t))))
         (fpath-absolute (format "%s%s" normalized-dir fpath-relative)))
    (if (file-exists-p fpath-absolute)
        (find-file fpath-absolute))))

(defun helm-find-project-files ()
  "Search files of the current working project with helm interface.

The root directory of the project is identified by searching up
from the location of the current buffer."
  (interactive)
  (let ((root-dir (helm-find-project-files-find-git-dir default-directory)))
    (if (string= root-dir "")
        (helm-find-project-files-find default-directory)
      (helm-find-project-files-find root-dir))))


(provide 'helm-find-project-files)

;;; helm-find-project-files.el ends here
