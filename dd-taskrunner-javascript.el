;;; dd-taskrunner-javascript.el --- Consult-based JS task runner -*- lexical-binding: t; -*-

;; Author: Diogo Doreto
;; Package-Requires: ((emacs "29.4") (consult "2.5"))
;; Keywords: javascript, task-runner, npm, bun, yarn, pnpm
;; URL: https://github.com/DiogoDoreto/dd-taskrunner-javascript.el

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides a consult-based task runner for JavaScript projects.
;; It automatically detects the package manager (npm, yarn, pnpm, bun) based on lockfiles,
;; and presents available scripts and common commands for quick execution.
;;
;; Features:
;; - Detects package manager from lockfile
;; - Lists scripts from package.json
;; - Provides customizable common commands (install, outdated, etc.)
;; - Integrates with consult for multi-source selection

;;; Code:

(require 'consult)
(require 'json)

(defcustom dd-taskrunner-javascript-lockfile-to-manager-alist
  '(("package-lock.json" . npm)
    ("bun.lock"          . bun)
    ("bun.lockb"         . bun)
    ("yarn.lock"         . yarn)
    ("pnpm-lock.yaml"    . pnpm))
  "Alist mapping lockfile file names to their respective package manager."
  :type  '(alist :key-type string :value-type symbol)
  :group 'dd-taskrunner-javascript)

(defcustom dd-taskrunner-javascript-default-manager 'npm
  "Default package manager to use if no lockfile is found."
  :type  'symbol
  :group 'dd-taskrunner-javascript)

(defcustom dd-taskrunner-javascript-commands
  '(("install"  . (:command "install"  :desc "Install packages"))
    ("outdated" . (:command "outdated" :desc "Outdated packages")))
  "Alist of default commands that will always be shown."
  :type  '(alist :key-type string :value-type plist)
  :group 'dd-taskrunner-javascript)

(defun dd-taskrunner-javascript--identify-package-manager (dir)
  "Identify the package manager according to the lockfile in DIR."
  (or (cdr (seq-find (lambda (lockfile-assoc)
                       (file-exists-p (expand-file-name (car lockfile-assoc) dir)))
                     dd-taskrunner-javascript-lockfile-to-manager-alist))
      dd-taskrunner-javascript-default-manager))

(defun dd-taskrunner-javascript--parse-package-json (file)
  (let ((json-object-type 'alist)
        (json-key-type 'string)
        (json-array-type 'list))
    (ignore-errors (json-read-file file))))

(defun dd-taskrunner-javascript--name-from-package-json (parsed-file)
  (alist-get "name" parsed-file nil nil #'string=))

(defun dd-taskrunner-javascript--scripts-from-package-json (parsed-file)
  (let* ((scripts (alist-get "scripts" parsed-file nil nil #'string=)))
    (when (and scripts (listp scripts))
      scripts)))

(defun dd-taskrunner-javascript--find-dominating-package-jsons ()
  "Return a list of up to 2 dominating package.json files, starting from the
current file's directory and moving up."
  (let* ((dir (if buffer-file-name
                  (file-name-directory buffer-file-name)
                default-directory))
         (home (expand-file-name "~/"))
         (last-dir (if (string-prefix-p home dir) home "/"))
         (result '()))
    (while (and dir
                (not (equal dir last-dir))
                (< (length result) 2))
      (let ((pkg (expand-file-name "package.json" dir)))
        (when (file-exists-p pkg)
          (push pkg result)))
      (setq dir (if (equal dir "/")
                    nil
                  (file-name-directory (directory-file-name dir)))))
    (let* ((topmost-file (car result))
           (pkgmgr (dd-taskrunner-javascript--identify-package-manager (file-name-directory topmost-file))))
      (mapcar (lambda (file)
                (let ((parsed-json (dd-taskrunner-javascript--parse-package-json file)))
                  (list :filepath file
                        :manager  pkgmgr
                        :root-p   (string= file topmost-file)
                        :name     (dd-taskrunner-javascript--name-from-package-json parsed-json)
                        :scripts  (dd-taskrunner-javascript--scripts-from-package-json parsed-json))))
              result))))

(defun dd-taskrunner-javascript--make-consult-source (package-plist)
  (let* ((scripts (mapcar (lambda (pair)
                            (cons (car pair) (list :script  (car pair)
                                                   :desc    (cdr pair)
                                                   :package package-plist)))
                          (plist-get package-plist :scripts))))
    (list :items    (append dd-taskrunner-javascript-commands scripts)
          :name     (concat (plist-get package-plist :name)
                            (when (plist-get package-plist :root-p)
                              " (root)"))
          :annotate (lambda (cand) (plist-get cand :desc))
          :action   (lambda (cand)
                      (let ((default-directory (file-name-directory (plist-get package-plist :filepath)))
                            (command (or (plist-get cand :command)
                                         (format "run %s" (plist-get cand :script)))))
                        (compile (format "%s %s"
                                         (plist-get package-plist :manager)
                                         command)))))))

(defun dd-taskrunner-javascript ()
  (interactive)
  (let* ((files (dd-taskrunner-javascript--find-dominating-package-jsons))
         (sources (mapcar #'dd-taskrunner-javascript--make-consult-source files)))
    (consult--multi sources
                    :prompt (format "Run with %s: " (plist-get (car files) :manager))
                    :require-match t)))

(provide 'dd-taskrunner-javascript)
;;; dd-taskrunner-javascript.el ends here
