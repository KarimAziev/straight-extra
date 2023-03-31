;;; straight-extra.el --- Extra utils for straight.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/straight-extra
;; Version: 0.1.0
;; Keywords: convenience abbrev
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extra utils for straight.el

;;; Code:


(declare-function find-library-suffixes "find-func")
(declare-function find-library-name "find-func")

(defvar straight-recipe-repositories)
(defvar find-library-source-path)
(defvar ivy-last)

(require 'straight)

(defvar straight-extra-melpa-packages-archive-hash
  (make-hash-table :test 'equal))

(defcustom straight-extra-save-use-package-file user-emacs-directory
  "Where to save packages."
  :group 'straight-extra-save-use-package-file
  :type '(radio :tag "Where to save packages."
                (file :tag "File")
                (directory :tag "Directory")
                (function :tag "Custom function")))

(defcustom straight-extra-melpa-cache-filename (expand-file-name
                                                "var/.staight-extra-archives.cache"
                                                user-emacs-directory)
  "Where to save melpa archives."
  :type 'file
  :group 'straight-extra)

(defcustom straight-extra-use-package-symbol-names '("use-package"
                                                     "use-package!"
                                                     "straight-use-package")
  "Symbol names to search installed packages."
  :group 'straight-extra
  :type '(repeat string))

(defvar straight-extra--docstring-positions
  (mapcar (lambda (it)
            (setcar it (intern (car it)))
            it)
          '(("defun" . 3)
            ("defmacro" . 3)
            ("defsubst" . 4)
            ("defcustom" . 3)
            ("define-skeleton" . 2)
            ("define-compilation-mode" . 3)
            ("define-minor-mode" . 2)
            ("define-derived-mode" . 4)
            ("define-generic-mode" . 8)
            ("ert-deftest" . 3)
            ("cl-defun" . 3)
            ("cl-defsubst" . 3)
            ("cl-defmacro" . 3)
            ("cl-defgeneric" . 3)
            ("cl-defmethod" . 6)
            ("defalias" . 4)
            ("defhydra" . 4)
            ("defgroup" . 3)
            ("deftheme" . 3)
            ("define-widget" . 3)
            ("transient-define-suffix" . 3)
            ("transient-define-argument" . 3)
            ("transient-define-prefix" . 3)
            ("defvar" . 4)
            ("defvar-local" . 4)
            ("cl-defstruct" . 3)
            ("easy-mmode-define-minor-mode" . 2)
            ("transient-define-infix" . 3)
            ("defface" . 3))))

(defun straight-extra-symbol-sexp-keymapp (sexp)
  "Return t if SEXP look like keymap variable."
  (when-let* ((value (nth 2 sexp))
              (vals (and (listp value)
                         (symbolp (car value))
                         (memq (car value) '(let let*))
                         (car (seq-drop value 1)))))
    (when (and (listp vals)
               (listp (car vals)))
      (seq-find (lambda (it)
                  (when-let ((val (and (listp (cdr it))
                                       (listp (cadr it))
                                       (cadr it))))
                    (and
                     (= 1 (length val))
                     (symbolp (car val))
                     (memq (car val) '(make-sparse-keymap)))))
                vals))))

(defun straight-extra-sexp-at-point ()
  "Return the elisp sexp at point, or nil if none is found."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (sexp-at-point)))

(defun straight-extra-list-at-point ()
  "Return the list at point, or nil if none is found."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (list-at-point)))

(defun straight-extra-symbol-keymapp (sym)
  "Return t if value of symbol SYM is a keymap."
  (when-let ((val (when (boundp sym)
                    (symbol-value sym))))
    (keymapp val)))

(defun straight-extra-function-p (symb)
  "Return t is SYMB can have arguments.
SYMB can be either symbol, either string."
  (member (if (symbolp symb)
              (symbol-name symb)
            symb)
          '("defun"
            "defmacro"
            "defun*"
            "defsubst"
            "cl-defun"
            "define-inline"
            "cl-defgeneric"
            "cl-defmethod"
            "define-advice")))

(defun straight-extra--unquote (exp)
  "Return EXP unquoted."
  (declare (pure t)
           (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun straight-extra-read-library-name--find-files (dirs suffixes)
  "Return a list of all files in DIRS that match SUFFIXES."
  (let ((files nil)
        (regexp (concat (regexp-opt suffixes) "\\'")))
    (dolist (dir dirs)
      (dolist (file (ignore-errors (directory-files dir nil regexp t)))
        (and (string-match regexp file)
             (push (cons (substring file 0 (match-beginning 0)) dir) files))))
    files))

(defun straight-extra-elisp-builtins-features ()
  "Return alist of builtin library names and parent directories."
  (require 'find-func)
  (let* ((dirs (or find-library-source-path
                   (seq-remove
                    (lambda (it)
                      (file-in-directory-p it
                                           user-emacs-directory))
                    load-path)))
         (suffixes (find-library-suffixes)))
    (straight-extra-read-library-name--find-files dirs suffixes)))

(defun straight-extra-key-description (key)
  "Return key description for KEY without errors."
  (or
   (ignore-errors (key-description
                   (kbd key)))
   (ignore-errors (key-description key))
   (ignore-errors (key-description (char-to-string
                                    key)))
   (ignore-errors
     (key-description (vector key)))))


(defun straight-extra--keymap-keys (keymap)
  "Return all the keys and commands in KEYMAP.
Flattens nested keymaps and follows remapped commands.

Returns a list of pairs (KEYCODES COMMAND), where KEYCODES is a
vector suitable for `key-description', and COMMAND is a smbol."
  (when keymap
    (cond ((and
            (symbolp keymap)
            (fboundp keymap)
            (keymapp (symbol-function keymap)))
           (straight-extra--keymap-keys (symbol-function keymap)))
          ((or
            (symbolp keymap)
            (functionp keymap)
            (stringp keymap)
            (vectorp keymap))
           `(([] ,keymap)))
          ((stringp (car-safe keymap))
           (straight-extra-keymap-keys-to-alist (cdr-safe keymap)))
          ((listp (cdr-safe keymap))
           (let (result)
             (dolist (item (cdr keymap))
               (cond ((and (consp item)
                           (eq (car-safe item) 'menu-bar))
                      nil)
                     ((consp item)
                      (pcase-let (()))
                      (let* ((source-0-- item)
                             (keycode
                              (car-safe
                               (prog1 source-0--
                                 (setq source-0--
                                       (cdr source-0--)))))
                             (value source-0--))
                        (mapc
                         #'(lambda
                             (input0)
                             (let* ((source-1-- input0)
                                    (keycodes
                                     (car-safe
                                      (prog1 source-1--
                                        (setq source-1--
                                              (cdr source-1--)))))
                                    (command
                                     (car source-1--)))
                               (setq result
                                     (cons
                                      (list
                                       (vconcat
                                        (vector keycode)
                                        keycodes)
                                       command)
                                      result))))
                         (straight-extra--keymap-keys value))))
                     ((char-table-p item)
                      (map-char-table
                       (lambda (keycode value)
                         (mapc
                          #'(lambda
                              (input0)
                              (let* ((source-14-- input0)
                                     (keycodes
                                      (car-safe
                                       (prog1 source-14--
                                         (setq source-14--
                                               (cdr source-14--)))))
                                     (command
                                      (car source-14--)))
                                (setq result
                                      (cons
                                       (list
                                        (vconcat
                                         (vector keycode)
                                         keycodes)
                                        command)
                                       result))))
                          (straight-extra--keymap-keys value)))
                       item))))
             (setq result
                   (let (res)
                     (let ((list result)
                           (i 0))
                       (while list
                         (let ((it
                                (car-safe
                                 (prog1 list
                                   (setq list
                                         (cdr list)))))
                               (it-index i))
                           (ignore it it-index)
                           (setq res
                                 (cons
                                  (if
                                      (let ((input0 it))
                                        (let* ((source-20-- input0)
                                               (keycodes
                                                (car-safe
                                                 (prog1 source-20--
                                                   (setq source-20--
                                                         (cdr
                                                          source-20--)))))
                                               (_
                                                (car source-20--)))
                                          (and
                                           (>
                                            (length keycodes)
                                            1)
                                           (eq
                                            (elt keycodes 0)
                                            'remap))))
                                      (let ((input0 it))
                                        (let* ((source-19-- input0)
                                               (keycodes
                                                (car-safe
                                                 (prog1 source-19--
                                                   (setq source-19--
                                                         (cdr
                                                          source-19--)))))
                                               (command
                                                (car source-19--)))
                                          (list
                                           (where-is-internal
                                            (elt keycodes 1)
                                            global-map t)
                                           command)))
                                    it)
                                  res)))
                         (setq i
                               (1+ i))))
                     (nreverse res)))
             (nreverse result))))))


(defun straight-extra-keymap-keys-to-alist (keymap &optional filter)
  "Map KEYMAP to alist with FILTER function.
FILTER function will be called with two arguments - key description and value."
  (let ((exclude-cmds '(digit-argument negative-argument
                                       self-insert-command
                                       undefined)))
    (delq nil
          (mapcar (lambda (it)
                    (when (listp it)
                      (when-let ((key-descr
                                  (straight-extra-key-description (car
                                                                   it)))
                                 (value (if (listp (cdr it))
                                            (cadr it)
                                          (cdr it))))
                        (unless (or
                                 (string-empty-p key-descr)
                                 (memq (cadr it) exclude-cmds)
                                 (string-match-p
                                  "^<\\|keymap\\|follow-link\\|compose-last-chars\\|drag-n-drop\\|menu\\|XF86Forward\\|XF86Back\\|help\\|iconify-frame\\|touch\\|mouse\\|wheel"
                                  key-descr)
                                 (when filter
                                   (not (funcall filter key-descr value))))
                          (cons key-descr value)))))
                  (reverse (straight-extra--keymap-keys (keymap-canonicalize
                                                         keymap)))))))

(defun straight-extra-format-keymap-to-alist (keymap &optional symb-prefix)
  "Convert KEYMAP to alist and filter by SYMB-PREFIX."
  (when (keymapp keymap)
    (if-let ((name (when symb-prefix
                     (car
                      (split-string (if (symbolp symb-prefix)
                                        (symbol-name symb-prefix)
                                      symb-prefix)
                                    "-" t)))))
        (straight-extra-keymap-keys-to-alist keymap
                                             (lambda (_k v)
                                               (and (symbolp v)
                                                    (string-prefix-p name
                                                                     (symbol-name
                                                                      v)))))
      (straight-extra-keymap-keys-to-alist keymap))))

(defmacro straight-extra-autodoc-with-temp-lisp-buffer (&rest body)
  "Execute BODY in temp buffer with Emacs Lisp mode without hooks."
  (declare (indent 2)
           (debug t))
  `(with-temp-buffer
     (erase-buffer)
     (let (emacs-lisp-mode-hook)
       (emacs-lisp-mode))
     (progn
       ,@body)))

(defun straight-extra--parse-list-at-point ()
  "Parse list at point and return alist of form (symbol-name args doc deftype).
E.g. (\"straight-extra--parse-list-at-point\" (arg) \"Doc string\" defun)"
  (when-let* ((sexp (unless (nth 4 (syntax-ppss (point)))
                      (list-at-point)))
              (type (car sexp))
              (id (straight-extra--unquote (when (symbolp (nth 1 sexp))
                                             (nth 1 sexp))))
              (name (symbol-name id)))
    (let ((doc (when-let ((pos (cdr
                                (assq type
                                      straight-extra--docstring-positions))))
                 (nth pos sexp)))
          (args (and (straight-extra-function-p type)
                     (nth 2 sexp))))
      (list name args doc
            (cond ((and (straight-extra-function-p type)
                        (or (and
                             (nth 3 sexp)
                             (listp (nth 3 sexp))
                             (symbolp (car (nth 3 sexp)))
                             (eq 'interactive (car (nth 3 sexp))))
                            (and
                             (nth 4 sexp)
                             (listp (nth 4 sexp))
                             (symbolp (car (nth 4 sexp)))
                             (eq 'interactive (car (nth 4 sexp))))))
                   'interactive)
                  ((or (straight-extra-symbol-keymapp id)
                       (straight-extra-symbol-sexp-keymapp sexp))
                   'keymap)
                  (t type))))))

(defun straight-extra-scan-get-buffer-maps ()
  "Return keymaps in current buffer."
  (when-let ((maps (plist-get (straight-extra-scan-buffer) :keymap)))
    (delq nil (mapcar (lambda (it)
                        (when-let* ((sym (intern (car it)))
                                    (val (symbol-value sym)))
                          (when (> (length val) 0)
                            (cons sym val))))
                      maps))))

(defun straight-extra-scan-buffer ()
  "Return plist of top level Lisp definitions.

Each key is definition type, converted to keyword (:defmacro, :defun etc),
except interactive functions, which holds under keyword :interactive.

The value of plist is a list of sublists of form (symbol-name args doc deftype).

See function `straight-extra--parse-list-at-point'."
  (save-excursion
    (let ((pl '()))
      (goto-char (point-max))
      (while (straight-extra-backward-list)
        (when-let ((sexp (straight-extra--parse-list-at-point)))
          (let ((keyword (intern (concat ":" (symbol-name (car
                                                           (reverse sexp)))))))
            (if-let ((group (plist-get pl keyword)))
                (setq pl (plist-put pl keyword (append group (list sexp))))
              (setq pl (plist-put pl keyword (list sexp)))))))
      pl)))

(defun straight-extra-read-builtin-lib ()
  "Read bultin library."
  (let* ((alist (reverse (straight-extra-elisp-builtins-features)))
         (annotf (lambda (str)
                   (format " (%s)" (cdr (assoc str alist)))))
         (cycle-sort-fn (lambda (it) it))
         (display-sort-fn (lambda (it)
                            (seq-sort-by #'length '> it))))
    (completing-read "Candidates: "
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function . ,annotf)
                             (cycle-sort-function . ,cycle-sort-fn)
                             (display-sort-function . ,display-sort-fn))
                         (complete-with-action action alist str pred))))))

(defun straight-extra-eval-string (str)
  "Read and evaluate all forms in STR.
Return the results of all forms as a list."
  (let ((next 0)
        ret)
    (condition-case nil
        (while t
          (setq ret (cons (funcall (lambda (ret)
                                     (setq next (cdr ret))
                                     (eval (car ret) t))
                                   (read-from-string str next))
                          ret)))
      (end-of-file))
    (nreverse ret)))



(defun straight-extra-generate-use-package-string (reponame &optional user
                                                            commands maps)
  "Generate string straght and use package installation from USER and REPONAME.
With COMMANDS also insert :commands.
With MAPS also insert :bind."
  (straight-extra-autodoc-with-temp-lisp-buffer
      (indent-tabs-mode -1)
      (insert
       "(use-package " reponame ")")
    (forward-char -1)
    (setq maps (delq nil
                     (mapcar
                      (lambda (it)
                        (when-let ((v
                                    (straight-extra-format-keymap-to-alist
                                     (cdr
                                      it)
                                     reponame)))
                          (cons
                           (car it)
                           v)))
                      maps)))
    (when (and user reponame)
      (newline-and-indent)
      (insert ":straight (" reponame ")")
      (forward-char -1)
      (newline-and-indent)
      (insert (format ":repo \"%s/%s\"" user reponame))
      (newline-and-indent)
      (insert ":type git")
      (newline-and-indent)
      (insert ":host github")
      (forward-char 1))
    (let ((key-commands))
      (when maps
        (newline-and-indent)
        (insert ":bind ()")
        (forward-char -1)
        (dotimes (i (length maps))
          (let ((it (nth i maps)))
            (let ((name (symbol-name (car it)))
                  (alist (cdr it))
                  (el))
              (when (> i 0)
                (newline-and-indent))
              (insert "(:map " name ")")
              (forward-char -1)
              (while (setq el (pop alist))
                (newline-and-indent)
                (insert  (if (string-match-p "<remap>" (car el))
                             (concat "([remap " (car (last (split-string
                                                            (car el)
                                                            "[\"\s\t<>]"
                                                            t)))
                                     "] . "
                                     (prin1-to-string (cdr el))")")
                           (prin1-to-string el)))
                (push (symbol-name (cdr el)) key-commands))
              (forward-char 1))))
        (forward-char 1))
      (setq commands (seq-difference commands key-commands))
      (when commands
        (newline-and-indent)
        (insert ":commands ()")
        (forward-char -1)
        (dotimes (i (length commands))
          (let ((cell (nth i commands)))
            (when (> i 0)
              (newline-and-indent))
            (insert cell)))
        (forward-char 1))
      (forward-sexp -1)
      (buffer-substring-no-properties (point-min)
                                      (point-max)))))

;;;###autoload
(defun straight-extra-insert-builtin-lib (lib)
  "Insert `use-package' with LIB as builtin lib."
  (interactive (list (straight-extra-read-builtin-lib)))
  (insert (format
           "(use-package %s\n\s\s:straight (:type built-in))"
           lib)))

(defun straight-extra-do-insert-use-package (package-name)
  "Insert `use-package' skeleton for PACKAGE-NAME."
  (let ((str (if (stringp package-name)
                 (concat "(use-package " package-name")")
               (prin1-to-string
                `(use-package ,package-name)))))
    (insert str)
    (forward-char -1)
    (let ((res (eval (read str) lexical-binding)))
      res)))

;;;###autoload
(defun straight-extra-configure-package ()
  "Insert `use-package' skeleton for PACKAGE-NAME."
  (interactive)
  (straight-extra-inside-use-package-p))

(defun straight-extra-locate-readme-for-current (package)
  "Return readme for PACKAGE.
PACKAGE should be either string or symbol."
  (car (straight-extra-locate-doc-files package)))

(defun straight-extra-locate-doc-files (package)
  "Return documentation files for PACKAGE.
PACKAGE should be either string or symbol."
  (when-let* ((libfile (ignore-errors (find-library-name
                                       (if (symbolp package)
                                           (symbol-name package)
                                         package))))
              (dir (let ((default-directory
                          (file-name-directory (file-truename
                                                libfile))))
                     (or (vc-root-dir) default-directory)))
              (files (let ((case-fold-search t))
                       (seq-sort-by
                        (lambda (it)
                          (let ((name (file-name-base it))
                                (ext (file-name-extension it)))
                            (pcase ext
                              ((and "org"
                                    (guard (string-match-p "manual" name)))
                               5)
                              ((and "org"
                                    (guard (string-match-p "guide" name)))
                               4)
                              ((and "org"
                                    (guard (string-match-p "readme" name)))
                               3)
                              ((and
                                "md"
                                (guard (string-match-p "readme" name)))
                               2)
                              ((guard (string-match-p "readme" name))
                               1)
                              (_ -1))))
                        '>
                        (directory-files-recursively
                         dir
                         "readme\\|manual\\|guide")))))
    (or (seq-filter (apply-partially 'string-match-p "\\(org\\|md\\)$")
                    files)
        files)))

(defun straight-extra-parse-readme (readme)
  "Extra `use-package' examples form README."
  (when (and readme (file-exists-p readme))
    (with-current-buffer (find-file-noselect readme)
      (goto-char (point-max))
      (let ((founds))
        (while (re-search-backward "[(]use-package " nil
                                   t 1)
          (when-let* ((bounds
                       (with-syntax-table
                           emacs-lisp-mode-syntax-table
                         (bounds-of-thing-at-point 'list)))
                      (valid-str
                       (buffer-substring-no-properties
                        (car
                         bounds)
                        (cdr
                         bounds))))
            (when (read valid-str)
              (push valid-str
                    founds))))
        (string-join (delq nil founds) "\n\n")))))


(defun straight-extra-find-file-readme (package)
  "Find file in other window for PACKAGE."
  (when-let ((files (straight-extra-locate-doc-files
                     package))
             (file (car files)))
    (straight-extra-file-visit file)
    (unless (= (length files) 1)
      (straight-extra-cycle-files files file))))

(defun straight-extra-file-visit (file &optional other-wind)
  "Select FILE window, if any or find FILE in OTHER-WIND if non nil."
  (cond ((and (buffer-live-p (get-file-buffer file))
              (get-buffer-window (get-file-buffer file)))
         (select-window (get-buffer-window file)))
        ((not (equal file buffer-file-name))
         (if other-wind
             (find-file file)
           (find-file-other-window file)))))

(defun straight-extra-index-switcher (step current-index switch-list)
  "Increase or decrease CURRENT-INDEX depending on STEP value and SWITCH-LIST."
  (cond ((> step 0)
         (if (>= (+ step current-index)
                 (length switch-list))
             0
           (+ step current-index)))
        ((< step 0)
         (if (or (<= 0 (+ step current-index)))
             (+ step current-index)
           (1- (length switch-list))))))

(defun straight-extra-cycle-items (action items &optional init-idx)
  "Cycle ITEMS with ACTION starting from INIT-IDX."
  (let ((idx (or init-idx 0)))
    (unwind-protect
        (progn
          (while
              (let ((init-head header-line-format))
                (setq header-line-format "n - next doc, p - prev doc")
                (let ((key (read-key-sequence
                            "n - next doc, p - prev doc")))
                  (setq header-line-format init-head)
                  (pcase key
                    ("n"
                     (setq idx
                           (straight-extra-index-switcher 1 idx
                                                          items))
                     t)
                    ("p" (setq idx
                               (straight-extra-index-switcher -1 idx
                                                              items))
                     t))))
            (funcall action
                     (nth
                      idx
                      items))))
      (setq unread-command-events
            (append (this-single-command-raw-keys)
                    unread-command-events)))
    (when idx
      (nth idx items))))

(defun straight-extra-cycle-files (files &optional file)
  "Cycle FILES with n and p keys, starting from FILE."
  (let ((file (or file (car files))))
    (when (> (length files) 1)
      (when (active-minibuffer-window)
        (minibuffer-message "n - next doc, p - prev doc"))
      (straight-extra-cycle-items 'find-file files (seq-position files file)))))



(defun straight-extra-find-readme-other-window-for-current (package)
  "Find file in other window for PACKAGE."
  (when-let ((files (straight-extra-locate-doc-files
                     package))
             (file (car files)))
    (find-file-other-window file)
    (goto-char (point-min))
    (unless (= (length files) 1)
      (straight-extra-cycle-files files file))))


;;;###autoload
(defun straight-extra-insert-use-package (&optional library)
  "Insert `use-package' skeleton for LIBRARY."
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (when-let* ((lib (or library (read-library-name)))
                (str
                 (progn (require (intern (if (symbolp lib)
                                             (intern lib)
                                           lib)))
                        (straight-extra-generate-use-package-string
                         lib nil
                         nil
                         (let (maps)
                           (mapatoms (lambda (sym)
                                       (and (boundp sym)
                                            (string-prefix-p (format "%s" lib)
                                                             (symbol-name
                                                              sym))
                                            (keymapp (symbol-value sym))
                                            (push (cons sym (symbol-value sym))
                                                  maps))))
                           maps)))))
      (with-current-buffer orig-buffer
        (insert str)))))

;;;###autoload
(defun straight-extra-insert-use-package-at-point ()
  "Eval and insert use package form with package."
  (interactive)
  (let* ((actions (remove nil
                          `((?e "existing")
                            ,(when (and (require 'gh-repo nil t)
                                        (fboundp 'gh-repo-read-user-repo))
                               '(?m "my repo"))
                            (?c "clone new")
                            (?b "built-in lib")
                            (?o "other"))))
         (answer (read-multiple-choice "Action: "
                                       actions)))
    (pcase (car answer)
      (?e
       (straight-extra-install-package (straight-extra-read-package)))
      (?c
       (let ((lib (funcall-interactively #'straight-extra-read-package)))
         (straight-extra-insert-use-package lib)
         (straight-extra-eval-string (format "(use-package %s :demand t)" lib))))
      (?b (insert (format
                   "(use-package %s\n\s\s:straight (:type built-in))"
                   (straight-extra-read-builtin-lib))))
      (?m
       (when (fboundp 'gh-repo-read-user-repo)
         (let ((lib (straight-extra-s-strip-props (gh-repo-read-user-repo
                                                   'identity)))
               (name)
               (user)
               (file)
               (str))
           (setq name (car (last (split-string lib "/"))))
           (setq user (car (split-string lib "/")))
           (straight-extra-eval-string
            (concat "(progn "(straight-extra-generate-use-package-string
                              name user)
                    ")"))
           (setq file (find-library-name name))
           (when file
             (setq str (with-current-buffer
                           (find-file-noselect file)
                         (eval-buffer)
                         (straight-extra-generate-use-package-string
                          name user (mapcar #'car (plist-get
                                                   (straight-extra-scan-buffer)
                                                   :interactive))
                          (straight-extra-scan-get-buffer-maps))))
             (insert str)))))
      (?o
       (insert "(use-package " (read-string "Package:\s")" )")))))


;;;###autoload
(defun straight-extra-install-package (package &optional outfile insert-pos)
  "Install PACKAGE and write it to OUTFILE at INSERT-POS."
  (interactive (list (straight-extra-read-package)))
  (let ((symb (intern package)))
    (condition-case nil
        (straight-use-package symb)
      (error
       (when (fboundp 'straight-pull-recipe-repositories)
         (straight-pull-recipe-repositories)
         (straight-use-package symb))))
    (require symb nil t))
  (when-let ((dest
              (cond ((eq outfile 'no-write)
                     nil)
                    ((and outfile)
                     outfile)
                    ((functionp
                      straight-extra-save-use-package-file)
                     (funcall
                      straight-extra-save-use-package-file
                      package))
                    ((file-directory-p
                      straight-extra-save-use-package-file)
                     (read-file-name (format "Save %s to: "
                                             package)))
                    ((file-name-absolute-p
                      straight-extra-save-use-package-file)
                     straight-extra-save-use-package-file))))
    (unless (file-exists-p dest)
      (unless (file-exists-p (file-name-directory dest))
        (make-directory (file-name-directory
                         dest)
                        'parents))
      (let ((non-dir (file-name-nondirectory dest)))
        (write-region
         (string-join (list (format
                             ";;; %s --- Configure %s lexical-binding: t; -*-"
                             non-dir package)
                            ";;; Commentary:"
                            ";;; Code:"
                            "(eval-when-compile (require 'use-package))"
                            (format "(provide '%s)\n;;; %s ends here"
                                    (file-name-base
                                     dest)
                                    non-dir))
                      "\n\n")
         nil
         dest)))
    (with-current-buffer (if (equal buffer-file-name dest)
                             (current-buffer)
                           (find-file-noselect dest))
      (if insert-pos
          (goto-char insert-pos)
        (goto-char (point-max))
        (straight-extra-backward-list))
      (require 'prettier-elisp nil t)
      (insert "(use-package " package ")")
      (forward-char -1)
      (when (fboundp 'prettier-elisp)
        (prettier-elisp))
      (unless (get-buffer-window (current-buffer))
        (pop-to-buffer (current-buffer))))
    (straight-extra-find-readme-other-window-for-current package)))

(defun straight-extra-serialize (data filename)
  "Serialize DATA to FILENAME.

The saved data can be restored with `straight-extra-unserialize'."
  (unless (file-exists-p
           (file-name-directory
            filename))
    (make-directory (file-name-directory
                     filename)
                    'parents))
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length)
                (prin1-to-string data))))))

(defun straight-extra-unserialize (filename)
  "Read data serialized by `straight-extra-serialize' from FILENAME."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (read (buffer-string))))))

(defun straight-extra-re-search-forward-inner (regexp &optional bound count)
  "This function is helper for `straight-extra-re-search-forward'.
Search forward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              (t
               (setq count (1- count)))))))
  (point))

(defun straight-extra-re-search-backward-inner (regexp &optional bound count)
  "This function is helper for `straight-extra-re-search-backward'.
Search backward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-backward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (or (nth 3 parse))
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              (t
               (setq count (1- count)))))))
  (point))

(defun straight-extra-re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0)
                (setq count (- count))
                #'straight-extra-re-search-backward-inner)
               ((> count 0) #'straight-extra-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err)
                 (cdr err)))))))

(defun straight-extra-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (unless n (setq n 1))
    (when-let ((str-start (nth 8 (syntax-ppss (point)))))
      (goto-char str-start))
    (let ((init-pos (point))
          (pos)
          (count n))
      (while (and (not (= count 0))
                  (when-let ((end (ignore-errors
                                    (funcall fn)
                                    (point))))
                    (unless (= end (or pos init-pos))
                      (setq pos end))))
        (setq count (1- count)))
      (if (= count 0)
          pos
        (goto-char init-pos)
        nil))))

(defun straight-extra-backward-list (&optional arg)
  "Move backward up across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (straight-extra-move-with 'backward-list arg))

(defun straight-extra-backward-up-list (&optional arg)
  "Move backward up across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (straight-extra-move-with 'backward-up-list arg))

(defun straight-extra-re-search-backward (regexp &optional bound noerror count)
  "Search backward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (straight-extra-re-search-forward regexp bound noerror (if count
                                                             (- count) -1)))

(defun straight-extra-annotate-melpa-description (package)
  "Return annotation for PACKAGE."
  (let* ((item (gethash
                (if (stringp package)
                    (intern package)
                  package)
                straight-extra-melpa-packages-archive-hash))
         (props (alist-get 'props item))
         (parts (list
                 (when-let ((year (car (mapcar #'number-to-string
                                               (append
                                                (alist-get 'ver item)
                                                nil)))))
                   (substring year 0 4))
                 (alist-get 'desc item)
                 (alist-get 'keywords props)
                 (alist-get 'authors props))))
    (setq parts
          (delete nil
                  (seq-map-indexed
                   (lambda (el i)
                     (when el
                       (propertize
                        (format "%s" el) 'face
                        (nth i
                             '(font-lock-keyword-face
                               font-lock-builtin-face
                               font-lock-warning-face
                               font-lock-comment-face)))))
                   parts)))
    (concat "\s" (string-join parts "\s"))))


(defvar json-false)
(defun straight-extra-fetch-json (url)
  "Fetch json data at URL."
  (require 'json)
  (with-temp-buffer
    (url-insert-file-contents url)
    (let ((json-false :false))
      (when (fboundp 'json-read)
        (json-read)))))

(defun straight-extra-ensure-melpa-archive (&optional force)
  "Fetch melpa archives. With FORCE inhibit cache."
  (cond ((and (not force)
              (hash-table-p straight-extra-melpa-packages-archive-hash)
              (not (hash-table-empty-p
                    straight-extra-melpa-packages-archive-hash)))
         straight-extra-melpa-packages-archive-hash)
        ((when (and (not force)
                    straight-extra-melpa-cache-filename
                    (file-exists-p straight-extra-melpa-cache-filename))
           (setq straight-extra-melpa-packages-archive-hash
                 (ignore-errors (straight-extra-unserialize
                                 straight-extra-melpa-cache-filename))))
         straight-extra-melpa-packages-archive-hash)
        (t
         (message "loading archives")
         (when-let ((data (straight-extra-fetch-json
                           "https://melpa.org/archive.json")))
           
           (setq straight-extra-melpa-packages-archive-hash (make-hash-table
                                                               :test 'equal))
           (dolist (cell data)
             (puthash (car cell)
                      (cdr cell)
                      straight-extra-melpa-packages-archive-hash))
           (when straight-extra-melpa-cache-filename
             (straight-extra-serialize
              straight-extra-melpa-packages-archive-hash
              straight-extra-melpa-cache-filename))
           straight-extra-melpa-packages-archive-hash))))

;;;###autoload
(defun straight-extra-browse-in-other-window ()
  "Abort minibuffers and browse current package."
  (interactive)
  (when-let ((current
              (straight-extra-get-current-candidate)))
    (run-with-timer 0 nil
                    'straight-extra-browse-action
                    (car
                     (split-string
                      (straight-extra-s-strip-props
                       current)
                      nil
                      t)))
    (abort-minibuffers)))

;;;###autoload
(defun straight-extra-browse-preview ()
  "Browse current package without exiting minibuffer."
  (interactive)
  (when-let ((current
              (straight-extra-get-current-candidate)))
    (with-minibuffer-selected-window
      (straight-extra-browse-action
       (car
        (split-string (straight-extra-s-strip-props
                       current)
                      nil t))))))

(defvar straight-extra-minibuffer-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map
                (kbd "C-c C-o")
                #'straight-extra-browse-in-other-window)
    (define-key map
                (kbd "C-j")
                #'straight-extra-browse-preview)
    map))

;;;###autoload
(defun straight-extra-completing-read-annotated (packages &optional predicate)
  "Read PACKAGES with annotated completions and PREDICATE."
  (interactive)
  (let ((items (mapcar (lambda (key)
                         (concat key (straight-extra--annotate key)))
                       (seq-sort-by
                        (lambda (key)
                          (or
                           (car
                            (append
                             (alist-get
                              'ver
                              (gethash
                               (if (stringp key)
                                   (intern key)
                                 key)
                               straight-extra-melpa-packages-archive-hash))
                             nil))
                           -1))
                        #'>
                        (seq-copy packages)))))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (minibufferp)
            (use-local-map
             (let ((map (copy-keymap straight-extra-minibuffer-keymap)))
               (set-keymap-parent map (current-local-map))
               map))))
      (car (split-string (completing-read "Which recipe? "
                                          items
                                          predicate
                                          'require-match)
                         nil t)))))

;;;###autoload
(defun straight-extra-browse-package (&optional arg)
  "Interectively browse a package.
With prefix argument ARG force to refetch archives."
  (interactive "P")
  (straight-extra-ensure-melpa-archive arg)
  (straight-extra-browse-action
   (funcall-interactively #'straight-extra-completing-read-annotated
                          (straight-recipes-list))))

(defun straight-extra-read-package (&optional arg)
  "Read a package with annotated completions.
With prefix argument ARG force to refetch archives."
  (straight-extra-ensure-melpa-archive arg)
  (straight-extra-completing-read-annotated
   (straight-recipes-list)))



(defun straight-extra-get-url (package-name &optional recipe)
  "Convert PACKAGE-NAME with RECIPE to url."
  (setq package-name (if (stringp package-name)
                         (intern (car (split-string package-name)))
                       package-name))
  (straight--with-plist (or recipe (straight--convert-recipe package-name))
      (host repo)
    (when (eq host 'sourcehut)
      (setq repo (concat "~" repo)))
    (let ((url (if-let ((domain (car (alist-get host
                                                straight-hosts))))
                   (format "https://%s/%s" domain repo)
                 (when repo (format "%s" repo)))))
      url)))


(defun straight-extra-buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of BUFFER-LIST with `major-mode' listed in MODES.
MODES can be either list of modes, or a mode.

If DERIVED-P is non-nil, test with `derived-mode-p', otherwise use `eq'."
  (unless (proper-list-p modes)
    (setq modes (list modes)))
  (seq-filter (if derived-p
                  (lambda (buf)
                    (apply #'provided-mode-derived-p
                           (buffer-local-value 'major-mode buf)
                           modes))
                (lambda (buf)
                  (memq (buffer-local-value 'major-mode buf) modes)))
              (or buffer-list (buffer-list))))

(defun straight-extra-browse-xwidget-in-other-window (&optional fn &rest args)
  "Apply FN with ARGS in left or right window.
If windows doesn't exists, split current window."
  (select-window
   (if-let ((xwidget-buff
             (car
              (straight-extra-buffers-in-mode
               'xwidget-webkit-mode
               (delete-dups
                (mapcar #'window-buffer (window-list)))))))
       (get-buffer-window xwidget-buff)
     (let ((other-wind (or (if (and (active-minibuffer-window)
                                    (minibuffer-selected-window))
                               (with-minibuffer-selected-window
                                 (let ((wind (selected-window)))
                                   (or
                                    (window-right wind)
                                    (window-left wind)
                                    (split-window-horizontally))))
                             (let ((wind (selected-window)))
                               (or
                                (window-right wind)
                                (window-left wind)
                                (split-window-horizontally))))
                           (let ((exclude-winds
                                  (delete
                                   nil
                                   (list (get-buffer-window
                                          "*Completions*" 0)
                                         (active-minibuffer-window)))))
                             (cond ((= 1
                                       (length (seq-difference (window-list)
                                                               exclude-winds)))
                                    (if (active-minibuffer-window)
                                        (with-minibuffer-selected-window
                                          (split-window-right))
                                      (split-window-right)))
                                   (t
                                    (if (active-minibuffer-window)
                                        (with-minibuffer-selected-window
                                          (or (window-right (selected-window))
                                              (when (window-left
                                                     (selected-window))
                                                (selected-window))))
                                      (or (window-right (selected-window))
                                          (when (window-left (selected-window))
                                            (selected-window))))))))))
       other-wind)))
  (when fn (apply fn args)))

(defun straight-extra-browse-url (url)
  "Try to browse URL with `eaf-open-browser', `xwidget' or `eww'."
  (cond ((not (display-graphic-p))
         (browse-url url))
        ((fboundp 'eaf-open-browser)
         (eaf-open-browser url))
        ((featurep 'xwidget-internal)
         (require 'xwidget)
         (let ((xwidget-webkit-last-session-buffer
                (car (straight-extra-buffers-in-mode
                      'xwidget-webkit-mode))))
           (cond ((buffer-live-p xwidget-webkit-last-session-buffer)
                  (pop-to-buffer-same-window xwidget-webkit-last-session-buffer)
                  (with-current-buffer xwidget-webkit-last-session-buffer
                    (xwidget-webkit-browse-url url)))
                 (t
                  (xwidget-webkit-browse-url
                   url)))))
        (t
         (browse-url url))))

(defun straight-extra-browse-action (package-name)
  "Browse PACKAGE-NAME with xwidget."
  (when-let ((url (straight-extra-get-url package-name)))
    (if (active-minibuffer-window)
        (with-selected-window (selected-window)
          (straight-extra-browse-xwidget-in-other-window
           'straight-extra-browse-url url))
      (straight-extra-browse-xwidget-in-other-window
       'straight-extra-browse-url url))))

;;;###autoload
(defun straight-extra-repo-status ()
  "Preview file without exiting minibuffer."
  (interactive)
  (when-let* ((input
               (if (and (eq completing-read-function
                            'ivy-completing-read)
                        (fboundp 'ivy-state-current)
                        (boundp 'ivy-last))
                   (ivy-state-current ivy-last)
                 (minibuffer-contents-no-properties)))
              (file (when (and
                           (file-exists-p input)
                           (file-readable-p input))
                      input))
              (minw (minibuffer-selected-window)))
    (with-minibuffer-selected-window
      (if (fboundp 'magit-status)
          (magit-status file)
        (vc-diff)))))

(defun straight-extra-completing-read-file (prompt files &optional predicate
                                                   require-match initial-input
                                                   hist def
                                                   inherit-input-method)
  "Read file name, prompting with PROMPT and completing with FILES.
PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, INHERIT-INPUT-METHOD
are the same as for `completing-read'."
  (sit-for 0.5)
  (minibuffer-with-setup-hook
      (lambda ()
        (use-local-map
         (let ((map
                (make-sparse-keymap)))
           (define-key map (kbd "C-j") #'straight-extra-repo-status)
           (set-keymap-parent map (current-local-map))
           map)))
    (completing-read prompt files
                     predicate
                     require-match
                     initial-input hist def
                     inherit-input-method)))

;;;###autoload
(defun straight-extra-jump-to-uncommited-repo (&optional confirm-dir)
  "Open modified repositiory in straight repos dir.
With optional argument CONFIRM-DIR ask about directory, othervise use straight
repositories."
  (interactive "P")
  (require 'magit nil t)
  (require 'git-util nil t)
  (let ((file (straight-extra-completing-read-file
               "Repo:"
               (if (fboundp 'git-util-modified-repos-in-dir)
                   (git-util-modified-repos-in-dir
                    (if confirm-dir
                        (read-directory-name "Directory" (straight--repos-dir))
                      (straight--repos-dir)))
                 (directory-files (straight--repos-dir)
                                  t
                                  directory-files-no-dot-files-regexp)))))
    (if (fboundp 'magit-status)
        (magit-status file)
      (find-file file))))
(defvar use-package-keywords)

(defun straight-extra-use-package-at-point-p ()
  "Return non nil at the beginning of `use-package' form."
  (when-let ((sexp (straight-extra-sexp-at-point)))
    (and (eq 'use-package (car-safe sexp))
         (listp (cdr sexp))
         (symbolp (cadr sexp))
         (cadr sexp))))

(defun straight-extra-inside-use-package-p ()
  "Return non nil inside `use-package' form."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (save-excursion
      (catch 'found
        (while (straight-extra-move-with 'backward-up-list)
          (when (straight-extra-use-package-at-point-p)
            (throw 'found (point))))))))

(defun straight-extra-get-current-package-name ()
  "Return package symbol if point is inside `use-package' form."
  (when-let ((beg (straight-extra-inside-use-package-p)))
    (save-excursion
      (goto-char beg)
      (down-list 1)
      (forward-sexp 2)
      (symbol-at-point))))

(defun straight-extra-get-use-package-bounds ()
  "Return bounds of parent `use-package' form."
  (when-let ((start (straight-extra-inside-use-package-p)))
    (save-excursion
      (goto-char start)
      (straight-extra-move-with 'forward-sexp)
      (cons start (point)))))

(defun straight-extra-get-use-package-keywords ()
  "Jumpt to the end of KEYWORD is `use-package' form."
  (pcase-let ((`(,beg . ,_end)
               (straight-extra-get-use-package-bounds)))
    (save-excursion
      (when beg
        (goto-char beg)
        (ignore-errors (seq-filter #'keywordp (straight-extra-sexp-at-point)))))))

;;;###autoload
(defun straight-extra-jump-to-use-package-keyword ()
  "Jumpt to the end of KEYWORD is `use-package' form."
  (interactive)
  (pcase-let ((`(,beg . ,_end)
               (straight-extra-get-use-package-bounds)))
    (when beg
      (when-let ((keyword-end
                  (save-excursion
                    (let* ((existing
                            (straight-extra-get-use-package-keywords))
                           (annotf (lambda (s)
                                     (if
                                         (not (memq (intern s) existing))
                                         " (insert)"
                                       "")))
                           (keyword (completing-read "Keyword"
                                                     (lambda
                                                       (str pred action)
                                                       (if
                                                           (eq action
                                                               'metadata)
                                                           `(metadata
                                                             (annotation-function
                                                              . ,annotf))
                                                         (complete-with-action
                                                          action
                                                          (seq-uniq
                                                           (append
                                                            existing
                                                            use-package-keywords))
                                                          str
                                                          pred))))))
                      (down-list)
                      (straight-extra-keyword-end-pos (intern keyword))))))
        (goto-char keyword-end)))))

(defun straight-extra-move-to--next-keyword ()
  "Move point to the next keyword start on the same depth."
  (while (keywordp (straight-extra-sexp-at-point))
    (forward-char 1))
  (let ((pos))
    (while (while (progn
                    (and (not (keywordp (straight-extra-sexp-at-point)))
                         (setq pos (straight-extra-move-with 'forward-sexp))))))
    (if pos
        (progn (goto-char pos)
               (straight-extra-move-with 'backward-sexp 1)
               (point))
      (point))))

(defun straight-extra-copy-keyword-value (keyword)
  "Copy KEYWORD value in `use-package' form."
  (when (memq keyword (straight-extra-get-use-package-keywords))
    (goto-char (straight-extra-keyword-end-pos keyword))
    (let ((beg (point)))
      (straight-extra-move-to--next-keyword)
      (buffer-substring-no-properties beg (point)))))

(defun straight-extra-keyword-end-pos (keyword)
  "Jumpt to the end of KEYWORD in `use-package' form."
  (pcase-let ((`(,beg . ,_end)
               (straight-extra-get-use-package-bounds)))
    (when beg
      (save-excursion
        (goto-char beg)
        (down-list)
        (let ((pos))
          (while (progn
                   (and (not (eq (straight-extra-sexp-at-point)
                                 keyword))
                        (setq pos (straight-extra-move-with
                                   'forward-sexp)))))
          (when pos
            (goto-char pos)
            pos))))))

(defun straight-extra-jump-to-keyword (keyword)
  "Jumpt to the end of KEYWORD is `use-package' form."
  (pcase-let ((`(,beg . ,_end)
               (straight-extra-get-use-package-bounds)))
    (when beg
      (let ((pos))
        (while (progn
                 (and (not (eq (straight-extra-sexp-at-point) keyword))
                      (setq pos (straight-extra-move-with 'forward-sexp)))))
        (when pos
          (goto-char pos)
          pos)))))

;;;###autoload
(defun straight-extra-insert-customs ()
  "Insert custom settings inside `use-package'."
  (interactive)
  (when-let* ((sexp (save-excursion
                      (when (straight-extra-backward-up-list)
                        (straight-extra-sexp-at-point))))
              (package (and (eq 'use-package (car-safe sexp))
                            (listp (cdr sexp))
                            (symbolp (cadr sexp))
                            (cadr sexp)
                            (symbol-name (cadr sexp))))
              (customs (mapcar
                        (lambda (it)
                          (list it (symbol-value it)))
                        (seq-filter
                         (lambda (it)
                           (when (and (custom-variable-p
                                       it)
                                      (string-prefix-p
                                       package
                                       (symbol-name
                                        it))
                                      (not
                                       (byte-code-function-p
                                        (symbol-value
                                         it))))
                             it))
                         obarray))))
    (newline-and-indent)
    (insert ":custom " (replace-regexp-in-string "^[(]\\|[)]$" "" (let ((print-length
                                                                         nil))
                                                                    (prin1-to-string
                                                                     customs))))
    (when (fboundp 'prettier-elisp)
      (prettier-elisp))))


(defun straight-extra--package-annotation (recipes)
  "Return an annotation function for RECIPES keys.
See documentation of `straight-extra--package-completion' for a
description of RECIPES.
Each package is annotated with its installation status (installed
or built-in) and its remote repository."
  (let ((annotfmt
         (propertize
          (concat (propertize " " 'display '(space :align-to 40)) " %s"
                  (propertize " " 'display '(space :align-to 51)) " %s")
          'face 'completions-annotations)))
    (lambda (package)
      (let* ((recipe (gethash package recipes))
             (remote (straight-extra-get-url package)))
        (straight--with-plist recipe (type)
          (format annotfmt
                  (cond ((eq type 'built-in)
                         "built-in")
                        ((straight--installed-p recipe) "installed")
                        (t ""))
                  (or remote "")))))))

(defun straight-extra-s-strip-props (item)
  "If ITEM is string, return it without text properties.

 If ITEM is symbol, return it is `symbol-name.'
 Otherwise return nil."
  (cond ((stringp item)
         (let ((str (seq-copy item)))
           (set-text-properties 0 (length str) nil str)
           str))
        ((and item (symbolp item))
         (symbol-name item))
        (nil item)))

;;;;;; Annotation functions
(defun straight-extra--annotate (package)
  "Return annotation for PACKAGE."
  (setq package (straight-extra-s-strip-props package))
  (let ((annotfmt
         (propertize
          (concat (propertize " " 'display '(space :align-to 35)) " %s"
                  (propertize " " 'display '(space :align-to 51)) " %s")
          'face 'completions-annotations)))
    (let* ((recipe (ignore-errors (gethash package straight--recipe-cache)))
           (melpa-descr (or (straight-extra-annotate-melpa-description package)
                            "")))
      (or (ignore-errors
            (straight--with-plist recipe (type)
              (concat (cond ((eq type 'built-in)
                             (propertize " (built-in)"
                                         'face 'success))
                            ((straight--installed-p recipe)
                             (propertize " (installed)"
                                         'face 'success))
                            (t ""))
                      " " (format annotfmt
                                  (or melpa-descr "")
                                  (if (eq type 'built-in)
                                      ""
                                    (straight-extra-get-url package
                                                            recipe))))))
          ""))))

(defun straight-extra--search-for-use-package-call (package)
  "Search for PACKAGE config in current buffer."
  (when (stringp package)
    (setq package (intern package)))
  (or (catch 'found
        (goto-char (point-max))
        (while (when (straight-extra-re-search-backward
                      (regexp-opt
                       straight-extra-use-package-symbol-names
                       'symbols)
                      nil t 1)
                 (straight-extra-backward-up-list))
          (when-let ((sexp (straight-extra-sexp-at-point)))
            (when (and
                   (symbolp (car-safe sexp))
                   (member (symbol-name (car-safe sexp))
                           straight-extra-use-package-symbol-names)
                   (listp (cdr sexp))
                   (symbolp (nth 1 sexp))
                   (eq (nth 1 sexp) package))
              (throw 'found (point))))))))

(defun straight-extra-find-use-package (package)
  "Search for first `use-package' call with PACKAGE using `find' and `grep'.
Return cons with a filename and point if found, or nil."
  (let* ((default-directory user-emacs-directory)
         (files (split-string
                 (shell-command-to-string
                  (string-join
                   (list
                    "find . -name '*.el' -not -path '*/straight/*' -type f -exec grep --color=never -l --null -e"
                    (shell-quote-argument
                     package)
                    "\\{\\} +")
                   " "))
                 "\0" t)))
    (let ((file)
          (found)
          (symb))
      (when files (setq symb
                        (if (stringp package)
                            (intern package)
                          package)))
      (while (setq file (pop files))
        (setq file (expand-file-name file user-emacs-directory))
        (with-temp-buffer
          (erase-buffer)
          (let (emacs-lisp-mode-hook)
            (emacs-lisp-mode))
          (insert-file-contents file)
          (setq found (straight-extra--search-for-use-package-call symb))
          (when found
            (setq found (cons file found))
            (setq files nil))))
      found)))

(defun straight-extra-jump-to-installed-package (package &optional other-wind)
  "Jump  to the first `use-package' call with PACKAGE using `find' and `grep'.
If OTHER-WIND is non nil, find file in other window."
  (when-let ((found (straight-extra-find-use-package package)))
    (if other-wind
        (find-file-other-window (car found))
      (find-file (car found)))
    (goto-char (cdr found))
    (pulse-momentary-highlight-region
     (cdr found)
     (save-excursion
       (forward-sexp)
       (point)))))

(defun straight-extra--package-completion (hash)
  "Return a  package name completion function.
HASH must be a hash table mapping package name strings to recipes."
  (let* ((recipes
          (reverse
           (mapcar #'straight-extra-s-strip-props (copy-tree (hash-table-keys
                                                              (copy-hash-table
                                                               hash))))))
         (annotfn (straight-extra--package-annotation hash))
         (metadata `(metadata (category . straight-recipe)
                              (annotation-function . ,annotfn))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          metadata
        (complete-with-action
         action recipes string pred)))))

(defun straight-extra-get-current-candidate ()
  "Return current minibuffer selection."
  (when (minibufferp)
    (if
        (and
         (eq
          completing-read-function
          'ivy-completing-read)
         (fboundp
          'ivy-state-current)
         (boundp
          'ivy-last))
        (ivy-state-current
         ivy-last)
      (with-selected-window
          (active-minibuffer-window)
        (car (split-string
              (minibuffer-contents-no-properties)))))))

;;;###autoload
(defun straight-extra-preview-installed-location ()
  "Find `use-package' call with current minibuffer candidate."
  (interactive)
  (when-let ((package (straight-extra-get-current-candidate)))
    (with-minibuffer-selected-window
      (straight-extra-jump-to-installed-package
       package))))

(defun straight-extra-read-installed-package (&optional preview-action predicate
                                                        initial-input)
  "Read package from `straight--recipe-cache' with INITIAL-INPUT and PREDICATE.
PREVIEW-ACTION should be a function that accept one argument - current
minibuffer candidate. It will be called in window called before minibuffer."
  (when (and initial-input (symbolp initial-input))
    (setq initial-input (symbol-name initial-input)))
  (minibuffer-with-setup-hook
      (lambda ()
        (when (minibufferp)
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-j")
                        (if preview-action
                            (lambda ()
                              (interactive)
                              (when-let
                                  ((package
                                    (straight-extra-get-current-candidate)))
                                (with-minibuffer-selected-window
                                  (funcall
                                   (or preview-action
                                       'straight-extra-jump-to-installed-package)
                                   package))))))
            (use-local-map (make-composed-keymap map (current-local-map))))))
    (completing-read
     "Package "
     (straight-extra--package-completion
      (copy-hash-table
       straight--recipe-cache))
     predicate
     t
     initial-input)))


;;;###autoload
(defun straight-extra-jump-to-package-readme (package)
  "Use `completing-read' to select a PACKAGE.
MESSAGE is displayed as the prompt; it should not end in punctuation
or whitespace.

FILTER is a function accepting one argument: a straight style recipe plist.
If it returns nil, the package is not considered a selection candidate."
  (interactive (list (straight-extra-read-installed-package
                      'straight-extra-find-file-readme
                      nil
                      (straight-extra-get-current-package-name))))
  (straight-extra-find-readme-other-window-for-current package))


;;;###autoload
(defun straight-extra-jump-to-package-config (package)
  "Use `completing-read' to select a PACKAGE.
MESSAGE is displayed as the prompt; it should not end in punctuation
or whitespace.

FILTER is a function accepting one argument: a straight style recipe plist.
If it returns nil, the package is not considered a selection candidate."
  (interactive
   (list
    (straight-extra-read-installed-package
     'straight-extra-jump-to-installed-package
     (lambda (it)
       (not (member it (mapcar #'symbol-name
                               straight-recipe-repositories)))))))
  (straight-extra-jump-to-installed-package package))


;;;###autoload (autoload 'straight-extra-transient-menu "straight-extra.el" nil t)
(transient-define-prefix straight-extra-transient-menu ()
  "Command dispatcher for straight."
  [["Check"
    ("C" "all" straight-check-all)
    ("c" "package" straight-check-package)]
   ["Rebuild"
    ("R" "all" straight-rebuild-all)
    ("r" "package" straight-rebuild-package)
    ("e" "prune" straight-prune-build)]
   ["Fetch"
    ("F" "all" straight-fetch-all)
    ("f" "package" straight-fetch-package)]
   ["Pull"
    ("A" "all" straight-pull-all)
    ("P" "package" straight-pull-package)]]
  [["Merge"
    ("M" "all" straight-merge-all)
    ("m" "package" straight-merge-package)]
   ["Normalize"
    ("N" "all" straight-normalize-all)
    ("n" "package" straight-normalize-package)]
   ["Push"
    ("U" "all" straight-push-all)
    ("u" "package" straight-push-package)]
   ["Versions"
    ("z" "freeze" straight-freeze-versions)
    ("T" "thaw" straight-thaw-versions)]]
  [["Explore"
    ("s" "use" straight-extra-install-package)
    ("d" "Jump to installed package readme"
     straight-extra-jump-to-package-readme)
    ("b" "browse" straight-extra-browse-package)]
   ["Other"
    ("i" "Insert at point"
     straight-extra-insert-use-package-at-point)
    ("p" "Jump to installed package config"
     straight-extra-jump-to-package-config)
    ("j" "jump to uncommited"
     straight-extra-jump-to-uncommited-repo)
    ("g" "get recipe" straight-get-recipe)
    ("w" "watcher start"
     straight-watcher-start)]])

(provide 'straight-extra)
;;; straight-extra.el ends here