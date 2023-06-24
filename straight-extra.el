;;; straight-extra.el --- Extra utils for straight.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/straight-extra
;; Version: 0.1.0
;; Keywords: convenience abbrev
;; Package-Requires: ((emacs "28.1") (project "0.9.8"))
;; SPDX-License-Identifier: GPL-3.0-or-later

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

(require 'project)

(declare-function find-library-suffixes "find-func")
(declare-function find-library-name "find-func")

(defcustom straight-extra-save-use-package-file user-emacs-directory
  "Where to save packages."
  :group 'straight-extra
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

(defcustom straight-extra-melpa-cache-alist (expand-file-name
                                             "var/.straight-extra-melpa.cache"
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

(defvar straight-recipe-repositories)
(defvar straight-extra--readed-files)
(defvar straight-extra--var-types (list
                                   (cons 'defvar 3)
                                   (cons 'defconst 3)
                                   (cons 'defvar-keymap 3)))

(defvar straight-extra--custom-types (list
                                      (cons 'defcustom 3)))
(defvar straight-extra--func-types
  (list
   (cons 'defun 3)
   (cons 'defmacro 2)
   (cons 'defsubst 3)
   (cons 'defhydra 3)
   (cons 'transient-define-prefix 3)
   (cons 'transient-define-suffix 3)
   (cons 'transient-define-argument 3)
   (cons 'transient-define-infix 3)
   (cons 'cl-defun 3)
   (cons 'cl-defsubst 3)
   (cons 'cl-defmacro 3)
   (cons 'cl-defgeneric 3)
   (cons 'cl-defmethod 3)))

(defvar straight-extra--modes-types
  (list
   (cons 'define-minor-mode 2)
   (cons 'define-derived-mode 4)
   (cons 'define-generic-mode 8)
   (cons 'define-compilation-mode 3)
   (cons 'easy-mmode-define-minor-mode 2)) )

(defvar straight-extra--def-type-poses
  (append
   (list (cons 'define-skeleton 2)
         (cons 'ert-deftest 3)
         (cons 'define-widget 3)
         (cons 'easy-mmode-define-minor-mode 2)
         (cons 'defclass 4)
         (cons 'cl-defstruct 3))
   straight-extra--var-types
   straight-extra--custom-types
   straight-extra--func-types
   straight-extra--modes-types))

(defvar straight-extra--interactive-types
  (list
   (cons 'defun 3)
   (cons 'defsubst 3)
   (cons 'cl-defun 3)
   (cons 'cl-defsubst 3)))

(defvar straight-extra--modes-types
  (list
   (cons 'define-minor-mode 2)
   (cons 'define-derived-mode 4)
   (cons 'define-generic-mode 8)
   (cons 'define-compilation-mode 3)
   (cons 'easy-mmode-define-minor-mode 2)) )



(defun straight-extra--find-lib-in-dir (sym dir)
  "Lookup for library SYM in current directory DIR."
  (require 'find-func)
  (when-let ((file (ignore-errors
                     (file-truename (find-library-name (symbol-name sym))))))
    (when (file-in-directory-p file dir)
      file)))

(defun straight-extra--sexp-declare-p (sexp)
  "Return non-nil if SEXP is declared form."
  (pcase sexp
    (`(defvar ,name)
     (list 'defvar name))
    (`(declare-function ,name)
     (list 'declare-function name))
    (`(declare-function ,name
                        ,_file)
     (list 'declare-function name))
    (`(declare-function ,name
                        ,_file
                        ,_args)
     (list 'declare-function name))
    (`(declare-function ,name ,_file ,_args ,_fileonly)
     (list 'declare-function name))))

(defun straight-extra--unquote (exp)
  "Return EXP unquoted."
  (declare (pure t)
           (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun straight-extra--get-doc-from-sexp (sexp)
  "Return documentation from SEXP."
  (when (proper-list-p sexp)
    (let* ((type (car-safe sexp))
           (pos (and type
                     (cdr
                      (assq type straight-extra--def-type-poses))))
           (doc-str
            (pcase type
              ('defvar-keymap (plist-get sexp :doc))
              ((guard (and pos
                           (eq type 'cl-defmethod)
                           (memq (nth 2 sexp) '(:around :after
                                                        :before))))
               (nth (1+ pos) sexp))
              ((guard (and pos))
               (nth pos sexp)))))
      doc-str)))
(defun straight-extra-read-sexp (item &optional extra-props)
  "Parse ITEM and return plist with EXTRA-PROPS."
  (let ((result (straight-extra--parse-sexp item extra-props)))
    (if (and (car-safe result)
             (symbolp (car result)))
        (list result)
      result)))


(defun straight-extra-plist-keys (plist)
  "Return the keys in PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    keys))

(defun straight-extra-annotate-value (str max-len)
  "Trim STR if the length exceeds MAX-LEN else return STR."
  (replace-regexp-in-string "[\n\r\n\f]+"
                            "\s"
                            (if (and (> (length str) max-len))
                                (concat (substring-no-properties str 0
                                                                 (-
                                                                  max-len
                                                                  3))
                                        "...")
                              str)))


(defun straight-extra-completing-read-annotated (prompt collection &optional
                                                        annot-fn keymap
                                                        predicate require-match
                                                        initial-input hist def
                                                        inherit-input-method)
  "Read a string in the minibuffer, with completion, ANNOT-FN and KEYMAP.
PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF and
INHERIT-INPUT-METHOD have the same meaning as for `completing-read'."
  (let* ((alist (copy-tree collection))
         (annotf
          (or annot-fn
              (cond ((and (consp (car alist))
                          (symbolp (car alist)))
                     (lambda (it)
                       (let ((value (cdr-safe (assq (intern it) alist))))
                         (concat " "
                                 (straight-extra-annotate-value
                                  (if
                                      (and
                                       (listp
                                        value)
                                       (=
                                        1
                                        (length
                                         value)))
                                      (prin1-to-string
                                       (car
                                        value))
                                    (prin1-to-string
                                     value))
                                  80)))))))))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (and keymap
                     (minibufferp))
            (let ((map (make-composed-keymap keymap
                                             (current-local-map))))
              (use-local-map map))))
      (completing-read prompt
                       (lambda (str pred action)
                         (if (eq action 'metadata)
                             `(metadata
                               (annotation-function . ,annotf))
                           (complete-with-action action alist
                                                 str pred)))
                       predicate require-match
                       initial-input hist def
                       inherit-input-method))))
(defun straight-extra-read-setq-variables ()
  "Parse ITEM and return plist with EXTRA-PROPS."
  (let ((result (sexp-at-point)))
    (pcase result
      (`(setq . ,plist)
       (let ((vars (straight-extra-plist-keys
                    plist)))
         (straight-extra-completing-read-annotated "Variable: "
                                                   vars
                                                   (lambda (it)
                                                     (concat
                                                      " "
                                                      (straight-extra-annotate-value
                                                       (prin1-to-string
                                                        (plist-get
                                                         plist
                                                         (intern
                                                          it)))
                                                       90)))))))))

(defun straight-extra--parse-sexp (item &optional extra-props)
  "Parse ITEM and return plist with EXTRA-PROPS."
  (when (proper-list-p item)
    (let ((type (car-safe item)))
      (when (and type
                 (symbolp type))
        (pcase type
          ('quote nil)
          ((or 'with-eval-after-load 'eval-when-compile
               'eval-after-load
               'if 'progn
               'and
               'let 'if-let 'when-let 'with-no-warnings
               'when 'unless 'eval-and-compile)
           (mapcan (lambda (it)
                     (straight-extra-read-sexp it extra-props))
                   (cdr item)))
          ('define-key
           (pcase item
             (`(define-key ,map ,key
                           ,(and cmd
                                 (guard
                                  (symbolp (straight-extra--unquote
                                            cmd)))))
              (list
               (cons (straight-extra--unquote cmd)
                     (append
                      (list
                       :type 'define-key
                       :map map
                       :value
                       (cond ((stringp key)
                              key)
                             ((vectorp key)
                              (key-description key))
                             ((and (listp key)
                                   (stringp (cadr key)))
                              (cadr key))
                             (t key)))
                      extra-props))))))
          ('require
           (when-let ((sym
                       (pcase item
                         (`(require ,(and name
                                          (guard (listp name))
                                          (guard (eq (car-safe name) 'quote))))
                          (straight-extra--unquote name))
                         (`(require ,(and name
                                          (guard (listp name))
                                          (guard (eq (car-safe name) 'quote)))
                                    ,_)
                          (straight-extra--unquote name))
                         (`(require ,(and name
                                          (guard (listp name))
                                          (guard (eq (car-safe name) 'quote)))
                                    ,_
                                    ,(and optional
                                          (guard (not (eq optional nil)))))
                          (straight-extra--unquote name)))))
             (if-let ((file (straight-extra--find-lib-in-dir
                             sym
                             default-directory)))
                 (append (list (cons sym (append extra-props
                                                 (list :type type))))
                         (straight-extra--read-file file))
               (straight-extra-plist-remove-nils
                (list (cons sym (append extra-props
                                        (list :type type))))))))
          ((or 'use-package 'use-package!)
           (when-let ((sym (and
                            (cadr item)
                            (symbolp (cadr item))
                            (cadr item))))
             (let* ((data
                     (mapcan (lambda (it)
                               (straight-extra-read-sexp it extra-props))
                             (cdr item)))
                    (v (cons sym
                             (append
                              extra-props
                              (list :type
                                    type)))))
               (append
                data
                (list v)))))
          (_
           (let* ((doc (straight-extra--get-doc-from-sexp item))
                  (sym
                   (cond ((not (cadr item))
                          nil)
                         ((and (symbolp (cadr item)))
                          (cadr item))
                         (t (straight-extra--unquote (cadr item)))))
                  (declaration (straight-extra--sexp-declare-p item))
                  (props (append (list
                                  :type type
                                  :doc doc)
                                 extra-props)))
             (when (and sym (symbolp sym))
               (pcase type
                 ((guard declaration)
                  (setq props (plist-put props :declared t)))
                 ((guard
                   (assq type straight-extra--func-types))
                  (let ((args (seq-find
                               #'proper-list-p
                               item)))
                    (setq props (plist-put props :args args))
                    (when (and (assq type
                                     straight-extra--interactive-types)
                               (eq 'interactive
                                   (if doc
                                       (cadr
                                        (member
                                         doc
                                         item))
                                     (car-safe
                                      (nth
                                       (cdr
                                        (assq
                                         type
                                         straight-extra--interactive-types))
                                       item)))))
                      (setq props (plist-put props :interactive t)))))
                 ((guard (assq type straight-extra--custom-types))
                  (setq props (plist-put props :value (nth 2 item))))
                 ((guard
                   (assq type (append
                               straight-extra--var-types)))
                  (setq props (plist-put props :keymap
                                         (or (eq type 'defvar-keymap)
                                             (when-let*
                                                 ((value (nth 2 item))
                                                  (vals
                                                   (and (listp value)
                                                        (symbolp
                                                         (car value))
                                                        (memq (car
                                                               value)
                                                              '(let let*))
                                                        (car (seq-drop
                                                              value 1)))))
                                               (when (and (listp vals)
                                                          (listp (car vals)))
                                                 (seq-find
                                                  (lambda (it)
                                                    (when-let ((val (and
                                                                     (listp
                                                                      (cdr
                                                                       it))
                                                                     (listp
                                                                      (cadr
                                                                       it))
                                                                     (cadr
                                                                      it))))
                                                      (and
                                                       (= 1 (length val))
                                                       (symbolp (car val))
                                                       (memq (car val)
                                                             '(make-sparse-keymap
                                                               make-keymap)))))
                                                  vals)))))))
                 ('provide
                  (setq sym (straight-extra--unquote sym)))
                 ((or 'put 'add-hook
                      'advice-add)
                  (when-let ((hook
                              (straight-extra--unquote
                               (nth 1
                                    item))))
                    (setq sym hook)
                    (setq props (plist-put
                                 props
                                 :value
                                 (straight-extra--unquote (nth 2
                                                               item))))))
                 (_
                  (unless (special-form-p sym)
                    (setq sym nil))))
               (when sym
                 (cons sym
                       (straight-extra-plist-remove-nils
                        props)))))))))))

(defun straight-extra-parse-items-in-file (file)
  "Find items in FILE."
  (with-temp-buffer
    (erase-buffer)
    (insert-file-contents file)
    (let ((sexps)
          (emacs-lisp-mode-hook nil)
          (sexp))
      (emacs-lisp-mode)
      (goto-char (point-min))
      (while (setq sexp (ignore-errors (read (current-buffer))))
        (let ((end (point))
              (beg)
              (autoload-item))
          (save-excursion
            (forward-sexp -1)
            (setq beg (point))
            (forward-line -1)
            (when (looking-at ";;;###")
              (setq autoload-item
                    (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))))
          (let* ((default-directory (file-name-directory
                                     file))
                 (parsed (delq nil
                               (straight-extra-read-sexp sexp
                                                         (list
                                                          :autoload
                                                          autoload-item
                                                          :start beg
                                                          :end end
                                                          :file file)))))
            (setq sexps (append sexps parsed)))))
      sexps)))

(defun straight-extra--read-file (file)
  "Find items in FILE."
  (if (not (boundp 'straight-extra--readed-files))
      (let ((straight-extra--readed-files))
        (unless (member file straight-extra--readed-files)
          (push file straight-extra--readed-files)
          (straight-extra-parse-items-in-file file)))
    (unless (member file straight-extra--readed-files)
      (push file straight-extra--readed-files)
      (straight-extra-parse-items-in-file file))))

(defun straight-extra-group-with (fn items &optional transform-fn)
  "Group ITEMS by calling FN with every item.
FN should return key.
TRANSFORM-FN should return transformed item."
  (seq-reduce (lambda (acc it)
                (let* ((key (funcall fn it))
                       (val (if transform-fn (funcall transform-fn it) it))
                       (cell (assoc key acc))
                       (group (if cell
                                  (append (cdr cell)
                                          (list val))
                                (list val))))
                  (if cell
                      (setcdr cell group)
                    (push (cons key group) acc))
                  acc))
              (seq-copy items) '()))

(defun straight-extra-plist-remove-nils (plist)
  "Return the keys in PLIST."
  (let* ((result (list 'head))
         (last result))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (new (and val (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (cdr result)))

(defun straight-extra-get-library-items (lib)
  "Find items in LIB."
  (let* ((sym (if (stringp lib)
                  (intern lib)
                lib))
         (name (if (stringp lib)
                   lib
                 (symbol-name lib)))
         (found (progn
                  (require sym nil t)
                  (file-truename (find-library-name name)))))
    (straight-extra-group-with
     (lambda (it)
       (let* ((props (cdr it))
              (type (plist-get props :type)))
         (cond ((or (eq type 'transient-define-prefix)
                    (plist-get props :interactive))
                :commands)
               ((memq type
                      (list 'define-minor-mode 'define-derived-mode
                            'define-global-minor-mode
                            'define-globalized-minor-mode))
                :commands)
               ((plist-get props :autoload)
                :autoload)
               ((eq type 'defcustom) :custom)
               ((eq type 'defvar-keymap) :bind)
               ((plist-get props :keymap) :bind)
               ((and (car-safe it)
                     (symbolp (car-safe it))
                     (boundp (car-safe it))
                     (keymapp (symbol-value (car it))))
                :bind)
               ((and (car-safe it)
                     (symbolp (car-safe it))
                     (boundp (car-safe it))
                     (keymapp (symbol-value (car it))))
                :bind)
               (t type))))
     (straight-extra--read-file found)
     (lambda (it)
       (cons (car it)
             (straight-extra-plist-remove-nils
              (cdr
               it)))))))

(defvar find-library-source-path)
(defvar ivy-last)
(defvar use-package-keywords)

(require 'straight)

(defvar straight-extra-melpa-packages-archive-hash
  (make-hash-table :test 'equal))

(defvar straight-extra-melpa-packages-archive-alist nil)
(defvar straight-extra-melpa-packages-archive-alist-downloads nil)
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
                         (memq (car value)
                               '(let let*))
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

(defun straight-extra-symbol-keymapp (sym)
  "Return t if value of symbol SYM is a keymap."
  (when-let ((val
              (when (boundp sym)
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
    (if-let ((name
              (when symb-prefix
                (car
                 (split-string (if (symbolp symb-prefix)
                                   (symbol-name symb-prefix)
                                 symb-prefix)
                               "-" t)))))
        (straight-extra-keymap-keys-to-alist
         keymap
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
  (when-let* ((sexp
               (unless (nth 4 (syntax-ppss (point)))
                 (list-at-point)))
              (type (car sexp))
              (id (straight-extra--unquote
                   (when (symbolp (nth 1 sexp))
                     (nth 1 sexp))))
              (name (symbol-name id)))
    (let ((doc
           (when-let ((pos (cdr
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
    (completing-read "Library: "
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


;;;###autoload
(defun straight-extra-jump-or-insert-to-use-package-keyword (&optional keyword)
  "Jumpt to the end of KEYWORD is `use-package' form."
  (interactive)
  (pcase-let ((`(,beg . ,_end)
               (straight-extra-get-use-package-bounds)))
    (when beg
      (when-let ((keyword-end
                  (save-excursion
                    (let* ((existing
                            (straight-extra-get-use-package-keywords))
                           (annotf
                            (lambda (s)
                              (if
                                  (not (memq (intern s) existing))
                                  " (insert)"
                                "")))
                           (keyword-name
                            (or
                             (when keyword
                               (if (symbolp keyword)
                                   (symbol-name keyword)
                                 keyword))
                             (completing-read "Keyword"
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
                           (keyword (intern keyword-name)))
                      (if (memq keyword existing)
                          (progn (straight-extra-jump-to-keyword keyword)
                                 (straight-extra-move-to--next-keyword)
                                 (skip-chars-backward "\s\t\n")
                                 (point))
                        (goto-char beg)
                        (down-list)
                        (when existing
                          (straight-extra-jump-to-keyword (car (last existing))))
                        (straight-extra-move-to--next-keyword)
                        (newline-and-indent)
                        (save-excursion
                          (insert (concat keyword-name
                                          "\s")))
                        (indent-for-tab-command)
                        (re-search-forward "\s" nil t 1))))))
        (goto-char keyword-end)))))

(defun straight-extra-replace-or-insert-keyword (keyword value)
  "Change value of KEYWORD in `use-package' with new VALUE."
  (let* ((keywords (straight-extra-get-use-package-keywords))
         (keyword-name (symbol-name keyword)))
    (cond ((not (memq keyword keywords))
           (pcase-let ((`(,beg . ,_end)
                        (straight-extra-get-use-package-bounds)))
             (goto-char beg)
             (if (not (memq keyword '(:init :straight
                                            :preface)))
                 (progn (forward-sexp 1)
                        (forward-char -1)
                        (unless (looking-back "\n" 0)
                          (newline-and-indent)))
               (down-list)
               (forward-sexp 2)
               (newline-and-indent))
             (save-excursion
               (insert (concat keyword-name "\s" value)))
             (indent-for-tab-command)
             (re-search-forward "\s" nil t 1)))
          (t
           (let* ((beg (- (straight-extra-jump-to-keyword keyword)
                          (length keyword-name)))
                  (end (save-excursion
                         (straight-extra-move-to--next-keyword))))
             (replace-region-contents beg end
                                      (lambda ()
                                        (concat keyword-name "\s" value))))))))

(defun straight-extra-read-use-package-maps (maps &optional existing-maps)
  "Read `use-package' MAPS in minibuffer.
EXISTING-MAPS used for annotations and should be the same form, as MAPS."
  (let* ((alist
          (append (list 'global-map)
                  (seq-uniq (mapcar #'car (append maps existing-maps)))))
         (annotf (lambda (str)
                   (if (string= str "global-map")
                       " "
                     (concat " " (if (assq (intern str) existing-maps)
                                     "exists"
                                   ""))))))
    (completing-read "Map: "
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function . ,annotf)
                             (category . keymap))
                         (complete-with-action action alist str
                                               pred))))))

(defun straight-extra-maps-to-alist (maps)
  "Convert MAPS form to alists."
  (let ((curr maps)
        (globals)
        (result))
    (while (setq curr (pop maps))
      (cond ((eq curr :map)
             (let* ((name (pop maps))
                    (map (seq-take-while (lambda (it)
                                           (not (or
                                                 (keywordp (car-safe it))
                                                 (keywordp it))))
                                         maps)))
               (setq maps (seq-drop-while (lambda (it)
                                            (not (or
                                                  (keywordp (car-safe it))
                                                  (keywordp it))))
                                          maps))
               (push (cons name map) result)))
            ((eq (car-safe curr) :map)
             (push (cdr curr) result))
            (t (setq globals (push curr globals)))))
    (cons globals result)))



;;;###autoload
(defun straight-extra-insert-keymap (&optional map-sym keyword)
  "Insert KEYWORD (:bind or :bind*) with package MAP-SYM."
  (interactive)
  (unless keyword (setq keyword
                        (if
                            (memq :bind*
                                  (straight-extra-get-use-package-keywords))
                            :bind*
                          :bind)))
  (let* ((maps-alist (straight-extra-get-package-keymaps)))
    (let* ((current-maps
            (straight-extra-maps-to-alist
             (straight-extra-get-keyword-value-sexps
              keyword)))
           (globals (car current-maps))
           (keymap-cmds
            (mapcar #'cdr
                    (mapcan #'cadr
                            maps-alist)))
           (package-global-commands (seq-difference
                                     (straight-extra-get-package-commands)
                                     keymap-cmds))
           (current-maps-alist (cdr
                                current-maps))
           (map-sym (or map-sym
                        (intern (straight-extra-read-use-package-maps
                                 maps-alist
                                 current-maps-alist))))
           (existing-commands (if (eq map-sym 'global-map)
                                  globals
                                (assq map-sym current-maps-alist)))
           (default-commands (if (eq map-sym 'global-map)
                                 (mapcar (lambda (it)
                                           (cons "global" it))
                                         package-global-commands)
                               (car (cdr (assq map-sym maps-alist)))))
           (syms (seq-uniq (mapcar (lambda (it)
                                     (if (consp it)
                                         (cdr it)
                                       it))
                                   (append default-commands
                                           existing-commands))))
           (annotf (lambda (str)
                     (let* ((cmd (intern str))
                            (curr-val
                             (car (rassq cmd existing-commands)))
                            (default-key-value
                             (car (rassq cmd default-commands))))
                       (concat " "
                               (if curr-val
                                   (propertize
                                    (substring-no-properties
                                     (or
                                      curr-val
                                      default-key-value))
                                    'face
                                    'success)
                                 default-key-value)))))
           (commands
            (when-let* ((choice
                         (completing-read
                          "Command: "
                          (lambda (str pred action)
                            (if (eq action 'metadata)
                                `(metadata
                                  (annotation-function .
                                                       ,annotf)
                                  (category . keymap))
                              (complete-with-action action
                                                    (append
                                                     (list
                                                      "*all*")
                                                     syms)
                                                    str
                                                    pred))))))
              (if (string= choice "*all*")
                  (remove nil
                          (seq-uniq (append (mapcar #'cdr existing-commands)
                                            (mapcar #'cdr default-commands))))
                (list (intern choice))))))
      (dolist (command commands)
        (straight-extra-insert-command command keyword map-sym maps-alist))
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "z")
                     (lambda ()
                       (interactive)
                       (straight-extra-insert-keymap map-sym keyword)))
         map)))))

(defun straight-extra-insert-bind (&optional map-sym)
  "Insert :bind KEYWORD with package MAP-SYM."
  (interactive)
  (straight-extra-insert-keymap map-sym :bind))

(defun straight-extra-insert-bind* (&optional map-sym)
  "Insert :bind* KEYWORD with package MAP-SYM."
  (interactive)
  (straight-extra-insert-keymap map-sym :bind*))

(defun straight-extra-insert-command (command keyword &optional map-sym
                                              maps-alist)
  "Insert KEYWORD (bind or bind*) with COMMAND, possible with keymap MAP-SYM.
MAPS-ALIST is package keymaps."
  (when (eq map-sym 'global-map)
    (setq map-sym nil))
  (let* ((current-maps
          (straight-extra-maps-to-alist
           (straight-extra-get-keyword-value-sexps
            keyword)))
         (globals (car current-maps))
         (current-maps-alist (cdr current-maps))
         (existing-commands (if map-sym
                                (assq map-sym current-maps-alist)
                              globals))
         (default-commands (if map-sym
                               (car (cdr (assq map-sym maps-alist)))
                             globals))
         (existing-key (car (rassq command (cdr existing-commands))))
         (default-key-cell
          (car (rassq command default-commands)))
         (bound (straight-extra-jump-to-keyword keyword))
         (command-cell (prin1-to-string (cons
                                         (or default-key-cell
                                             existing-key
                                             "")
                                         command))))
    (cond ((not bound)
           (newline-and-indent)
           (if map-sym
               (progn
                 (insert (concat (symbol-name keyword) " (:map "
                                 (symbol-name
                                  map-sym)
                                 ")"))
                 (forward-char -1)
                 (newline-and-indent)
                 (insert command-cell))
             (insert (concat (symbol-name keyword) " (" command-cell ")"))))
          ((and existing-commands map-sym)
           (down-list)
           (catch 'map-pos
             (let ((sexp))
               (while
                   (setq sexp
                         (ignore-errors
                           (read
                            (current-buffer))))
                 (let ((value (ignore-errors (cadr sexp))))
                   (when-let ((found (if
                                         (when (eq sexp :map)
                                           (forward-sexp 1)
                                           (eq (sexp-at-point) map-sym))
                                         (progn
                                           (point))
                                       (if (and (eq (car-safe sexp) :map)
                                                (eq value
                                                    map-sym))
                                           (progn (forward-sexp -1)
                                                  (down-list)
                                                  (forward-sexp 2)
                                                  (point))))))
                     (throw 'map-pos found))))))
           (when existing-key
             (catch 'command-pos
               (let ((sexp))
                 (while
                     (setq sexp
                           (ignore-errors
                             (read
                              (current-buffer))))
                   (when (and (eq (cdr-safe sexp)
                                  command)
                              (equal (car-safe sexp)
                                     existing-key))
                     (throw 'command-pos (progn
                                           (let ((p
                                                  (point)))
                                             (forward-sexp
                                              -1)
                                             (delete-region
                                              (point) p))
                                           (point))))))))
           (newline-and-indent)
           (insert command-cell))
          ((and map-sym (not existing-commands))
           (down-list)
           (up-list)
           (forward-char -1)
           (newline-and-indent)
           (insert (concat "(:map " (symbol-name map-sym) ")"))
           (forward-char -1)
           (newline-and-indent)
           (insert command-cell))
          ((and existing-key
                (not map-sym))
           (down-list)
           (let ((sexp))
             (catch 'map-pos
               (while
                   (setq sexp
                         (ignore-errors
                           (read
                            (current-buffer))))
                 (when (and (eq (cdr-safe sexp)
                                command)
                            (equal (car-safe sexp)
                                   existing-key))
                   (throw 'map-pos (progn
                                     (let ((p (point)))
                                       (forward-sexp
                                        -1)
                                       (delete-region (point) p))
                                     (point))))))
             (insert command-cell)))
          (t (down-list)
             (let ((sexp))
               (catch 'map-pos
                 (while
                     (setq sexp
                           (ignore-errors
                             (read
                              (current-buffer))))
                   (when (or (keywordp sexp)
                             (keywordp (car-safe sexp)))
                     (throw 'map-pos (progn (forward-sexp -1)
                                            (point)))))))
             (insert command-cell)))
    (when (looking-at "[(]" 0)
      (save-excursion
        (newline-and-indent)))
    (forward-sexp -1)
    (down-list)
    (forward-char 1))
  (message "Type z to repeat"))

(defun straight-extra-read-custom-variables ()
  "Read custom variables."
  (let* ((alist (cdr
                 (when-let ((lib (straight-extra-get-current-package-name)))
                   (append
                    (cdr
                     (assq :custom (straight-extra-get-library-items lib)))))))
         (annotf (lambda (str)
                   (concat " "
                           (let* ((sym (intern str))
                                  (val (cdr (assq sym alist)))
                                  (value (ignore-errors (symbol-value sym)))
                                  (type (plist-get val :type))
                                  (doc (plist-get val :doc)))
                             (string-join
                              (remove nil
                                      (list
                                       (when type
                                         (propertize (prin1-to-string type)
                                                     'face
                                                     'font-lock-keyword-face))
                                       (when value
                                         (propertize
                                          (replace-regexp-in-string
                                           "[\n\r\f]+"
                                           " "
                                           (prin1-to-string
                                            value))
                                          'face
                                          'font-lock-keyword-face))
                                       (when doc
                                         (replace-regexp-in-string
                                          "[\r\f\n]+"
                                          " "
                                          doc))))
                              " ")))))
         (cycle-sort-fn (lambda (it) it))
         (display-sort-fn (lambda (it)
                            (seq-sort-by #'length '< it))))
    (completing-read "Candidates: "
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function . ,annotf)
                             (cycle-sort-function . ,cycle-sort-fn)
                             (display-sort-function . ,display-sort-fn))
                         (complete-with-action action alist str pred))))))

(defun straight-extra-get-customs ()
  "Insert custom settings inside `use-package'."
  (when-let* ((package-name (straight-extra-get-current-package-name))
              (items (straight-extra-get-library-items package-name))
              (customs (cdr (assq :custom items))))
    (mapcar (lambda (it)
              (list (car it)
                    (plist-get (cdr it) :value)))
            customs)))


;;;###autoload
(defun straight-extra-insert-customs ()
  "Insert custom settings inside `use-package'."
  (interactive)
  (let* ((current-customs (straight-extra-get-keyword-value-sexps
                           :custom))
         (customs (seq-remove (lambda (it)
                                (memq (car it) current-customs))
                              (straight-extra-get-customs))))
    (when customs
      (straight-extra-jump-or-insert-to-use-package-keyword :custom)
      (insert (replace-regexp-in-string "^[(]\\|[)]$" ""
                                        (let ((print-length
                                               nil))
                                          (prin1-to-string
                                           customs))))
      (when (fboundp 'prettier-elisp)
        (prettier-elisp)))))

;;;###autoload
(defun straight-extra-insert-setq-customs ()
  "Insert custom settings inside `use-package'."
  (interactive)
  (let* ((current-customs (straight-extra-get-keyword-value-sexps
                           :config))
         (customs (seq-remove (lambda (it)
                                (memq (car it) current-customs))
                              (straight-extra-get-customs))))
    (when customs
      (straight-extra-jump-or-insert-to-use-package-keyword :custom)
      (insert (replace-regexp-in-string "^[(]\\|[)]$" ""
                                        (let ((print-length
                                               nil))
                                          (prin1-to-string
                                           customs))))
      (when (fboundp 'prettier-elisp)
        (prettier-elisp)))))


(defun straight-extra-insert-load-path ()
  "Insert :load-path inside `use-package'."
  (interactive)
  (let* ((current-value
          (car (straight-extra-get-keyword-value-sexps :load-path)))
         (prompt (if (stringp current-value)
                     (format "Change %s to " current-value)
                   (format "Change load-path ")))
         (new-value (read-directory-name prompt)))
    (straight-extra-replace-or-insert-keyword :load-path new-value)))

;;;###autoload
(defun straight-extra-insert-config-keyword ()
  "Insert :config recipe to current use package form."
  (interactive)
  (let ((keywords (straight-extra-get-use-package-keywords))
        (sexps (straight-extra-get-keyword-value-sexps :config)))
    (let ((result (mapcar (lambda (it)
                            (append (list 'setq) it))
                          (straight-extra-get-customs))))
      (setq result (if (listp result)
                       (prin1-to-string result)
                     result))
      (cond ((and (memq :config keywords)
                  sexps)
             (straight-extra-jump-to-keyword :config)
             (when (straight-extra-re-search-forward "[(]" nil t 1)
               (forward-char -1)
               (pcase-let ((`(,beg . ,end)
                            (bounds-of-thing-at-point 'sexp)))
                 (replace-region-contents beg end
                                          (lambda () result)))))
            ((and (memq :config keywords)
                  (not sexps))
             (straight-extra-jump-to-keyword :config)
             (insert (concat " " result)))
            (t
             (straight-extra-jump-or-insert-to-use-package-keyword :config)
             (insert (if (symbol-at-point)
                         (concat " " result)
                       result)))))))

;;;###autoload
(defun straight-extra-insert-straight-keyword ()
  "Insert :straight recipe to current use package form."
  (interactive)
  (let ((recipe (straight-extra-current-recipe))
        (keywords (straight-extra-get-use-package-keywords))
        (sexps (straight-extra-get-keyword-value-sexps :straight)))
    (let* ((choices (remove nil `("git" "built-in" "nil"
                                  ,(when recipe (prin1-to-string recipe)))))
           (choice (completing-read ":type\s" choices))
           (result
            (pcase choice
              ("built-in" '(:type built-in))
              ("git"
               (require 'gh-repo nil t)
               (require 'git-util nil t)
               (cond ((and (fboundp 'gh-repo-read-user-repo)
                           (yes-or-no-p (format
                                         "Repo of %s?"
                                         (ignore-errors (process-lines
                                                         "git" "config"
                                                         "user.email")))))
                      `(:repo ,(substring-no-properties
                                (gh-repo-read-user-repo 'identity))
                              :type git
                              :host github))
                     ((and (fboundp 'git-util-url-get-candidates)
                           (fboundp 'git-util-url-to-recipe))
                      (let ((url
                             (completing-read
                              "Repo: "
                              (git-util-url-get-candidates))))
                        (or (git-util-url-to-recipe)
                            `(:repo ,(read-string "Repo:" url)
                                    :type git
                                    :host github))))
                     (t
                      `(:repo ,(read-string "Repo")
                              :type git
                              :host ,(read-string "Host")))))
              ("nil" nil)
              (_ choice))))
      (setq result (if (listp result)
                       (prin1-to-string result)
                     result))
      (cond ((and (memq :straight keywords)
                  sexps)
             (straight-extra-jump-to-keyword :straight)
             (when (straight-extra-re-search-forward "[(]" nil t 1)
               (forward-char -1)
               (pcase-let ((`(,beg . ,end)
                            (bounds-of-thing-at-point 'sexp)))
                 (replace-region-contents beg end
                                          (lambda () result)))))
            ((and (memq :straight keywords)
                  (not sexps))
             (straight-extra-jump-to-keyword :straight)
             (insert (concat " " result)))
            (t
             (straight-extra-jump-or-insert-to-use-package-keyword :straight)
             (insert (if (symbol-at-point)
                         (concat " " result)
                       result)))))))




(defun straight-extra-get-package-commands ()
  "Return keymaps for current `use-package' form."
  (when-let ((lib (straight-extra-get-current-package-name)))
    (pcase-let ((`(,beg . ,end)
                 (straight-extra-get-use-package-bounds)))
      (or
       (require lib nil t)
       (eval-region beg end)
       (require lib nil t))
      (mapcar #'car
              (cdr (assq :commands
                         (straight-extra-get-library-items lib)))))))



(defun straight-extra-get-package-keymaps ()
  "Return keymaps for current `use-package' form."
  (when-let ((lib (straight-extra-get-current-package-name)))
    (pcase-let ((`(,beg . ,end)
                 (straight-extra-get-use-package-bounds)))
      (or
       (require lib nil t)
       (eval-region beg end)
       (require lib nil t))
      (let ((items (cdr (assq :bind
                              (straight-extra-get-library-items lib)))))
        (remove nil
                (mapcar (lambda (it)
                          (when-let ((val (straight-extra-format-keymap-to-alist
                                           (symbol-value
                                            (car it)))))
                            (list (car it) val)))
                        items))))))

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
(defun straight-extra-configure-package ()
  "Insert `use-package' skeleton for PACKAGE-NAME."
  (interactive)
  (if (straight-extra-inside-use-package-p)
      (progn (straight-extra-insert-keymap))
    (when-let ((sexp (sexp-at-point)))
      (if
          (when (eq (car-safe sexp)
                    (mapcar #'intern-soft
                            straight-extra-use-package-symbol-names))
            (forward-char 1)
            (straight-extra-inside-use-package-p))
          (straight-extra-insert-keymap)
        (forward-char -1)
        (straight-extra-jump-to-package-config
         (straight-extra-read-installed-package
          'straight-extra-jump-to-installed-package
          (lambda (it)
            (not
             (member it
                     (mapcar
                      #'symbol-name
                      straight-recipe-repositories))))))
        (forward-char 1)))))

(defun straight-extra-locate-doc-files (package)
  "Return documentation files for PACKAGE.
PACKAGE should be either string or symbol."
  (when-let* ((libfile (ignore-errors (file-truename
                                       (find-library-name
                                        (if (symbolp package)
                                            (symbol-name package)
                                          package)))))
              (dir
               (let ((default-directory
                      (file-name-directory (file-truename
                                            libfile))))
                 (or (vc-root-dir) default-directory)))
              (files
               (let ((case-fold-search t))
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
    (or (seq-filter (apply-partially #'string-match-p "\\(org\\|md\\)$")
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
                 (progn
                   (require (intern (if (symbolp lib)
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
                            ,(when (and
                                    (require 'gh-repo nil t)
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

(defvar straight-extra-last-written-file nil)

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
                    ((and (not straight-extra-save-use-package-file)
                          (not straight-extra-last-written-file))
                     (setq straight-extra-last-written-file
                           (read-file-name
                            (format
                             "Save %s to: "
                             package)
                            user-emacs-directory)))
                    ((and straight-extra-last-written-file
                          (not straight-extra-save-use-package-file))
                     (read-file-name (format
                                      "Save %s to: "
                                      package)
                                     (if (file-directory-p
                                          straight-extra-last-written-file)
                                         straight-extra-last-written-file
                                       (file-name-directory
                                        straight-extra-last-written-file)))
                     (file-directory-p straight-extra-save-use-package-file))
                    ((functionp
                      straight-extra-save-use-package-file)
                     (funcall
                      straight-extra-save-use-package-file
                      package))
                    ((file-directory-p
                      straight-extra-save-use-package-file)
                     (read-file-name (format "Save %s to: "
                                             package)
                                     straight-extra-save-use-package-file))
                    ((file-name-absolute-p
                      straight-extra-save-use-package-file)
                     straight-extra-save-use-package-file))))
    (while (file-directory-p dest)
      (setq dest (read-file-name "File"
                                 dest)))
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
      (insert "(use-package " package ")"
              (if (looking-at "\n")
                  ""
                "\n"))
      (skip-chars-backward "\n")
      (forward-char -1)
      (require 'prettier-elisp nil t)
      (when (fboundp 'prettier-elisp)
        (prettier-elisp))
      (unless (get-buffer-window (current-buffer))
        (pop-to-buffer-same-window (current-buffer))))
    (with-selected-window (selected-window)
      (straight-extra-find-readme-other-window-for-current package))))

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
      (insert
       (let (print-length)
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
  (let* ((item (alist-get
                (if (stringp package)
                    (intern package)
                  package)
                straight-extra-melpa-packages-archive-alist))
         (props (alist-get 'props item))
         (parts (list
                 (when-let ((year (car (mapcar #'number-to-string
                                               (append
                                                (alist-get 'ver item)
                                                nil)))))
                   (substring year 0 4))
                 (alist-get 'desc item)
                 (alist-get 'downloads item)
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
                               font-lock-property-name-face
                               font-lock-warning-face
                               font-lock-comment-face)))))
                   parts)))
    (concat "\s" (string-join parts "\s"))))

(defvar json-false)
(defvar json-null)
(defvar json-object-type)
(defvar json-array-type)
(declare-function json-read "json")

(defun straight-extra-read-json (&optional object-type array-type false-object
                                           null-object)
  "Parse STR with natively compiled function or with json library.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array'/`vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.
If NULL-OBJECT is nil or `'nil', will be converted to nil.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-buffer
       :object-type (or object-type 'alist)
       :array-type
       (pcase array-type
         ('list 'list)
         ('vector 'array)
         (_ 'array))
       :null-object (if (or (not null-object)
                            (eq null-object 'nil))
                        nil
                      :null)
       :false-object (or false-object :false))
    (when
        (require 'json nil t)
      (let ((json-object-type (or object-type 'alist))
            (json-array-type
             (pcase array-type
               ('list 'list)
               ('array 'vector)
               (_ 'vector)))
            (json-null (or null-object :null))
            (json-false (or false-object :false)))
        (json-read)))))

(defun straight-extra-fetch-json (url)
  "Fetch json data at URL."
  (with-temp-buffer
    (url-insert-file-contents url)
    (straight-extra-read-json 'alist
                              'list
                              :false)))

(defun straight-extra-ensure-melpa (&optional force)
  "Fetch melpa archives. With FORCE inhibit cache."
  (cond ((and (not force)
              straight-extra-melpa-packages-archive-alist)
         straight-extra-melpa-packages-archive-alist)
        ((when (and (not force)
                    straight-extra-melpa-cache-filename
                    (file-exists-p straight-extra-melpa-cache-filename))
           (setq straight-extra-melpa-packages-archive-alist
                 (ignore-errors (straight-extra-unserialize
                                 straight-extra-melpa-cache-filename))))
         straight-extra-melpa-packages-archive-alist)
        (t
         (message "loading archives")
         (let ((data (straight-extra-fetch-json
                      "https://melpa.org/archive.json"))
               (downloads (straight-extra-fetch-json
                           "https://melpa.org/download_counts.json")))
           (dolist (it downloads)
             (let ((count (cdr it))
                   (key (car it)))
               (when-let* ((item
                            (assq key
                                  data))
                           (data
                            (cdr item)))
                 (setq data (push (cons 'downloads count) data))
                 (setcdr item data))))
           (setq straight-extra-melpa-packages-archive-alist
                 (seq-sort-by
                  (lambda (it)
                    (let* ((item
                            (cdr it))
                           (downloads (cdr (assq 'downloads item))))
                      (or downloads 0)))
                  #'>
                  data))
           (straight-extra-serialize straight-extra-melpa-packages-archive-alist
                                     straight-extra-melpa-cache-filename)
           straight-extra-melpa-packages-archive-alist))))

(defun straight-extra-ensure-downloads ()
  "Fetch json data at URL."
  (straight-extra-fetch-json "https://melpa.org/download_counts.json"))
(defvar straight-extra-melpa-downloads nil)
(defun straight-extra-add-downloads ()
  "Fetch json data at URL."
  (setq straight-extra-melpa-downloads
        (straight-extra-fetch-json
         "https://melpa.org/download_counts.json"))
  (dolist (it straight-extra-melpa-downloads)
    (let ((count (cdr it))
          (key (car it)))
      (when-let* ((item
                   (assq key
                         straight-extra-melpa-packages-archive-alist))
                  (data
                   (cdr item)))
        (setq data (push (cons 'downloads count) data))
        (setcdr item data)))))

(defun straight-extra--alistp (list)
  "Return T if LIST is an association list."
  (and (proper-list-p list)
       (seq-every-p #'consp list)))

(defun straight-extra-to-string (item)
  "Convert ITEM to STING."
  (pcase item
    ((pred straight-extra--alistp)
     (mapcar (lambda (it)
               (cons it (straight-extra-to-string (cdr item))))
             item))
    ((pred symbolp)
     (symbol-name item))
    ((pred stringp)
     item)
    ((pred numberp)
     (number-to-string item))
    ((guard (and item
                 (cdr-safe item)
                 (not (listp (cdr-safe item)))))
     (cons (car item)
           (straight-extra-to-string (cdr-safe item))))
    ((pred listp)
     (mapcar #'straight-extra-to-string item))))



(defun straight-extra-pick-from-alist (props alist)
  "Pick PROPS from ALIST."
  (mapcar (lambda (cell)
            (let ((key (car cell))
                  (value-format (or (cdr cell)
                                    (apply-partially #'format "%s")))
                  (value (cdr (assq (car cell) alist))))
              (if value
                  (funcall
                   value-format
                   (or (cdr (assq key cell))
                       ""))
                "")))
          props))

(defun straight-extra-add-sorters (columns)
  "Add sorters by number to COLUMNS."
  (apply #'vector
         (seq-map-indexed
          (lambda (col i)
            (when (eq (nth 2 col) 'number)
              (setf (nth 2 col)
                    (lambda (ea eb)
                      (let ((a (aref (cadr ea) i))
                            (b (aref (cadr eb) i)))
                        (> (string-to-number (or a "0"))
                           (string-to-number (or b "0")))))))
            col)
          columns)))

(define-derived-mode straight-extra-table-report-mode tabulated-list-mode
  "straight-extra-table report."
  "Show report gathered about unused definitions."
  (setq tabulated-list-format
        (straight-extra-add-sorters
         [("Package" 20 t)
          ("Description" 50 nil)
          ("Year" 5 number)
          ("Loads" 10 number)
          ("Keywords" 10)]))
  (tabulated-list-init-header))


(defun straight-extra-map-entries-alist ()
  "Map `straight-extra-melpa-packages-archive-alist' to tabulated entries."
  (let ((result))
    (pcase-dolist (`(,id . ,cell) straight-extra-melpa-packages-archive-alist)
      (let ((name (symbol-name id))
            (props (cdr (assq 'props cell))))
        (let ((downloads
               (or (cdr (assq 'downloads cell)) 0))
              (desc (or (cdr (assq 'desc cell)) ""))
              (url (cdr (assq 'url props)))
              (ver (substring-no-properties (string-join
                                             (mapcar
                                              (apply-partially
                                               #'format "%d")
                                              (cdr
                                               (assq 'ver
                                                     cell)))
                                             ".")
                                            0 4))
              (keywords (string-join
                         (cdr (assq 'keywords props)) " ")))
          (push (list (symbol-name id)
                      (apply #'vector
                             (mapcar
                              (lambda (it)
                                (or it ""))
                              (list
                               (cons name
                                     (list
                                      'follow-link t
                                      'button-data url
                                      'action
                                      #'straight-extra-browse-url-no-select))
                               desc
                               ver
                               (if downloads
                                   (number-to-string
                                    downloads)
                                 "0")
                               keywords))))
                result))))
    result))

;;;###autoload
(defun straight-extra-list-packages (&optional force)
  "Show ITEMS in Tabulated List buffer.
With prefix argument FORCE to refetch archives."
  (interactive "P")
  (with-current-buffer (get-buffer-create "*Melpa Packages*")
    (straight-extra-table-report-mode)
    (straight-extra-ensure-melpa force)
    (setq tabulated-list-entries
          (straight-extra-map-entries-alist))
    (tabulated-list-print)
    (unless (get-buffer-window (current-buffer))
      (pop-to-buffer (current-buffer)))))




;;;###autoload
(defun straight-extra-browse-in-other-window ()
  "Abort minibuffers and browse current package."
  (interactive)
  (when-let ((current
              (straight-extra-minibuffer-item)))
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
              (straight-extra-minibuffer-item)))
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
(defun straight-extra-completing-read-package (packages &optional predicate)
  "Read PACKAGES with annotated completions and PREDICATE."
  (interactive)
  (let ((items (mapcar (lambda (key)
                         (concat key (straight-extra--annotate key)))
                       (seq-copy packages))))
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


(defun straight-extra-table-get-prop (item property)
  "Get PROPERTY at ITEM.
ITEM should be a propertized string."
  (get-text-property 0 property item))

(defun straight-extra-table-button-action (button)
  "Run an action for BUTTON."
  (let ((current-window (selected-window))
        (props (get-text-property button 'props
                                  (marker-buffer button)))
        (report-buffer (current-buffer)))
    (with-selected-window current-window
      (let ((buff (progn (find-file-noselect
                          (straight-extra-table-get-prop props :file))
                         (get-file-buffer
                          (straight-extra-table-get-prop props :file)))))
        (switch-to-buffer-other-window buff)
        (when (with-current-buffer buff
                (yes-or-no-p "Remove?"))
          (when-let ((found (seq-find
                             (lambda (it) (let ((id (car it)))
                                            (equal id
                                                   (straight-extra-table-get-prop props :id))))
                             (buffer-local-value
                              'tabulated-list-entries
                              report-buffer))))
            (with-current-buffer report-buffer
              (setq tabulated-list-entries (delete found
                                                   tabulated-list-entries))
              (tabulated-list-revert))))))))



;;;###autoload
(defun straight-extra-browse-package (&optional arg)
  "Interectively browse a package.
With prefix argument ARG force to refetch archives."
  (interactive "P")
  (straight-extra-ensure-melpa arg)
  (straight-extra-browse-action
   (funcall-interactively #'straight-extra-completing-read-package
                          (straight-recipes-list))))

(defun straight-extra-read-package (&optional arg)
  "Read a package with annotated completions.
With prefix argument ARG force to refetch archives."
  (straight-extra-ensure-melpa arg)
  (straight-extra-completing-read-package
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
    (let ((url
           (if-let ((domain (car (alist-get host
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

(defun straight-extra-browse-url-no-select (url)
  "Browse URL with xwidget in other window."
  (with-selected-window (selected-window)
    (straight-extra-browse-xwidget-in-other-window
     #'straight-extra-browse-url url)))

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
  (when-let ((url (concat (straight-extra-get-url package-name) "#readme")))
    (if (active-minibuffer-window)
        (with-selected-window (selected-window)
          (straight-extra-browse-xwidget-in-other-window
           #'straight-extra-browse-url url))
      (straight-extra-browse-xwidget-in-other-window
       #'straight-extra-browse-url url))))

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
              (file
               (when (and
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

(defun straight-extra-use-package-at-point-p ()
  "Return non nil at the beginning of `use-package' form."
  (when-let ((sexp (straight-extra-sexp-at-point)))
    (and
     (car-safe sexp)
     (symbolp (car-safe sexp))
     (member (symbol-name (car sexp)) straight-extra-use-package-symbol-names)
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

(defun straight-extra-get-keyword-value (&optional keyword)
  "Jumpt to the end of KEYWORD is `use-package' form."
  (pcase-let ((`(,beg . ,_end)
               (straight-extra-get-use-package-bounds)))
    (when beg
      (save-excursion
        (when (straight-extra-jump-to-keyword keyword)
          (let* ((beg (point))
                 (end (save-excursion
                        (straight-extra-move-to--next-keyword))))
            (buffer-substring-no-properties beg end)))))))

(defun straight-extra-get-keyword-value-sexps (&optional keyword)
  "Jumpt to the end of KEYWORD is `use-package' form."
  (when-let ((str (straight-extra-get-keyword-value keyword)))
    (pcase keyword
      ((or :bind :bind*)
       (let ((val (car (straight-extra-read-string str))))
         (if (or (keywordp (car-safe val))
                 (stringp (car-safe val)))
             (progn
               (straight-extra-jump-to-keyword keyword)
               (down-list)
               (forward-char -1)
               (pcase-let* ((`(,beg . ,end)
                             (bounds-of-thing-at-point 'sexp))
                            (rep
                             (when beg
                               (buffer-substring-no-properties beg end))))
                 (replace-region-contents beg end
                                          (lambda ()
                                            (concat "(" rep ")"))))
               (list val))
           val)))
      (_ (straight-extra-read-string str)))))

(defun straight-extra-read-string (str)
  "Read STR."
  (with-temp-buffer
    (erase-buffer)
    (save-excursion
      (insert str))
    (let ((sexps)
          (sexp))
      (while
          (setq sexp
                (ignore-errors (read
                                (current-buffer))))
        (push sexp sexps))
      (reverse sexps))))

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

(defun straight-extra-move-to--next-keyword ()
  "Move point to the next keyword start on the same depth."
  (ignore-errors (while (keywordp (straight-extra-sexp-at-point))
                   (forward-sexp)))
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

(defun straight-extra-jump-to-keyword (keyword)
  "Jumpt to the end of KEYWORD is `use-package' form."
  (pcase-let ((`(,beg . ,_end)
               (straight-extra-get-use-package-bounds)))
    (when beg
      (goto-char beg)
      (down-list)
      (let ((pos))
        (while (progn
                 (and (not (eq (straight-extra-sexp-at-point) keyword))
                      (setq pos (straight-extra-move-with 'forward-sexp)))))
        (when pos
          (goto-char pos)
          pos)))))



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
      (or (ignore-errors
            (let* ((recipe (gethash package recipes))
                   (remote (straight-extra-get-url package)))
              (straight--with-plist recipe (type)
                (format annotfmt
                        (substring-no-properties
                         (cond ((eq type 'built-in)
                                "built-in")
                               ((straight--installed-p recipe) "installed")
                               (t "")))
                        (substring-no-properties (or remote ""))))))
          ""))))

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
           (melpa-descr (string-trim (or (straight-extra-annotate-melpa-description
                                          package)
                                         ""))))
      (when (string-empty-p melpa-descr)
        (when-let ((vect (cdr
                          (when (boundp 'package--builtins)
                            (assq (intern package)
                                  package--builtins)))))
          (setq melpa-descr (car (last (if (vectorp vect)
                                           (append vect nil)
                                         vect))))))
      (or (ignore-errors
            (straight--with-plist recipe (type)
              (concat
               (cond ((eq type 'built-in)
                      (propertize " (built-in)"
                                  'face 'font-lock-keyword-face))
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
        (while
            (when (straight-extra-re-search-backward
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

(defun straight-extra-scan-all-pass (filters)
  "Create an unary predicate function from FILTERS.
Return t if every one of the provided predicates is satisfied by provided
 argument."
  (lambda (item)
    (not (catch 'found
           (dolist (filter filters)
             (unless (funcall filter item)
               (throw 'found t)))))))

(defun straight-extra-scan-find-project-files (&optional directory no-filter)
  "Return all elisp files from project instance in DIRECTORY.
If optional argument NO-FILTER is non-nil, return all files, else
return only elisp files."
  (let* ((project-find-functions '(project-try-vc try))
         (pr (project-current t directory))
         (dirs (list (project-root pr)))
         (files (project-files pr dirs)))
    (if no-filter
        (seq-filter #'file-exists-p files)
      (seq-filter (straight-extra-scan-all-pass
                   (list (apply-partially #'string-suffix-p ".el")
                         #'file-exists-p))
                  files))))

(defun straight-extra-scan-project ()
  "Search for first `use-package' call with PACKAGE using `find' and `grep'.
Return cons with a filename and point if found, or nil."
  (let* ((default-directory user-emacs-directory)
         (files (straight-extra-scan-find-project-files user-emacs-directory)))
    (let ((straight-extra--readed-files)
          (file)
          (syms))
      (while (setq file (pop files))
        (unless (member file straight-extra--readed-files)
          (push file straight-extra--readed-files)
          (setq syms (nconc syms (straight-extra-parse-items-in-file file)))))
      syms)))

(defun straight-extra-find-all-use-package-calls ()
  "Search for first `use-package' call with PACKAGE using `find' and `grep'.
Return cons with a filename and point if found, or nil."
  (seq-filter (lambda (it)
                (let ((type (plist-get (cdr it) :type)))
                  (memq type
                        '(use-package straight-use-package use-package!))))
              (straight-extra-scan-project)))

(defun straight-extra-find-use-package (package)
  "Search for first `use-package' call with PACKAGE using `find' and `grep'.
Return cons with a filename and point if found, or nil."
  (let* ((default-directory user-emacs-directory)
         (files (split-string
                 (shell-command-to-string
                  (string-join
                   (list
                    "find . -name '*.el' -not -path '*/straight/*' -type f -exec grep --color=never -l --null -e"
                    "use-package"
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
(defvar straight-extra-found-use-packages nil)
(defun straight-extra-jump-to-installed-package (package &optional other-wind)
  "Jump  to the first `use-package' call with PACKAGE using `find' and `grep'.
If OTHER-WIND is non nil, find file in other window."
  (when-let* ((found (assq (if (stringp package)
                               (intern package)
                             package)
                           straight-extra-found-use-packages))
              (pl (cdr found)))
    (if other-wind
        (find-file-other-window (plist-get pl :file))
      (find-file (plist-get pl :file)))
    (goto-char (plist-get pl :start))
    (pulse-momentary-highlight-region
     (plist-get pl :start)
     (plist-get pl :end))))

(defun straight-extra--package-completion (hash &optional collection)
  "Return a  package name completion COLLECTION function.
HASH must be a hash table mapping package name strings to recipes."
  (let* ((recipes
          (reverse
           (mapcar #'straight-extra-s-strip-props (copy-tree (hash-table-keys
                                                              (copy-hash-table
                                                               hash))))))
         (annotfn 'straight-extra--annotate)
         (metadata `(metadata (category . straight-recipe)
                              (annotation-function . ,annotfn))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          metadata
        (complete-with-action
         action (or collection recipes) string pred)))))

(defun straight-extra-minibuffer-item ()
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
  (when-let ((package (straight-extra-minibuffer-item)))
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
  (setq straight-extra-found-use-packages
        (straight-extra-find-all-use-package-calls))
  (minibuffer-with-setup-hook
      (lambda ()
        (when (minibufferp)
          (let ((map (make-sparse-keymap)))
            (define-key map
                        (kbd "C-j")
                        (if preview-action
                            (lambda ()
                              (interactive)
                              (when-let ((package
                                          (straight-extra-minibuffer-item)))
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
       straight--recipe-cache)
      (mapcar (lambda (it)
                (substring-no-properties (symbol-name (car it))))
              straight-extra-found-use-packages))
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
  "Use `completing-read' to select a PACKAGE with CALLBACK.
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
  (when (straight-extra-jump-to-installed-package package)
    (forward-char 1)
    t))

(defun straight-extra-current-recipe ()
  "Return recipe for inside `use-package'."
  (when-let ((package (straight-extra-get-current-package-name)))
    (ignore-errors (gethash (symbol-name package) straight--recipe-cache))))



(defvar straight-extra-keywords-inserters-alist
  '((:bind .
           straight-extra-insert-bind)
    (:bind* .
            straight-extra-insert-bind*)
    (:config .
             straight-extra-insert-config-keyword)
    (:custom . straight-extra-insert-customs)
    (:straight . straight-extra-insert-straight-keyword)))




;;;###autoload (autoload 'straight-extra-configure-package-menu "straight-extra.el" nil t)
(transient-define-prefix straight-extra-configure-package-menu ()
  "Configure package at point."
  [:description
   (lambda ()
     (format "Configure %s"
             (straight-extra-get-current-package-name)))
   [("m" ":bind Insert keymap" straight-extra-insert-keymap)
    ("c" ":custom Customize" straight-extra-insert-customs)
    ("k" "Other keyword"
     straight-extra-jump-or-insert-to-use-package-keyword)
    ("d" "Readme"
     straight-extra-jump-to-package-readme)]
   [("o" "Other package"
     straight-extra-transient-menu)]]
  (interactive)
  (if (straight-extra-get-current-package-name)
      (transient-setup 'straight-extra-configure-package-menu)
    (when (call-interactively #'straight-extra-jump-to-package-config)
      (transient-setup 'straight-extra-configure-package-menu))))

;;;###autoload (autoload 'straight-extra-straight-only-menu "straight-extra.el" nil t)
(transient-define-prefix straight-extra-straight-only-menu ()
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
    ("T" "thaw" straight-thaw-versions)]
   ["Use"
    ("t" "try package" straight-use-package)]])

;;;###autoload (autoload 'straight-extra-transient-menu "straight-extra.el" nil t)
(transient-define-prefix straight-extra-transient-menu ()
  "Command dispatcher for straight."
  [["Explore"
    ("s" "use" straight-extra-install-package)
    ("d" "Jump to installed package readme"
     straight-extra-jump-to-package-readme)
    ("b" "browse" straight-extra-browse-package)
    ("S" "straight commands" straight-extra-straight-only-menu)]
   ["Configure"
    ("c" "Configure package" straight-extra-configure-package-menu)]
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