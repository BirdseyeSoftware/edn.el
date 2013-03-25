;;; edn.el --- EDN Notation parser / generator

;; Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Roman Gonzalez <romanandreg@gmail.com>
;; Inspired from json.el by Edward O'Connor <ted@oconnor.cx>
;; Version: 0.1
;; Keywords: convenience

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a library for parsing and generating EDN (Extensible Data
;; Notation).

;; Learn all about EDN here: <URL:https://github.com/edn-format/edn>.

;; The user-serviceable entry points for the parser are the functions
;; `edn-read' and `edn-read-from-string'. The encoder has a single
;; entry point, `edn-encode'.

;; Since there are several natural representations of key-value pair
;; mappings in elisp (alist, plist, hash-table), `edn-read' allows you
;; to specify which you'd prefer (see `edn-map-type').

;;; History:

;; 2013-02-06 - Initial version.

;;; Code:

(eval-when-compile (require 'cl))

;; Compatibility code

(defalias 'edn-encode-char0 'encode-char)
(defalias 'edn-decode-char0 'decode-char)


;; Parameters

(defvar edn-encode-timestamp nil
  "Encode tuples of integers as timestamps.")

(defvar edn-map-type 'alist
  "Type to convert EDN objects to.
Must be one of `alist', `plist', or `hash-table'. Consider let-binding
this around your call to `edn-read' instead of `setq'ing it.")


(defvar edn-key-type nil
  "Type to convert EDN keys to.
Must be one of `string', `symbol', `keyword', or nil.

If nil, `edn-read' will guess the type based on the value of
`edn-map-type':

    If `edn-map-type' is:   nil will be interpreted as:
      `hash-table'                `string'
      `alist'                     `symbol'
      `plist'                     `keyword'

Note that values other than `string' might behave strangely for
Sufficiently Weird keys. Consider let-binding this around your call to
`edn-read' instead of `setq'ing it.")

(defvar edn-false :edn-false
  "Value to use when reading EDN `false'.
If this has the same value as `edn-nil', you might not be able to tell
the difference between `false' and `nil'. Consider let-binding this
around your call to `edn-read' instead of `setq'ing it.")

(defvar edn-nil nil
  "Value to use when reading EDN `nil'.
If this has the same value as `edn-false', you might not be able to
tell the difference between `false' and `nil'. Consider let-binding
this around your call to `edn-read' instead of `setq'ing it.")



;;; Utilities

(defun edn-join (strings separator)
  "Join STRINGS with SEPARATOR."
  (mapconcat 'identity strings separator))

;; (defun edn-alist-p (list)
;;   "Non-nil if and only if LIST is an alist."
;;   (or (null list)
;;       (and (consp (car list))
;;            (edn-alist-p (cdr list)))))


(defun edn-alist-p (list)
  "Non-nil if and only if LIST is an alist."
  (and (listp list)
       (evenp (length list))
       (consp (car list))))

(defun edn-plist-p (list)
  "Non-nil if and only if LIST is a plist."
  (or (null list)
      (and (keywordp (car list))
           (consp (cdr list))
           (edn-plist-p (cddr list)))))

;; Reader utilities

(defsubst edn-advance (&optional n)
  "Skip past the following N characters."
  (forward-char n))

(defsubst edn-peek ()
  "Return the character at point."
  (let ((char (char-after (point))))
    (or char :edn-eof)))

(defsubst edn-pop ()
  "Advance past the character at point, returning it."
  (let ((char (edn-peek)))
    (if (eq char :edn-eof)
        (signal 'end-of-file nil)
      (edn-advance)
      char)))

(defun edn-skip-whitespace ()
  "Skip past the whitespace at point."
  (skip-chars-forward "\t\r\n\f\b "))



;; Error conditions

(put 'edn-error 'error-message "Unknown EDN error")
(put 'edn-error 'error-conditions '(edn-error error))

(put 'edn-readtable-error 'error-message "EDN readtable error")
(put 'edn-readtable-error 'error-conditions
     '(edn-readtable-error edn-error error))

(put 'edn-unknown-keyword 'error-message "Unrecognized keyword")
(put 'edn-unknown-keyword 'error-conditions
     '(edn-unknown-keyword edn-error error))

(put 'edn-number-format 'error-message "Invalid number format")
(put 'edn-number-format 'error-conditions
     '(edn-number-format edn-error error))

(put 'edn-string-escape 'error-message "Bad unicode escape")
(put 'edn-string-escape 'error-conditions
     '(edn-string-escape edn-error error))

(put 'edn-string-format 'error-message "Bad string format")
(put 'edn-string-format 'error-conditions
     '(edn-string-format edn-error error))

(put 'edn-keyword-format 'error-message "Bad EDN keyword format")
(put 'edn-keyword-format 'error-conditions
     '(edn-keyword-format edn-error error))

(put 'edn-symbol-format 'error-message "Bad EDN symbol format")
(put 'edn-symbol-format 'error-conditions
     '(edn-symbol-format edn-error error))


(put 'edn-set-format 'error-message "Bad EDN set")
(put 'edn-set-format 'error-conditions
     '(edn-set-format edn-error error))

(put 'edn-map-format 'error-message "Bad EDN object")
(put 'edn-map-format 'error-conditions
     '(edn-map-format edn-error error))



;;; Keywords

(defvar edn-keywords '("true" "false" "nil")
  "List of EDN keywords.")


;; Keyword parsing

(defun edn-read-keyword (keyword)
  "Read a EDN keyword at point.
KEYWORD is the keyword expected."
  (unless (member keyword edn-keywords)
    (signal 'edn-unknown-keyword (list keyword)))
  (mapc (lambda (char)
          (unless (char-equal char (edn-peek))
            (signal 'edn-unknown-keyword
                    (list (save-excursion
                            (backward-word 1)
                            (thing-at-point 'word)))))
          (edn-advance))
        keyword)
  (unless (looking-at "\\(\\s-\\|[],}]\\|$\\)")
    (signal 'edn-unknown-keyword
            (list (save-excursion
                    (backward-word 1)
                    (thing-at-point 'word)))))
  (cond ((string-equal keyword "true") t)
        ((string-equal keyword "false") edn-false)
        ((string-equal keyword "nil") edn-nil)))

;; Keyword encoding

(defun edn-encode-lang-keyword (keyword)
  "Encode KEYWORD as a EDN value."
  (cond ((eq keyword t)          "true")
        ((eq keyword edn-false) "false")
        ((eq keyword edn-nil)  "nil")))

;;; Numbers

;; Number parsing

(defun edn-read-number (&optional sign)
 "Read the EDN number following point.
The optional SIGN  argument is for internal use.

N.B.: Only numbers which can fit in Emacs Lisp's native number
representation will be parsed correctly."
 ;; If SIGN is non-nil, the number is explicitly signed.
 (let ((number-regexp
        "\\([0-9]+\\)?\\(\\.[0-9]+\\)?\\([Ee][+-]?[0-9]+\\)?"))
   (cond ((and (null sign) (char-equal (edn-peek) ?-))
          (edn-advance)
          (- (edn-read-number t)))
         ((and (null sign) (char-equal (edn-peek) ?+))
          (edn-advance)
          (edn-read-number t))
         ((and (looking-at number-regexp)
               (or (match-beginning 1)
                   (match-beginning 2)))
          (goto-char (match-end 0))
          (string-to-number (match-string 0)))
         (t (signal 'edn-number-format (list (point)))))))

;; Number encoding

(defun edn-encode-number (number)
  "Return a EDN representation of NUMBER."
  (format "%s" number))

;;; Strings

(defvar edn-special-chars
  '((?\" . ?\")
    (?\\ . ?\\)
    ;; (?/ . ?/)
    (?b . ?\b)
    (?f . ?\f)
    (?n . ?\n)
    (?r . ?\r)
    (?t . ?\t))
  "Characters which are escaped in EDN, with their elisp counterparts.")

;; String parsing

(defun edn-read-escaped-char ()
  "Read the EDN string escaped character at point."
  ;; Skip over the '\'
  (edn-advance)
  (let* ((char (edn-pop))
         (special (assq char edn-special-chars)))
    (cond
     (special (cdr special))
     ((not (eq char ?u)) char)
     ((looking-at "[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]")
      (let ((hex (match-string 0)))
        (edn-advance 4)
        (edn-decode-char0 'ucs (string-to-number hex 16))))
     (t
      (signal 'edn-string-escape (list (point)))))))


(defun edn-read-symbol ()
  "Read the EDN symbol at point."
  ;; skip the '
  (edn-advance)
  (when (char-equal (edn-peek) ? )
    (signal 'edn-symbol-format (list)))
  (let ((characters '())
        (char (edn-peek)))
    (while (and (not (eq char :edn-eof))
                (not (char-equal char ? ))
                (not (char-equal char ?\)))
                (not (char-equal char ?\}))
                (not (char-equal char ?\])))
      (push (edn-pop)
            characters)
      (setq char (edn-peek)))
    (if characters
        (intern (apply 'string (nreverse characters)))
      "")))

(defun edn-read-keyword ()
  "Read the EDN keyword at point."
  (edn-advance)
  (when (char-equal (edn-peek) ? )
    (signal 'edn-keyword-format (list)))
  (let ((characters '())
        (char (edn-peek)))
    (while (and (not (eq char :edn-eof))
                (not (char-equal char ? ))
                (not (char-equal char ?\)))
                (not (char-equal char ?\}))
                (not (char-equal char ?\])))
      (push (edn-pop)
            characters)
      (setq char (edn-peek)))
    (if characters
        (intern (concat ":" (apply 'string (nreverse characters))))
      "")))

(defun edn-read-string ()
  "Read the EDN string at point."
  (unless (char-equal (edn-peek) ?\")
    (signal 'edn-string-format (list "doesn't start with '\"'!")))
  ;; Skip over the '"'
  (edn-advance)
  (let ((characters '())
        (char (edn-peek)))
    (while (not (char-equal char ?\"))
      (push (if (char-equal char ?\\)
                (edn-read-escaped-char)
              (edn-pop))
            characters)
      (setq char (edn-peek)))
    ;; Skip over the '"'
    (edn-advance)
    (if characters
        (apply 'string (nreverse characters))
      "")))

;; String encoding

(defun edn-encode-char (char)
  "Encode CHAR as a EDN string."
  (setq char (edn-encode-char0 char 'ucs))
  (let ((control-char (car (rassoc char edn-special-chars))))
    (cond
     ;; Special EDN character (\n, \r, etc.)
     (control-char
      (format "\\%c" control-char))
     ;; ASCIIish printable character
     ((and (> char 31) (< char 161))
      (format "%c" char))
     ;; Fallback: UCS code point in \uNNNN form
     (t
      (format "\\u%04x" char)))))

(defun edn-encode-string (string)
  "Return a EDN representation of STRING."
  (format "\"%s\"" (mapconcat 'edn-encode-char string "")))

(defun edn-encode-keyword (kw)
  (format "%s" kw))

;;; EDN Objects

(defun edn-new-object ()
  "Create a new Elisp object corresponding to a EDN object.
Please see the documentation of `edn-map-type'."
  (cond ((eq edn-map-type 'hash-table)
         (make-hash-table :test 'equal))
        (t
         (list))))

(defun edn-add-to-object (object key value)
  "Add a new KEY -> VALUE association to OBJECT.
Returns the updated object, which you should save, e.g.:
    (setq obj (edn-add-to-object obj \"foo\" \"bar\"))
Please see the documentation of `edn-map-type' and `edn-key-type'."
  (let ((edn-key-type
         (if (eq edn-key-type nil)
             (cdr (assq edn-map-type '((hash-table . string)
                                           (alist . symbol)
                                           (plist . keyword))))
           edn-key-type)))
    (setq key
          (cond ((eq edn-key-type 'string)
                 (if (stringp key)
                     key
                   (format "%s" key)))

                ((eq edn-key-type 'keyword)
                 (if (keywordp key)
                     key
                   (intern (concat ":" key))))

                ;; ((eq edn-key-type 'symbol)
                ;;  ;; (if (symbolp key)
                ;;  ;;     key
                ;;  ;;   (intern key))
                ;;  key
                ;;  )
                (t
                 key)))
    (cond ((eq edn-map-type 'hash-table)
           (puthash key value object)
           object)
          ((eq edn-map-type 'alist)
           (cons (cons key value) object))
          ((eq edn-map-type 'plist)
           (cons key (cons value object))))))

;; EDN object parsing

(defun edn-read-map ()
  "Read the EDN object at point."
  ;; Skip over the "{"
  (edn-advance)
  (edn-skip-whitespace)
  ;; read key/value pairs until "}"
  (let ((elements (edn-new-object))
        key value)
    (while (not (char-equal (edn-peek) ?}))
      (edn-skip-whitespace)
      (setq key (edn-read))
      (edn-skip-whitespace)
      (when (char-equal (edn-peek) ?})
        (signal 'edn-map-format (list "odd number of values" (edn-peek))))
      (when (char-equal (edn-peek) ?,)
        (edn-advance)
        (edn-skip-whitespace))
      (setq value (edn-read))
      (setq elements (edn-add-to-object elements key value))
      (edn-skip-whitespace)
      (when (char-equal (edn-peek) ?,)
        (edn-advance)
        (edn-skip-whitespace)))
    ;; Skip over the "}"
    (edn-advance)
    elements))

;; Hash table encoding

(defun edn-encode-hash-table (hash-table)
  "Return a EDN representation of HASH-TABLE."
  (format "{%s}"
          (edn-join
           (let (r)
             (maphash
              (lambda (k v)
                (push (format "%s %s"
                              (edn-encode k)
                              (edn-encode v))
                      r))
              hash-table)
             r)
           " ")))

;; List encoding (including alists and plists)

(defun edn-encode-alist (alist)
  "Return a EDN representation of ALIST."
  (format "{%s}"
          (edn-join (mapcar (lambda (cons)
                               (format "%s %s"
                                       (edn-encode (car cons))
                                       (edn-encode (cdr cons))))
                             alist)
                     " ")))

(defun edn-encode-plist (plist)
  "Return a EDN representation of PLIST."
  (let (result)
    (while plist
      (push (concat (edn-encode (car plist))
                    " "
                    (edn-encode (cadr plist)))
            result)
      (setq plist (cddr plist)))
    (concat "{" (edn-join (nreverse result) " ") "}")))

(defun edn-encode-simple-list (list)
  "Return a EDN simple list"
  (concat "(" (mapconcat 'edn-encode list " ") ")"))


(defun rfc3339-datetime (&optional time)
  (let ((stamp (format-time-string "%Y-%m-%dT%H:%M:%S%z" time)))
    (format "%s:%s" (substring stamp 0 -2) (substring stamp -2))))

(defun edn-timestamp-p (list)
  (and edn-encode-timestamp
       (listp list)
       (= (length list) 2)
       (integerp (car list))
       (integerp (second list))))

(defun edn-encode-timestamp (list)
  (format "#inst \"%s\"" (rfc3339-datetime list)))

(defun edn-encode-list (list)
  "Return a EDN representation of LIST.
Tries to DWIM: simple lists become EDN lists, while alists and plists
become EDN objects."
  (cond ((null list)         "nil")
        ((edn-alist-p list)  (edn-encode-alist list))
        ((edn-plist-p list)  (edn-encode-plist list))
        ((edn-timestamp-p list) (edn-encode-timestamp list))
        ((listp list)        (edn-encode-simple-list list))
        (t
         (signal 'edn-error (list list)))))

;; List parsing

(defun edn-read-list ()
  "Read the EDN list at point."
  ;; Skip over the "("
  (edn-advance)
  (edn-skip-whitespace)
  ;; read values until ")"
  (let (elements)
    (while (not (char-equal (edn-peek) ?\)))
      (push (edn-read) elements)
      (edn-skip-whitespace)
      (when (char-equal (edn-peek) ?,)
        (edn-advance))
      (edn-skip-whitespace))
    ;; Skip over the ")"
    (edn-advance)
    (apply 'list (nreverse elements))))

;; Set parsing

(defun edn-read-set ()
  "Read the EDN vector at point."
  ;; Skip over the "#"
  (edn-advance)
  (unless (char-equal (edn-peek) ?\{)
    (signal 'edn-error (list "invalid set datastructure")))
  (edn-advance)
  (edn-skip-whitespace)
  ;; read values until "}"
  (let (elements)
    (while (not (char-equal (edn-peek) ?\}))
      (push (edn-read) elements)
      (edn-skip-whitespace)
      (when (char-equal (edn-peek) ?,)
        (edn-advance))
      (edn-skip-whitespace))
    ;; Skip over the "}"
    (edn-advance)
    (apply 'list (nreverse elements))))

;; Vector parsing

(defun edn-read-vector ()
  "Read the EDN vector at point."
  ;; Skip over the "["
  (edn-advance)
  (edn-skip-whitespace)
  ;; read values until "]"
  (let (elements)
    (while (not (char-equal (edn-peek) ?\]))
      (push (edn-read) elements)
      (edn-skip-whitespace)
      (when (char-equal (edn-peek) ?,)
        (edn-advance))
      (edn-skip-whitespace))
    ;; Skip over the "]"
    (edn-advance)
    (apply 'vector (nreverse elements))))

;; Array encoding

(defun edn-encode-vector (vec)
  "Return a EDN representation of ARRAY."
  (concat "[" (mapconcat 'edn-encode vec " ") "]"))



;;; EDN reader.

(defvar edn-readtable
  (let ((table
         '((?t  edn-read-keyword "true")
           (?f  edn-read-keyword "false")
           (?n  edn-read-keyword "nil")
           (?{  edn-read-map)
           (?\[ edn-read-vector)
           (?\( edn-read-list)
           (?\# edn-read-set)
           (?\" edn-read-string)
           (?:  edn-read-keyword)
           (?'  edn-read-symbol))))
    (mapc (lambda (char)
            (push (list char 'edn-read-number) table))
          '(?- ?+ ?. ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    table)
  "Readtable for EDN reader.")


(defun edn-read ()
  "Parse and return the EDN object following point.
Advances point just past EDN object."
  (edn-skip-whitespace)
  (let ((char (edn-peek)))
    (if (not (eq char :edn-eof))
        (let ((record (cdr (assq char edn-readtable))))
          (if (functionp (car record))
              (apply (car record) (cdr record))
            (signal 'edn-readtable-error record)))
      (signal 'end-of-file nil))))

;; Syntactic sugar for the reader

(defun edn-read-from-string (string)
  "Read the EDN object contained in STRING and return it."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (edn-read)))

(defun edn-read-file (file)
  "Read the first EDN object contained in FILE and return it."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (edn-read)))

;;; EDN encoder

(defun edn-encode (object)
  "Return a EDN representation of OBJECT as a string."
  (cond ((memq object (list t edn-nil edn-false))
         (edn-encode-lang-keyword object))
        ((stringp object)      (edn-encode-string object))
        ((keywordp object)     (edn-encode-keyword object))
        ((symbolp object)      (edn-encode-string
                                (symbol-name object)))
        ((numberp object)      (edn-encode-number object))
        ((arrayp object)       (edn-encode-vector object))
        ((hash-table-p object) (edn-encode-hash-table object))
        ((vectorp object)      (edn-encode-vector object))
        ((listp object)        (edn-encode-list object))
        (t                     (signal 'edn-error (list object)))))

(provide 'edn)

;;; edn.el ends here
