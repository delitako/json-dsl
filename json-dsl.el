;;; json-dsl.el --- Simple DSL for JSON -*- lexical-binding: t; -*-

;; Author: Alphazino
;; Version: 0.2
;; URL: https://github.com/Alphazino/json-dsl

;;; Commentary:

;; The builtin json-encode almost works as a DSL for writing json,
;; but it has some potential pitfalls that may result in problems.
;; For example, a list will be encoded as a json array except when empty,
;; where it will be null. Empty alists and plists will have similar issues.

;;; Code:

(require 'json)
(require 'seq)
(require 'pcase)

(defconst json-dsl--empty-obj (make-hash-table :size 1))
(defconst json-dsl--empty-arr (make-vector 0 nil))

(defun json-dsl--convert-key (key)
  "Convert a key to a string to avoid special cases."
  (pcase key
    ((pred keywordp) (substring (symbol-name key) 1))
    ((pred symbolp) (symbol-name key))
    ((pred stringp) key)
    (_ (error "Invaid key: %s" key))))

(defun json-dsl--convert-pair (pair)
  "Convert a single list (PAIR) to a dotted pair."
  (pcase pair
    (`(,key ,value) (cons (json-dsl--convert-key key) (json-dsl--convert-data value)))
    (_ (error "Invalid key-value pair: %s" pair))))

(defun json-dsl--convert-obj (obj)
  "Convert OBJ to an alist for json-encode."
  (let ((pairs (seq-partition obj 2)))
    (if pairs
      (mapcar #'json-dsl--convert-pair pairs)
      json-dsl--empty-obj)))

(defun json-dsl--convert-list (data)
  "Convert DATA to arrays and objects."
  (pcase data
    ((and `(arr . ,tail) (guard tail)) (mapcar #'json-dsl--convert-data tail))
    ((and `(arr . ,tail) (guard (not tail))) json-dsl--empty-arr)
    (`(obj . ,tail) (json-dsl--convert-obj tail))
    (_ (error "Invalid json-dsl value: %s" data))))

(defun json-dsl--convert-data (data)
  "Convert DATA from DSL to format that json-encode will understand."
  (pcase data
    ('true t)
    ('false json-false)
    ('null json-null)
    ((pred keywordp) (substring (symbol-name data) 1))
    ((pred symbolp) (symbol-name data))
    ((pred numberp) data) 
    ((pred stringp) data)
    ((pred listp) (json-dsl--convert-list data))
    (_ (error "Invalid json-dsl value: %s" data))))

;;;###autoload
(defun json-dsl (data &optional pretty-print)
  "Generate a JSON string from DATA. It will pretty print if PRETTY-PRINT is non-nil.

JSON true, false, and null are represented with symbols of the same name.
  If you are encoding symbols from an unknown source, convert them to strings.

Keywords will be encoded as strings with the colon removed.

All other symbols will be encoded as strings as is.

Numbers will be passed directly to json-encode and formatted appropriately.

Arrays must be formatted as lists like (arr val1 val2 val3 ...).
  Vectors or lists without arr as the first value will not work.

Objects must be formatted like (obj key1 val1 key2 val2 ...).
  Start with obj and then alternate between keys and values.
  Keys may be strings, symbols, or keywords"
  (let ((json-encoding-pretty-print pretty-print))
    (json-encode (json-dsl--convert-data data))))

;;;###autoload
(defun json-dsl-pretty (data)
  "Same as json-dsl but always pretty prints DATA."
  (json-dsl data t))

(provide 'json-dsl)

;;; json-dsl.el ends here
