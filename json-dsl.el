;;; json-dsl.el --- Simple DSL for JSON -*- lexical-binding: t; -*-

;; Author: Alphazino
;; Maintainer: Alphazino
;; Version: 0.1
;; URL: https://github.com/Alphazino/json-dsl
;; Repository: https://github.com/Alphazino/json-dsl.git

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

(defun json-dsl--convert-pair (pair)
  "Convert a single list (PAIR) to a dotted pair."
  (pcase pair
    ((and `(,key ,value) (guard (or (symbolp key) (stringp key))))
     (cons key (json-dsl--convert-data value)))
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
(defun json-dsl (pretty-print data)
  "Generate a json string from DATA. Will pretty print if PRETTY-PRINT in non-nil."
  (let ((json-encoding-pretty-print pretty-print))
    (json-encode (json-dsl--convert-data data))))

(provide 'json-dsl)

;;; json-dsl.el ends here
