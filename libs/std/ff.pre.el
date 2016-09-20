;;; -*- lexical-binding: t -*-
(require 'cl)
(setq oc (make-hash-table :test 'equal))

; (defun oc-hash-from-alist (alist)
;   (let ((hash (make-hash-table :test 'equal)))
;     (mapcar (lambda (element)
;               (puthash (car element) (cdr element) hash))
;             alist)
;     hash))

(defun oc-hash-from-alist (plist)
  (let ((pl plist)
        (result (make-hash-table :test 'equal)))
    (while pl
      (puthash (car pl) (cadr pl) result)
      (setq pl (cddr pl)))
    result))

(puthash 'instances (make-hash-table :test 'equal) oc)

(puthash 'clone-object (lambda (obj) (copy-hash-table obj)) oc)

; (puthash 'clone-object (lambda (obj)
;   (let ((result (make-hash-table :test 'equal)))
;     (maphash (lambda (key val) (puthash key val result)) obj)
;     result)) oc)
