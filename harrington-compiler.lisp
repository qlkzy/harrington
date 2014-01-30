;;;;;; Compiler for the rules in Harrington (targeting python)

;;;;; Bookkeeping

(defpackage :harrington-compiler
  (:use :common-lisp))

(in-package :harrington-compiler)

;;;;; Definitions

(defstruct predicate
  offset
  condition)

(defstruct test
  predicate
  names)

(defparameter matcher-names
  '((* nil)
    (? 'unknown)
    (- 'empty)
    (× 'offboard)
    (÷ 'no-ship)
    (% 'except-ship)
    (/ 'maybe-ship)
    (X 'ship)))

(defparameter test-pattern-a
  '((* ? -)
    (X / %)
    (÷ × *)))

(defparameter test-pattern-b
  '((X ?)))

(defparameter test-pattern-c
  '((* ? -)))

(defun matcher-name (m)
  (assoc m matcher-names))

(defun pattern-to-predicates (pattern)
  (loop
     for row in pattern
     for ri from 0
     append (loop
               for matcher in row
               for ci from 0
               collect (make-predicate :offset (list ri ci)
                                       :condition (matcher-name matcher)))))

(defun pattern-to-tests (name pattern)
  (loop for p in (pattern-to-predicates pattern)
     when (predicate-condition p)
     collect (make-test :predicate p :names (list name))))

(defun test-pred-equal (x y)
  (and (equal (predicate-offset (test-predicate x))
              (predicate-offset (test-predicate y)))
       (equal (predicate-condition (test-predicate x))
              (predicate-condition (test-predicate y)))))

(defun test-pred-before (x y)
  (let ((px (test-predicate x))
        (py (test-predicate y))))
  (or (pred-offset-before px py)
      (pred-condition-before px py)))

(defun pred-offset-before (x y)
  (or (< (car (predicate-offset x))
         (car (predicate-offset y)))
      (and (>= (car (predicate-offset x))
               (car (predicate-offset y)))
           (< (cadr (predicate-offset x))
              (cadr (predicate-offset y))))))

(defun pred-condition-before (x y)
  (let ((conditions (mapcar #'cadr matcher-names)))
    (< (position (predicate-condition x) conditions)
       (position (predicate-condition y) conditions))))

(defun merge-one-test (x y)
  (make-test :predicate (test-predicate x)
             :names (append (test-names x)
                            (test-names y))))

(defun merge-tests (x y &optional acc)
  (cond
    ((and (null x)
          (null y))
     acc)
    ((null x)
     (append (nreverse y) acc))
    ((null y)
     (append (nreverse x) acc))
    ((test-pred-equal (car x) (car y))
     (merge-tests (cdr x)
                  (cdr y)
                  (cons (merge-one-test (car x) (car y))
                        acc)))
    ((test-pred-before (car x) (car y))
     (merge-tests (cdr x)
                  y
                  (cons (car x) acc)))
    (t
     (merge-tests x
                  (cdr y)
                  (cons (car y) acc)))))
