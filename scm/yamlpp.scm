;;; guile-yamlpp.scm --- Guile Scheme YAML handling based on yaml-cpp
;;;
;;; Copyright © 2023-2024 Georgios Athanasiou <yorgath@gmail.com>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <https://www.gnu.org/licenses/>.

(define-module (yamlpp)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 textual-ports)
  ;;
  #:export (yaml-null)
  ;;
  #:export (yaml-load)
  #:export (yaml-load-all)
  #:export (yaml-load-all-from-file)
  #:export (yaml-load-file)
  ;;
  #:export (make-yaml-emitter)
  #:export (yaml-emitter-good?)
  #:export (yaml-emitter-string)
  #:export (yaml-emit!)
  #:export (yaml-emit-comment!)
  #:export (yaml-emit-newline!)
  #:export (yaml-begin-doc!)
  #:export (yaml-end-doc!)
  #:export (yaml-begin-seq!)
  #:export (yaml-end-seq!)
  #:export (yaml-begin-map!)
  #:export (yaml-end-map!)
  #:export (yaml-emit-key!)
  #:export (yaml-emit-value!)
  #:export (yaml-emit-anchor!)
  #:export (yaml-emit-alias!)
  #:export (yaml-set-style!)
  #:export (yaml-set-string-format!)
  #:export (yaml-set-bool-format!)
  #:export (yaml-set-int-base!)
  #:export (yaml-set-seq-format!)
  #:export (yaml-set-map-format!)
  #:export (yaml-set-indent!)
  #:export (yaml-manipulators))

;; Make the C functions available.
(load-extension "libguile-yamlpp" "init")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Null values in YAML documents are converted to this.
(define yaml-null 'null)

(define (mention fmt . args)
  "Show a warning."
  (let* ((all-args (append (list #f fmt) args))
         (text (apply simple-format all-args)))
    (warn text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (yaml-load text)
  "Parse first YAML document in the string."
  (expand-node (yaml-load-node text)))

(define (yaml-load-all text)
  "Parse all the YAML documents in the string."
  (map (lambda (node) (expand-node node))
       (yaml-load-nodes text)))

(define (yaml-load-file path)
  "Parse the first document in a YAML file."
  (expand-node (yaml-load-node-from-file path)))

(define (yaml-load-all-from-file path)
  "Parse all the documents in a YAML file."
  (map (lambda (node) (expand-node node))
       (yaml-load-nodes-from-file path)))

(define (expand-node node)
  "Convert YAML node to list of atoms."
  (case (yaml-node-type node)
    ((null)
     yaml-null)
    ((scalar)
     (yaml-scalar-value node))
    ((sequence)
     ;; Convert sequence to vector, not list, like guile-json does.
     (list->vector
      (map (lambda (node) (expand-node node))
           (yaml-node->list node))))
    ((map)
     (map (lambda (pair)
            (cons (expand-node (car pair))
                  (expand-node (cdr pair))))
          (yaml-node->alist node)))
    (else 'undefined)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (yaml-emit! emitter obj)
  "Write the object to the given YAML emitter."
  (cond
   ((vector? obj) (emit-vector! emitter obj))
   ((list? obj) (emit-alist!  emitter obj))
   (else (yaml-emit-scalar! emitter obj))))

(define (emit-vector! emitter vec)
  "Write the vector as a YAML sequence."
  (yaml-begin-seq! emitter)
  (vector-for-each
   (lambda (i elt)
     (yaml-emit! emitter elt))
   vec)
  (yaml-end-seq! emitter))

(define (emit-alist! emitter alist)
  "Write the association list as a YAML mapping."
  (yaml-begin-map! emitter)
  (for-each
   (lambda (pair)
     (let ((key (car pair))
           (value (cdr pair)))
       (yaml-emit-key! emitter)
       (yaml-emit! emitter key)
       (yaml-emit-value! emitter)
       (yaml-emit! emitter value)))
   alist)
  (yaml-end-map! emitter))

(define (yaml-emit-scalar! emitter obj)
  "Write the scalar object to the given YAML emitter."
  (cond
   ((eq? yaml-null obj)
    (yaml-emit-null! emitter))
   ((string? obj)
    (yaml-emit-string! emitter obj))
   ((char? obj)
    (yaml-emit-string! emitter (string obj)))
   ((boolean? obj)
    (yaml-emit-boolean! emitter obj))
   ((number? obj)
    (emit-number! emitter obj))
   (else (scm-error 'yaml-emitter-error #f
                "Do not know how to emit object as YAML: ~S" obj obj))))

(define (emit-number! emitter number)
  "Write any number."
  (cond
   ((exact-integer? number)
    (yaml-emit-integer! emitter number))
   ((nan? number)
    (emit-nan! emitter))
   ((inf? number)
    (emit-inf! emitter number))
   ((rational? number)
    (emit-rational! emitter number))
   ((real? number)
    (emit-real! emitter number))))

(define (emit-nan! emitter)
  "Write the not-a-number symbol."
  (yaml-set-style! emitter 'auto)
  (yaml-emit-string! emitter ".nan"))

(define (emit-inf! emitter number)
  "Write the infinite number."
  (yaml-set-style! emitter 'auto)
  (yaml-emit-string! emitter
                     (if (positive? number) ".inf" "-.inf")))

(define (emit-rational! emitter number)
  "Write the rational number as a YAML float."
  (emit-real! emitter (exact->inexact number)))

(define (emit-real! emitter number)
  "Write the real number to the given YAML emitter."
  ;; Emit the number as a string.  Converting it to a double at the C
  ;; level results in unacceptable approximations.
  (yaml-set-style! emitter 'auto)
  (yaml-emit-string! emitter (number->string number)))

;; List of valid manipulators.
(define yaml-manipulators
  '(auto
    ;;
    single-quoted
    double-quoted
    literal
    ;;
    yes-no-bool
    true-false-bool
    on-off-bool
    upper-case
    lower-case
    camel-case
    long-bool
    short-bool
    ;;
    dec
    hex
    oct
    ;;
    block
    flow))

(define (filter-manipulators manipulators)
  "Keep only the valid manipulators."
  (let loop ((manips manipulators)
             (result '()))
    (if (null? manips)
        (reverse result)
        (let* ((manip (car manips))
               (result (if (memq manip yaml-manipulators)
                           (cons manip result)
                           (begin
                             (mention "Unknown manipulator ~S" manip)
                             result))))
          (loop (cdr manips) result)))))

(define (yaml-set-style! emitter . manipulators)
  "Set the style of the next element to be emitted.

See the `yaml-manipulators' variable for a list of valid manipulator
values.  The manipulators are applied in the order they appear; later
values may override previous ones."
  (for-each
   (lambda (manip)
     (yaml-set-style-1! emitter manip))
   (filter-manipulators manipulators)))

(define (yaml-set-string-format! emitter . manipulators)
  "Set the format of the emitted strings."
  (for-each
   (lambda (manip)
     (yaml-set-string-format-1! emitter manip))
   (filter-manipulators manipulators)))

(define (yaml-set-bool-format! emitter . manipulators)
  "Set the format of the emitted booleans."
  (for-each
   (lambda (manip)
     (yaml-set-bool-format-1! emitter manip))
   (filter-manipulators manipulators)))

(define (yaml-set-seq-format! emitter . manipulators)
  "Set the format of the emitted sequences."
  (for-each
   (lambda (manip)
     (yaml-set-seq-format-1! emitter manip))
   (filter-manipulators manipulators)))

(define (yaml-set-map-format! emitter . manipulators)
  "Set the format of the emitted mappings."
  (for-each
   (lambda (manip)
     (yaml-set-map-format-1! emitter manip))
   (filter-manipulators manipulators)))
