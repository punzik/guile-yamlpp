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
  #:export (yaml-string-manipulators)
  #:export (yaml-bool-manipulators)
  #:export (yaml-int-manipulators)
  #:export (yaml-manipulators)
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
  #:export (yaml-set-indent!))

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
;; Wrap the primitive procedures used for reading YAML.

(define (yaml-load-node string)
  "Read the first YAML document present in the string.

The result is a <yaml-node> object."
  (prim:yaml-load-node string))

(define (yaml-load-nodes string)
  "Read all the YAML documents present in the string.

The result is a list of <yaml-node> objects."
  (prim:yaml-load-nodes string))

(define (yaml-load-node-from-file path)
  "Read the first YAML document from the file at the given path.

The result is a <yaml-node> object."
  (prim:yaml-load-node-from-file path))

(define (yaml-load-nodes-from-file path)
  "Read all the YAML documents from the file at the given path.

The result is a list of <yaml-node> objects."
  (prim:yaml-load-nodes-from-file path))

(define (yaml-node-type node)
  "Return a symbol that denotes the type of the YAML node.

The result is one of the following symbols:

- null
- scalar
- sequence
- map
- undefined"
  (prim:yaml-node-type node))

(define (yaml-scalar-value node)
  "Return the value stored in the scalar YAML node.

This procedure should be used when it is known that NODE holds a
scalar YAML value that it is *not* null (i.e. nulls must be treated as
a separate case).  It tries to guess the correct type for the value,
resorting to string when everything else fails.  The type of the
result can be one of the following Scheme types:

- boolean
- integer
- double
- string"
  (prim:yaml-scalar-value node))

(define (yaml-node->list node)
  "Return a list of nodes of a YAML sequence node.

This should only be used for nodes of type 'sequence."
  (prim:yaml-node->list node))

(define (yaml-node->alist node)
  "Return an association list from the pairs in a YAML mapping node

This should only be used for nodes of type 'map."
  (prim:yaml-node->alist node))

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

;; YAML manipulators for strings.
(define yaml-string-manipulators
  '(auto
    single-quoted
    double-quoted
    literal))

;; YAML manipulators for booleans.
(define yaml-bool-manipulators
  '(yes-no-bool
    true-false-bool
    on-off-bool
    upper-case
    lower-case
    camel-case
    long-bool
    short-bool))

;; YAML manipulators for integers.
(define yaml-int-manipulators
  '(dec
    hex
    oct))

;; YAML manipulators for sequences.
(define yaml-seq-manipulators
  '(flow
    block))

;; YAML manipulators for mappings.
(define yaml-map-manipulators
  '(auto
    flow
    block))

;; List of all valid manipulators.
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

;; Construct a list of all known YAML manipulators from the partial
;; lists.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap the primitive procedures used for emitting YAML.

(define (make-yaml-emitter)
  "Return a new object that can be used to create YAML documents."
  (prim:make-yaml-emitter))

(define (yaml-emitter-good? emitter)
  "Return a boolean that shows the state of a YAML emitter.

It returns `#f' when the EMITTER is in a bad state.  This can happen
for example when the user has requested the ending of a parent
element (e.g. a sequence) when one of its children is still open."
  (prim:yaml-emitter-good? emitter))

(define (yaml-emitter-string emitter)
  "Return the contents of the YAML EMITTER as a string."
  (prim:yaml-emitter-string emitter))

(define (yaml-emit-null! emitter)
  "Emit the null YAML value."
  (prim:yaml-emit-null! emitter))

(define (yaml-emit-string! emitter string)
  "Write a string to the YAML document."
  (prim:yaml-emit-string! emitter string))

(define (yaml-emit-boolean! emitter boolean)
  "Write a boolean value to the YAML document."
  (prim:yaml-emit-boolean! emitter boolean))

(define (yaml-emit-integer! emitter number)
  "Write an integer to the YAML document."
  (prim:yaml-emit-integer! emitter number))

(define (yaml-emit-comment! emitter string)
  "Write a comment to the YAML document."
  (prim:yaml-emit-comment! emitter string))

(define (yaml-emit-newline! emitter)
  "Add a line break to the YAML document."
  (prim:yaml-emit-newline! emitter))

(define (yaml-begin-doc! emitter)
  "Mark the beginning of a new YAML document."
  (prim:yaml-begin-doc! emitter))

(define (yaml-end-doc! emitter)
  "Mark the ending of the current YAML document."
  (prim:yaml-end-doc! emitter))

(define (yaml-begin-seq! emitter)
  "Start a YAML sequence."
  (prim:yaml-begin-seq! emitter))

(define (yaml-end-seq! emitter)
  "End the current YAML sequence."
  (prim:yaml-end-seq! emitter))

(define (yaml-begin-map! emitter)
  "Start a YAML mapping."
  (prim:yaml-begin-map! emitter))

(define (yaml-end-map! emitter)
  "End the current YAML mapping."
  (prim:yaml-end-map! emitter))

(define (yaml-emit-key! emitter)
  "Instruct the emitter to treat the next element it receives as a key of
the current YAML mapping."
  (prim:yaml-emit-key! emitter))

(define (yaml-emit-value! emitter)
  "Instruct the emitter to treat the next element it receives as the
value of the current YAML mapping pair."
  (prim:yaml-emit-value! emitter))

(define (yaml-emit-anchor! emitter name)
  "Create an anchor for the next YAML element."
  (prim:yaml-emit-anchor! emitter name))

(define (yaml-emit-alias! emitter name)
  "Add an alias using the anchor with the given name."
  (prim:yaml-emit-alias! emitter name))

(define (yaml-set-style-1! emitter manipulator)
  "Set the style for the next YAML element.

MANIPULATOR must be a symbol.  See `yaml-manipulators' for a list of
valid manipulators."
  (prim:yaml-set-style-1! emitter manipulator))

(define (yaml-set-string-format-1! emitter manipulator)
  "Affect the style of emitted YAML strings globally.

MANIPULATOR must be a symbol.  See `yaml-manipulators' for a list of
valid manipulators."
  (prim:yaml-set-string-format-1! emitter manipulator))

(define (yaml-set-bool-format-1! emitter manipulator)
  "Affect the style of emitted YAML booleans globally.

MANIPULATOR must be a symbol.  See `yaml-manipulators' for a list of
valid manipulators."
  (prim:yaml-set-bool-format-1! emitter manipulator))

(define (yaml-set-int-base! emitter manipulator)
  "Set the numeral system for the emitted integers.

MANIPULATOR must be a symbol.  See `yaml-manipulators' for a list of
valid manipulators."
  (prim:yaml-set-int-base! emitter manipulator))

(define (yaml-set-seq-format-1! emitter manipulator)
  "Affect the style of emitted YAML sequences globally.

MANIPULATOR must be a symbol.  See `yaml-manipulators' for a list of
valid manipulators."
  (prim:yaml-set-seq-format-1! emitter manipulator))

(define (yaml-set-map-format-1! emitter manipulator)
  "Affect the style of emitted YAML mappings globally.

MANIPULATOR must be a symbol.  See `yaml-manipulators' for a list of
valid manipulators."
  (prim:yaml-set-map-format-1! emitter manipulator))

(define (yaml-set-indent! emitter length)
  "Set the number of spaces used for indentation."
  (prim:yaml-set-indent! emitter length))

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

(define* (filter-manipulators manipulators #:optional (valid #f))
  "Keep only the valid manipulators."
  (let ((valid (if valid valid yaml-manipulators)))
        (let loop ((manips manipulators)
                   (result '()))
          (if (null? manips)
              (reverse result)
              (let* ((manip (car manips))
                     (result (if (memq manip valid)
                                 (cons manip result)
                                 (begin
                                   (mention "Unknown manipulator ~S" manip)
                                   result))))
                (loop (cdr manips) result))))))

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
   (filter-manipulators manipulators yaml-string-manipulators)))

(define (yaml-set-bool-format! emitter . manipulators)
  "Set the format of the emitted booleans."
  (for-each
   (lambda (manip)
     (yaml-set-bool-format-1! emitter manip))
   (filter-manipulators manipulators yaml-bool-manipulators)))

(define (yaml-set-seq-format! emitter . manipulators)
  "Set the format of the emitted sequences."
  (for-each
   (lambda (manip)
     (yaml-set-seq-format-1! emitter manip))
   (filter-manipulators manipulators yaml-seq-manipulators)))

(define (yaml-set-map-format! emitter . manipulators)
  "Set the format of the emitted mappings."
  (for-each
   (lambda (manip)
     (yaml-set-map-format-1! emitter manip))
   (filter-manipulators manipulators yaml-map-manipulators)))
