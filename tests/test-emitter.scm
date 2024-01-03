;;; tests/test-emitter.scm --- test the guile-yamlpp emitter
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

(use-modules (srfi srfi-64)
             (yamlpp))

(test-begin "test-yamlpp-emitter")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check that the emitter object is of the correct type.
(test-error "emit:type-error"
            #t
            (yaml-emit! 0 "Fe"))

;; Create a new YAML emitter.
(let ((emitter (make-yaml-emitter)))
  (test-assert "emit:make-emitter"
    (yaml-emitter-good? emitter)))

;; Check that the YAML emitter state is tracked.
(let ((emitter (make-yaml-emitter)))
  ;; Closing a sequence when none is open.
  (yaml-end-seq! emitter)
  (test-assert "emit:check-emitter-state"
    (not (yaml-emitter-good? emitter))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Null
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Write the null value.  Note that the YAML representation of the
;; null value is not necessarily the same as the Scheme one!  Nulls
;; are always written as tildes with yaml-cpp 0.6.3.
(let ((emitter (make-yaml-emitter)))
  (yaml-emit! emitter yaml-null)
  (test-equal "emit:null"
    "~"
    (yaml-emitter-string emitter)))

;; Null symbols in strings must be quoted automatically.
(let* ((emitter (make-yaml-emitter))
       (string (symbol->string yaml-null))
       (expected (string-append "\"" string "\"")))
  (yaml-emit! emitter string)
  (test-equal "emit:quote-null"
    expected
    (yaml-emitter-string emitter)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Write a string to a YAML emitter.
(let ((emitter (make-yaml-emitter)))
  (yaml-emit! emitter "Fe")
  (test-equal "emit:plain-string"
    "Fe"
    (yaml-emitter-string emitter)))

;; Check that single quotes are escaped properly.
(let ((emitter (make-yaml-emitter)))
  (yaml-emit! emitter "'Fe'")
  (test-equal "emit:auto-escape-single-quotes"
    "\"'Fe'\""
    (yaml-emitter-string emitter)))

;; Check that double quotes are escaped properly.
(let ((emitter (make-yaml-emitter)))
  (yaml-emit! emitter "\"Fe\"")
  (test-equal "emit:auto-escape-double-quotes"
    "\"\\\"Fe\\\"\""
    (yaml-emitter-string emitter)))

;; Force double quotes.
(let ((emitter (make-yaml-emitter)))
  (yaml-set-string-format! emitter 'double-quoted)
  (yaml-emit! emitter "Fe")
  (test-equal "emit:force-double-quotes"
    "\"Fe\""
    (yaml-emitter-string emitter)))

;; Force single quotes.
(let ((emitter (make-yaml-emitter)))
  (yaml-set-string-format! emitter 'single-quoted)
  (yaml-emit! emitter "Fe")
  (test-equal "emit:force-single-quotes"
    "'Fe'"
    (yaml-emitter-string emitter)))

;; Force literal style.
(let ((emitter (make-yaml-emitter)))
  (yaml-set-string-format! emitter 'literal)
  (yaml-emit! emitter "Fe\n26")
  (test-equal "emit:force-literal"
    "|\n  Fe\n  26"
    (yaml-emitter-string emitter)))

;; Single characters are also emitted as strings.
(let ((emitter (make-yaml-emitter)))
  (yaml-emit! emitter #\C)
  (test-equal "emit:character"
    "C"
    (yaml-emitter-string emitter)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Booleans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Write the true value to a YAML emitter.
(let ((emitter (make-yaml-emitter)))
  (yaml-emit! emitter #t)
  (test-equal "emit:true"
    "true"
    (yaml-emitter-string emitter)))

;; Write the false value to a YAML emitter.
(let ((emitter (make-yaml-emitter)))
  (yaml-emit! emitter #f)
  (test-equal "emit:false"
    "false"
    (yaml-emitter-string emitter)))

;; Alternative boolean style.
(let ((emitter (make-yaml-emitter)))
  (yaml-set-bool-format! emitter 'on-off-bool)
  (yaml-set-seq-format! emitter 'flow)
  (yaml-emit! emitter #(#t #f))
  (test-equal "emit:on-off-bool"
    "[on, off]"
    (yaml-emitter-string emitter)))

;; Alternative boolean style.
(let ((emitter (make-yaml-emitter)))
  (yaml-set-bool-format! emitter 'yes-no-bool)
  (yaml-set-seq-format! emitter 'flow)
  (yaml-emit! emitter #(#t #f))
  (test-equal "emit:yes-no-bool"
    "[yes, no]"
    (yaml-emitter-string emitter)))

;; Select boolean length & letter case.
(let ((emitter (make-yaml-emitter))
      (styles '(short-bool long-bool camel-case upper-case))
      (expected (string-join
                 '("- y"
                   "- true"
                   "- True"
                   "- TRUE")
                 "\n")))
  (yaml-begin-seq! emitter)
  (for-each
   (lambda (style)
     (yaml-set-style! emitter style)
     (yaml-emit! emitter #t))
   styles)
  (yaml-end-seq! emitter)
  (test-equal "emit:bool-style"
    expected
    (yaml-emitter-string emitter)))

;; Set boolean format globally.
(let ((emitter (make-yaml-emitter)))
  (yaml-set-bool-format! emitter 'on-off-bool 'upper-case)
  (yaml-emit! emitter #f)
  (test-equal "emit:bool-format"
    "OFF"
    (yaml-emitter-string emitter)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emit an integer.
(let ((emitter (make-yaml-emitter)))
  (yaml-emit! emitter 26)
  (test-equal "emit:integer"
    "26"
    (yaml-emitter-string emitter)))

;; Emit a negative integer.
(let ((emitter (make-yaml-emitter)))
  (yaml-emit! emitter -26)
  (test-equal "emit:negative-integer"
    "-26"
    (yaml-emitter-string emitter)))

;; Emit an out of range integer.
(let ((emitter (make-yaml-emitter))
      ;; We don't expect long longs to be more than 64 bits wide.
      (big-int (round-ash 1 64)))
  (test-error "emit:out-of-range-integer"
              #t
              (yaml-emit! emitter big-int)))

;; Select numeral system.
(let ((emitter (make-yaml-emitter))
      (expected "[26, 0x1a, 032]"))
  (yaml-set-seq-format! emitter 'flow)
  (yaml-begin-seq! emitter)
  (for-each
   (lambda (sys)
     (yaml-set-style! emitter sys)
     (yaml-emit! emitter 26))
   '(dec hex oct))
  (yaml-end-seq! emitter)
  (test-equal "emit:integer-base"
    expected
    (yaml-emitter-string emitter)))

;; Set numeral system globally.
(let ((emitter (make-yaml-emitter))
      (vec #(26 28))
      (expected "[0x1a, 0x1c]"))
  (yaml-set-int-base! emitter 'hex)
  (yaml-set-seq-format! emitter 'flow)
  (yaml-emit! emitter vec)
  (test-equal "emit:global-integer-base"
    expected
    (yaml-emitter-string emitter)))

;; String formatting must not affect integers.
(let ((emitter (make-yaml-emitter)))
  (yaml-set-string-format! emitter 'double-quoted)
  (yaml-emit! emitter 26)
  (test-equal "emit:string-format-/->integer"
    "26"
    (yaml-emitter-string emitter)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Real numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emit an exact (rational) real number.
(let ((emitter (make-yaml-emitter)))
  (yaml-emit! emitter 1/2)
  (test-equal "emit:rational-real"
    "0.5"
    (yaml-emitter-string emitter)))

;; Emit an inexact real number.
(let ((emitter (make-yaml-emitter)))
  (yaml-emit! emitter (* 2 (acos 0)))
  (test-equal "emit:irrational-real"
    "3.141592653589793"
    (yaml-emitter-string emitter)))

;; Emit the infinity symbol.
(let ((emitter (make-yaml-emitter)))
  (yaml-emit! emitter +inf.0)
  (test-equal "emit:infinity"
    ".inf"
    (yaml-emitter-string emitter)))

;; Emit the negative infinity symbol.
(let ((emitter (make-yaml-emitter)))
  (yaml-emit! emitter -inf.0)
  (test-equal "emit:negative-infinity"
    "-.inf"
    (yaml-emitter-string emitter)))

;; Emit the not-a-number symbol.
(let ((emitter (make-yaml-emitter)))
  (yaml-emit! emitter +nan.0)
  (test-equal "emit:nan"
    ".nan"
    (yaml-emitter-string emitter)))

;; String formatting must not affect numbers.
(let ((emitter (make-yaml-emitter))
      (vec #(26.0 +inf.0 -inf.0 +nan.0 "Fe"))
      (expected "[26.0, .inf, -.inf, .nan, \"Fe\"]"))
  (yaml-set-seq-format! emitter 'flow)
  (yaml-set-string-format! emitter 'double-quoted)
  (yaml-emit! emitter vec)
  (test-equal "emit:string-format-/->real"
    expected
    (yaml-emitter-string emitter)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emit a YAML sequence in discrete steps.
(let ((emitter (make-yaml-emitter)))
  (yaml-begin-seq! emitter)
  (yaml-emit! emitter "Fe")
  (yaml-emit! emitter "Ni")
  (yaml-end-seq! emitter)
  (test-equal "emit:progressive-seq"
    "- Fe\n- Ni"
    (yaml-emitter-string emitter)))

;; Emit a Scheme vector as a YAML sequence.
(let ((emitter (make-yaml-emitter))
      (vec #("Fe" "Ni"))
      (expected "- Fe\n- Ni"))
  (yaml-emit! emitter vec)
  (test-equal "emit:vector"
    expected
    (yaml-emitter-string emitter)))

;; Write a YAML sequence in the flow style.
(let ((emitter (make-yaml-emitter))
      (vec #("Fe" "Ni"))
      (expected "[Fe, Ni]"))
  (yaml-set-style! emitter 'flow)
  (yaml-emit! emitter vec)
  (test-equal "emit:flow-seq"
    expected
    (yaml-emitter-string emitter)))

;; Style setting should not be persistent.
(let ((emitter (make-yaml-emitter))
      (vec #("Fe" "Ni"))
      (expected (string-join
                 '("- [Fe, Ni]"
                   "-"
                   "  - Fe"
                   "  - Ni")
                 "\n")))
  (yaml-begin-seq! emitter)
  (yaml-set-style! emitter 'flow)
  (yaml-emit! emitter vec)
  (yaml-emit! emitter vec)
  (yaml-end-seq! emitter)
  (test-equal "emit:temp-seq-style"
    expected
    (yaml-emitter-string emitter)))

;; Check that the format of sequences can be set globally.
(let ((emitter (make-yaml-emitter))
      (vec #("Fe" "Ni"))
      (expected "[[Fe, Ni], [Fe, Ni]]"))
  (yaml-set-seq-format! emitter 'flow)
  (yaml-begin-seq! emitter)
  (yaml-emit! emitter vec)
  (yaml-emit! emitter vec)
  (yaml-end-seq! emitter)
  (test-equal "emit:seq-format"
    expected
    (yaml-emitter-string emitter)))

;; Style setting should affect all the children of a compound element.
(let ((emitter (make-yaml-emitter))
      (vec #(#("Fe" 26)
             #("Ni" 28)))
      (expected "[[Fe, 26], [Ni, 28]]"))
  (yaml-set-style! emitter 'flow)
  (yaml-emit! emitter vec)
  (test-equal "emit:style-seq-children"
    expected
    (yaml-emitter-string emitter)))

;; Check that sequence style can be reverted to block.
(let ((emitter (make-yaml-emitter))
      (vec #("Fe" "Ni"))
      (expected "- Fe\n- Ni"))
  (yaml-set-style! emitter 'flow)
  (yaml-set-style! emitter 'block)
  (yaml-emit! emitter vec)
  (test-equal "emit:revert-seq-style"
    expected
    (yaml-emitter-string emitter)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mappings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emit a YAML mapping in discrete steps.
(let ((emitter (make-yaml-emitter)))
  (yaml-begin-map! emitter)
  (yaml-emit-key! emitter)
  (yaml-emit! emitter "Fe")
  (yaml-emit-value! emitter)
  (yaml-emit! emitter 26)
  (yaml-emit-key! emitter)
  (yaml-emit! emitter "Ni")
  (yaml-emit-value! emitter)
  (yaml-emit! emitter 28)
  (yaml-end-map! emitter)
  (test-equal "emit:progressive-map"
    "Fe: 26\nNi: 28"
    (yaml-emitter-string emitter)))

;; Emit a Scheme association list as a YAML mapping.
(let ((emitter (make-yaml-emitter))
      (alist '(("Fe" . 26) ("Ni" . 28)))
      (expected "Fe: 26\nNi: 28"))
  (yaml-emit! emitter alist)
  (test-equal "emit:alist"
    expected
    (yaml-emitter-string emitter)))

;; Write a YAML mapping in the flow style.
(let ((emitter (make-yaml-emitter))
      (alist '(("Fe" . 26) ("Ni" . 28)))
      (expected "{Fe: 26, Ni: 28}"))
  (yaml-set-style! emitter 'flow)
  (yaml-emit! emitter alist)
  (test-equal "emit:flow-map"
    expected
    (yaml-emitter-string emitter)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complex elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mappings as elements of sequences.
(let* ((emitter (make-yaml-emitter))
       (fe '(("symbol" . "Fe") ("atomic-number" . 26)))
       (ni '(("symbol" . "Ni") ("atomic-number" . 28)))
       (elements (vector fe ni))
       (expected (string-join
                  '("- symbol: Fe"
                    "  atomic-number: 26"
                    "- symbol: Ni"
                    "  atomic-number: 28")
                  "\n")))
  (yaml-emit! emitter elements)
  (test-equal "emit:seq-as-map-value"
    expected
    (yaml-emitter-string emitter)))

;; Complex mapping keys.
(let ((emitter (make-yaml-emitter))
      (obj (list (cons #("Fe" "Ni") #(26 28))))
      (expected (string-join
                 '("? - Fe"
                   "  - Ni"
                   ": - 26"
                   "  - 28")
                 "\n")))
  (yaml-emit! emitter obj)
  (test-equal "emit:seq-as-map-key"
    expected
    (yaml-emitter-string emitter)))

;; Check that sequence and mapping formats are independent of each
;; other.
(let ((emitter (make-yaml-emitter))
      (obj (list (cons #("Fe" "Ni") #(26 28))))
      (expected "[Fe, Ni]: [26, 28]"))
  (yaml-set-seq-format! emitter 'flow)
  (yaml-emit! emitter obj)
  (test-equal "emit:seq-format-/->map"
    expected
    (yaml-emitter-string emitter)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Try to emit an object that cannot be handled.
(let ((emitter (make-yaml-emitter)))
  ;; Symbols are *not* emitted as strings.
  (test-error "emit:symbol-error"
              #t
              (yaml-emit! emitter 'Fe)))

;; Write document delimiters.
(let ((emitter (make-yaml-emitter))
      (expected (string-join
                 '("Fe"
                   "---"
                   "Ni"
                   "..."
                   "")
                 "\n")))
  (yaml-emit! emitter "Fe")
  (yaml-begin-doc! emitter)
  (yaml-emit! emitter "Ni")
  (yaml-end-doc! emitter)
  (test-equal "emit:doc-delim"
    expected
    (yaml-emitter-string emitter)))

;; Write YAML comments.
(let ((emitter (make-yaml-emitter))
      (expected (string-join
                 '("# A chemical element:"
                   "- Fe  # iron"
                   "# And here comes another:"
                   "- Ni")
                 "\n")))
  (yaml-begin-seq! emitter)
  (yaml-emit-comment! emitter "A chemical element:")
  (yaml-emit! emitter "Fe")
  (yaml-emit-comment! emitter "iron")
  (yaml-emit-newline! emitter)
  (yaml-emit-comment! emitter "And here comes another:")
  (yaml-emit! emitter "Ni")
  (yaml-end-seq! emitter)
  (test-equal "emit:comment"
    expected
    (yaml-emitter-string emitter)))

;; Use anchors and aliases.
(let ((emitter (make-yaml-emitter))
      (expected (string-join
                 '("- &iron Fe"
                   "- *iron")
                 "\n")))
  (yaml-begin-seq! emitter)
  (yaml-emit-anchor! emitter "iron")
  (yaml-emit! emitter "Fe")
  (yaml-emit-alias! emitter "iron")
  (yaml-end-seq! emitter)
  (test-equal "emit:alias"
    expected
    (yaml-emitter-string emitter)))

;; Control the indentation.
(let ((text "Fe\nNi"))
  ;; Default is two spaces.
  (let ((emitter (make-yaml-emitter))
        (expected "|\n  Fe\n  Ni"))
    (yaml-set-string-format! emitter 'literal)
    (yaml-emit! emitter text)
    (test-equal "emit:default-indent"
      expected
      (yaml-emitter-string emitter)))
  ;; Make indentation wider.
  (let ((emitter (make-yaml-emitter))
        (expected "|\n    Fe\n    Ni"))
    (yaml-set-string-format! emitter 'literal)
    (yaml-set-indent! emitter 4)
    (yaml-emit! emitter text)
    (test-equal "emit:set-indent"
      expected
      (yaml-emitter-string emitter))))

;; Use the auto manipulator to revert styles.
(let ((emitter (make-yaml-emitter))
      (expected "[Fe, 'Ni']"))
  (yaml-set-seq-format! emitter 'flow)
  (yaml-set-style! emitter 'single-quoted)
  (yaml-begin-seq! emitter)
  (yaml-set-style! emitter 'auto)
  (yaml-emit! emitter "Fe")
  (yaml-emit! emitter "Ni")
  (yaml-end-seq! emitter)
  (test-equal "emit:revert-style-with-auto"
    expected
    (yaml-emitter-string emitter)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-end)
