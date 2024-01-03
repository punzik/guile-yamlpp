;;; tests/test-reader.scm --- test the guile-yamlpp reader
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

(test-begin "test-yamlpp-reader")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Unquoted string.
(test-equal "read:plain-string"
  "Fe"
  (yaml-load "Fe"))

;; This number is quoted, so it must be parsed as a string.
(test-equal "read:single-quoted-string"
  "26"
  (yaml-load "\"26\""))

;; Single quotes should work too.
(test-equal "read:double-quoted-string"
    "26" (yaml-load "'26'"))

;; Using an explicit string tag.
(test-equal "read:explicit-string-tag"
  "26"
  (yaml-load "!!str 26"))

;; Using a verbatim string tag.
(test-equal "read:verbatim-string-tag"
  "26"
  (yaml-load "!<tag:yaml.org,2002:str> 26"))

;; TODO: Fails with yaml-cpp 0.6.3!
(test-skip 1)
(test-eq "read:empty-tagged-string"
  yaml-null
  (yaml-load "!!str"))

;; Timestamps are not supported.
(test-equal "read:timestamp-not"
  "2023-12-09"
  (yaml-load "2023-12-09"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Positive integer.
(test-equal "read:integer"
  26
  (yaml-load "26"))

;; Positive integer with explicit plus sign.
(test-equal "read:plus-sign-integer"
  26
  (yaml-load "+26"))

;; Negative integer.
(test-equal "read:negative-integer"
  -26
  (yaml-load "-26"))

;; Explicit tag must override quotes.
(test-equal "read:explicit-integer-tag"
  26
  (yaml-load "!!int '26'"))

;; Verbatim tag must override quotes.
(test-equal "read:verbatim-integer-tag"
  26
  (yaml-load "!<tag:yaml.org,2002:int> '26'"))

;; Hexadecimal numbers are supported.
(test-equal "read:hex-integer"
  10
  (yaml-load "0xA"))

;; Octal numbers are *not* supported.
(test-equal "read:oct-integer-not"
  "0o11"
  (yaml-load "0o11"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Real numbers and beyond
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Decimal notation.
(test-equal "read:decimal-real"
  26.0
  (yaml-load "26.0"))

;; Exponential notation.
(test-equal "read:exponential-real"
  0.26
  (yaml-load "26.0e-2"))

;; Negative zero.
(test-assert "read:negative-zero"
  (and (equal? -0.0 (yaml-load "-0.0"))
       (not (equal? 0.0 (yaml-load "-0.0")))))

;; Infinities.
(test-equal "read:infinity"
  #(+inf.0 -inf.0)
  (yaml-load "[.inf, -.inf]"))

;; Not a number.
(test-equal "read:nan"
  +nan.0
  (yaml-load ".nan"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Booleans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Standard boolean values.
(test-equal "read:bool"
  #(#t #f)
  (yaml-load "[true, false]"))

;; Upper case & capitalized booleans must be accepted.
(test-equal "read:cap-bool"
  #(#t #f)
  (yaml-load "[True, FALSE]"))

;; Alternative boolean notations.
(test-equal "read:alt-bool"
  (vector #(#t #f)
          #(#t #f)
          #(#t #f))
  (yaml-load "[[yes, no], [y, n], [on, off]]"))

;; Boolean shorthand.
(test-equal "read:short-bool"
  #(#t #f)
  (yaml-load "[y, n]"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Null
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The tilde represents the null value in YAML.
(test-eq "read:tilde-null"
  yaml-null
  (yaml-load "~"))

;; The `null' string must yield the null symbol.
(test-eq "read:null"
  yaml-null
  (yaml-load "null"))

;; Capitalization is permitted.
(test-eq "read:cap-null"
  yaml-null
  (yaml-load "Null"))

;; An empty document results in null.
(test-eq "read:empty-doc"
  yaml-null
  (yaml-load ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Flow style, one line.
(test-equal "read:flow-seq"
  #("Fe" "Ni")
  (yaml-load "[Fe, Ni]"))

;; Flow style, multiple lines.
(test-equal "read:multiline-flow-seq"
  #("Fe" "Ni")
  (yaml-load
   (string-join
    (list "[Fe,"
          " Ni]")
    "\n")))

;; Block style.
(test-equal "read:block-seq"
  #("Fe" "Ni")
  (yaml-load
   (string-join
    (list "- Fe"
          "- Ni")
    "\n")))

;; Block style, another way.
(test-equal "read:sparse-block-seq"
  #("Fe" "Ni")
  (yaml-load
   (string-join
    (list "-"
          "  Fe"
          "-"
          "  Ni")
    "\n")))

;; Omitted values result in nulls.
(test-equal "read:seq-omitted-values"
  (vector "Fe" yaml-null "Ni")
  (yaml-load "[Fe, , Ni]"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mappings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Flow style, one line.
(test-equal "read:flow-map"
  '(("Fe" . 26) ("Ni" . 28))
  (yaml-load "{Fe: 26, Ni: 28}"))

;; Flow style, multiple lines.
(test-equal "read:multiline-flow-map"
  '(("Fe" . 26) ("Ni" . 28))
  (yaml-load
   (string-join
    (list "{Fe: 26,"
          " Ni: 28}")
    "\n")))

;; Block style.
(test-equal "read:block-map"
  '(("Fe" . 26) ("Ni" . 28))
  (yaml-load
   (string-join
    (list "Fe: 26"
          "Ni: 28")
    "\n")))

;; Omitted values result in nulls.
(test-equal "read:map-omitted-values"
  `(("Fe" . ,yaml-null))
  (yaml-load "Fe:"))

;; Complex keys.
(test-equal "read:seq-map-key"
  '((#("Fe" "Ni") . #(26 28)))
  (yaml-load
   (string-join
    (list "? - Fe"
          "  - Ni"
          ": [26, 28]")
    "\n")))

;; (Un)ordered set.  Values should be null.  Note that the YAML
;; specification defines an unordered set type, but yaml-cpp preserves
;; the order anyway.
(test-equal "read:set"
  `(("Fe" . ,yaml-null) ("Ni" . ,yaml-null))
  (yaml-load
   (string-join
    (list "!!set"
          "? Fe"
          "? Ni")
    "\n")))

;; Ordered mappings are not supported by yaml-cpp.  This should result
;; in an association list but is parsed as a vector of alists instead.
(test-equal "read:ordered-map-not"
  '#((("Fe" . 26)) (("Ni" . 28)))
  (yaml-load
   (string-join
    (list "!!omap"
          "- Fe: 26"
          "- Ni: 28")
    "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quoting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Escapes do not work with the plain style.
(test-equal "read:plain-style-no-escape"
  "Fe\\nNi"
;;   ^^^
  (yaml-load "Fe\\nNi"))
;;              ^^^

;; Escapes *do* work inside double quotes.
(test-equal "read:double-quote-escape"
  "Fe\nNi"
;;   ^^
  (yaml-load "\"Fe\\nNi\""))
;;                ^^^

;; Backslashes and double quotes must be escaped within double quotes.
(test-equal "read:double-quote-must-escape"
  " \\ \" "
;;  ^^ ^^
  (yaml-load "\" \\\\ \\\" \""))
;;               ^^^^ ^^^^

;; Using single quotes in YAML to avoid escapes.
(test-equal "read:single-quote-no-escape"
  " \\ \" "
;;  ^^ ^^
  (yaml-load "' \\ \" '"))
;;              ^^ ^^

;; Double quotes allow unicode escape sequences.
(test-equal "read:unicode-escape"
  "☺"
  (yaml-load "\"\\u263A\""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Whitespace must be discarded around content.
(test-equal "read:discard-whitespace"
  26
  (yaml-load " 26 \n "))

;; Folding must convert newlines to spaces.  The default chomping
;; behavior is `clip', so the first newline is preserved, while
;; additional blank lines are discarded.
(test-equal "read:fold-clip"
  "Fe Ni\n"
  (yaml-load
   (string-join
    (list ">"
          "  Fe"
          "  Ni"
          ""
          ""
          "")
    "\n")))

;; The `strip' chomping indicator must discard the trailing newline.
(test-equal "read:fold-strip"
  "Fe Ni"
  (yaml-load
   (string-join
    (list ">-"
          "  Fe"
          "  Ni"
          ""
          ""
          "")
    "\n")))

;; The `keep' chomping indicator must preserve the empty lines.
(test-equal "read:fold-keep"
  "Fe Ni\n\n"
  (yaml-load
   (string-join
    (list ">+"
          "  Fe"
          "  Ni"
          ""
          ""
          "")
    "\n")))

;; Literal style is commonly used with the `strip' indicator.
;; Trailing lines must be discarded.
(test-equal "read:literal-strip"
  "\nFe\nNi"
  (yaml-load
   (string-join
    (list "|-"
          ""
          "  Fe"
          "  Ni"
          ""
          ""
          "")
    "\n")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Comments should be discarded.
(test-equal "read:comment"
  "Fe"
  (yaml-load
   (string-join
    (list "# Ignore this."
          "Fe # And this.")
    "\n")))

;; Quotes should suppress the commenting mechanism.
(test-equal "read:hash-in-string"
  "# Fe"
  (yaml-load "'# Fe'"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for serial documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal "read:multi-doc"
  '("Fe" "Ni")
  (yaml-load-all
   (string-join
    (list "---"
          "Fe"
          "---"
          "Ni"
          "...")
    "\n")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading documents from the file system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((dir (getenv "GUILE_YAMLPP_TEST_DATA_PATH"))
       (filename (string-append dir "/test.yaml")))
  ;; Load just the first document.
  (test-equal "read:file"
    "Fe"
    (yaml-load-file filename))
  ;; Load all the documents.
  (test-equal "read:multi-doc-file"
    '("Fe" "Ni" "Zn")
    (yaml-load-all-from-file filename)))

;; File system errors must be caught.
(test-error "read:catch-file-error"
            'yaml-file-error
            (yaml-load-file "does.not.exist"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Anchors and aliases should be recognized.
(test-equal "read:alias"
  #("Fe" "Fe")
  (yaml-load "[&elt Fe, *elt]"))

;; Parser errors should be caught.
(test-error "read:catch-parse-error"
            'yaml-parser-error
            (yaml-load "!@#$%"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-end)
