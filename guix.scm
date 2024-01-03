(use-modules
 ;;
 (guix packages)
 (guix gexp)
 (guix utils)
 (guix git-download)
 (guix build-system gnu)
 ;;
 ((guix licenses) #:prefix license:)
 ;;
 (gnu packages autotools)
 (gnu packages pkg-config)
 (gnu packages serialization)
 (gnu packages guile))

;; Copied verbatim from the Guix cookbook.
(define vcs-file?
  ;; Return true if the given file is under version control.
  (or (git-predicate (current-source-directory))
      (const #t)))                                ;not in a Git checkout

(package
  (name "guile-yamlpp")
  (version "dev")
  (source (local-file "." "guile-yamlpp-dev-checkout"
                      #:recursive? #t
                      #:select? vcs-file?))
  (build-system gnu-build-system)
  (native-inputs (list autoconf
                       automake
                       libtool
                       pkg-config))
  (inputs (list guile-3.0
                yaml-cpp))
  (native-search-paths (list (search-path-specification
                              (variable "GUILE_EXTENSIONS_PATH")
                              (files (list "lib/guile/3.0")))))
  (home-page "https://gitlab.com/yorgath/guile-yamlpp")
  (synopsis "Guile YAML reader/writer based on @code{yaml-cpp}")
  (description
   "A module for GNU Guile to read and write YAML files.  It works using
bindings to the @code{yaml-cpp} C++ library.")
  (license license:gpl3+))
