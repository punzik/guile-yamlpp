(use-modules
 ;;
 (guix packages)
 (guix gexp)
 (guix utils)
 (guix build-system gnu)
 ;;
 ((guix licenses) #:prefix license:)
 ;;
 (gnu packages autotools)
 (gnu packages pkg-config)
 (gnu packages serialization)
 (gnu packages guile))

(package
  (name "guile-yamlpp")
  (version "0.1")
  (source (local-file (string-append (current-source-directory)
		                     "/" name "-" version ".tar.gz")))
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
  (synopsis "Guile YAML reader/writer based on @code{yaml-cpp}")
  (description
   "A module for GNU Guile to read and write YAML files.  It works using
bindings to the @code{yaml-cpp} C++ library.")
  (home-page "https://gitlab.com/yorgath/guile-yamlpp")
  (license license:gpl3+))
