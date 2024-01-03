guileextdir = $(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)
guileext_LTLIBRARIES = libguile-yamlpp.la

libguile_yamlpp_la_SOURCES = \
  macros.h \
  util.cxx \
  util.h \
  reader.cxx \
  reader.h \
  emitter.cxx \
  emitter.h \
  init.cxx

libguile_yamlpp_la_CXXFLAGS = @YAML_CFLAGS@ @GUILE_CFLAGS@ -Wall
libguile_yamlpp_la_LDFLAGS = @YAML_LIBS@ @GUILE_LIBS@
