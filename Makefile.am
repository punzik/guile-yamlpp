ACLOCAL_AMFLAGS = -I m4
SUBDIRS = cxx scm

EXTRA_DIST = \
  README.org \
  build-aux/test-driver.scm \
  tests/test-reader.scm \
  tests/test-emitter.scm \
  tests/test.yaml

TEST_EXTENSIONS = .scm
TESTS = \
  tests/test-reader.scm \
  tests/test-emitter.scm

SCM_LOG_DRIVER = \
  $(top_builddir)/pre-inst-env \
  $(GUILE) --no-auto-compile -e main \
  $(top_srcdir)/build-aux/test-driver.scm

AM_SCM_LOG_DRIVER_FLAGS = --brief=yes
