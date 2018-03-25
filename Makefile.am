SUBDIRS = earley-parser

lisp_files = earley-parser.el
lisp_LISP = $(lisp_files)
include common.mk

#: Run all tests
test: check

#: Run all tests with minimum verbosity
check-short:
	$(MAKE) check 2>&1  | ruby test/make-check-filter.rb

check:
	$(MAKE) -C test $@

# #: Run "make install" non-verbosely
# install-short:
# 	$(MAKE) install 2>&1  | $(RUBY) test/make-check-filter.rb

.PHONY: test check check-short rmChangeLog
