AC_INIT(elisp-elisp-earley, 1.0.0,)
AC_CONFIG_SRCDIR(earley-parser.el)
AM_INIT_AUTOMAKE([foreign])
AM_MAINTAINER_MODE

AC_PATH_PROG([EMACS], [emacs], [emacs])
AC_ARG_WITH(emacs, AC_HELP_STRING([--with-emacs],
                  [location of emacs program]), EMACS=$withval)

AC_MSG_NOTICE("Checking emacs version")
$EMACS -batch -q --no-site-file -eval \
  '(if (<= emacs-major-version 23)
       (progn
         (error "You need GNU Emacs 24 or better.")
         (kill-emacs 1)
       )
   )'
if test $? -ne 0 ; then
    AC_MSG_ERROR([Can't continue until above error is corrected.])
fi

if test "$NO_CHECK_EMACS_PACKAGES" = "" ; then
AC_MSG_NOTICE("Checking prerequiste packages")
$EMACS -batch -q --no-site-file -eval \
  '(dolist (package
         (quote (cl-lib load-relative test-simple)))
        (require package))
   )'
fi
if test $? -ne 0 ; then
    AC_MSG_ERROR([Can't continue until above error is corrected.])
fi

##################################################################
# See if --with-lispdir was set. If not, set it to a reasonable default
# based on where bash thinks bashdb is supposed to be installed.
##################################################################

AM_MISSING_PROG(GIT2CL, git2cl, $missing_dir)

# Check whether --with-lispdir was given.
if test "${with_lispdir+set}" = set; then :
else
  my_lispdir="${datadir}/emacs/site-lisp"
  if test "${my_lispdir+set}" = set; then :
    with_lispdir=$my_lispdir
  fi
fi

##
## Find out where to install the emacs lisp files
##
AM_PATH_LISPDIR
lispdir_earley_parser=$lispdir/
AC_SUBST([lispdir])
AC_SUBST([lispdir_earley_parser])

AM_CONDITIONAL(INSTALL_EMACS_LISP, test "x$lispdir_earley_parser" != "x")

AC_CONFIG_FILES([Makefile \
		 common.mk \
		 earley-parser/Makefile \
		 test/Makefile \
		 ])
AC_OUTPUT
