# Copyright (c) 2013-2014, ARM Limited
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE

AC_INIT([vobla], [0.01], [carp@arm.com])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])

dnl Scala installation
AC_SUBST(SCALA_HOME)
AC_SUBST(SCALA_RUNTIME)
AC_ARG_WITH([scala],
  [AS_HELP_STRING([--with-scala=dir],[directory containing scala])],
  [
   SCALA_HOME="$with_scala"
   SCALA_RUNTIME="$with_scala"/bin/scala
  ],
  [
   SCALA_HOME="/usr/share/scala"
   SCALA_RUNTIME=scala
  ]
)

AC_CHECK_FILE(["$SCALA_HOME/lib/scala-compiler.jar"],[],[AC_MSG_ERROR([could not find file])])
AC_CHECK_FILE(["$SCALA_HOME/lib/scala-library.jar"],[],[AC_MSG_ERROR([could not find file])])
AC_CHECK_FILE(["$SCALA_HOME/lib/scala-reflect.jar"],[],[AC_MSG_ERROR([could not find file])])

dnl ANTLR3 jar
AC_SUBST(ANTLR_HOME)
AC_ARG_WITH([antlr3],
  [AS_HELP_STRING([--with-antlr3=dir],[directory containing ANTLR3 jars])],
  [ANTLR_HOME="$with_antlr3"],
  [AC_MSG_ERROR([please specify path to ANTLR3 jars using --with-antlr3])]
)
AC_CHECK_FILE(["$ANTLR_HOME/antlr3.jar"],[],[AC_MSG_ERROR([could not find file])])

dnl pencil util dir
AC_SUBST(PENCIL_UTIL)
AC_ARG_WITH([pencil-util],
  [AS_HELP_STRING([--with-pencil-util=dir],[directory containing pencil-util])],
  [PENCIL_UTIL=`readlink -f "$with_pencil_util"]`,
  [
   AC_MSG_WARN([pencil-util directory not found, BLAS testsuite will not be run.])
   PENCIL_UTIL=""
  ]
)
if test "x$PENCIL_UTIL" != "x"
then
    AC_CHECK_FILE(["$PENCIL_UTIL/include/pencil.h"],[],[AC_MSG_ERROR([could not find file])])
fi

dnl ant-antlr3 jar
AC_SUBST(ANT_ANTLR_HOME)
AC_ARG_WITH([ant-antlr3],
  [AS_HELP_STRING([--with-ant-antlr3=dir],[directory containing ant-antlr3 jar])],
  [ANT_ANTLR_HOME="$with_ant_antlr3"],
  [ANT_ANTLR_HOME="$ANTLR_HOME"]
)
AC_CHECK_FILE(["$ANT_ANTLR_HOME/ant-antlr3.jar"],[],[AC_MSG_ERROR([could not find file])])

dnl PENCIL lib jar
AC_SUBST(PENCIL_JAR)
AC_ARG_WITH([pencil],
  [AS_HELP_STRING([--with-pencil=file],[pencil.jar file])],
  [PENCIL_JAR="$with_pencil"],
  [PENCIL_JAR="/usr/local/share/pencil/pencil.jar"]
)
PENCIL_JAR=`readlink -f $PENCIL_JAR`
AC_CHECK_FILE(["$PENCIL_JAR"],[],[AC_MSG_ERROR([could not find pencil.jar])])

dnl Use PENCIL linker provided via --with-... flag, or fall back to first one in PATH.
OLDPATH=$PATH
AC_SUBST(PENCIL_LINKER)
AC_ARG_WITH([pencil-linker],
  [AS_HELP_STRING([--with-pencil-linker=program],[pencil-linker to use])],
  [
    abspath="`readlink -f $with_pencil_linker`"
    PATH="`dirname $abspath`:$PATH"
    PENCIL_LINKER_NAME="`basename $with_pencil_linker`"
  ],
  [PENCIL_LINKER_NAME="pencil-linker"]
)
AC_PATH_PROG([PENCIL_LINKER], $PENCIL_LINKER_NAME)
AS_IF([test ! -x "$PENCIL_LINKER"], [AC_MSG_ERROR([could not find pencil-linker])])
PATH=$OLDPATH

dnl Use PENCIL optimizer provided via --with-... flag, or fall back to first one in PATH.
OLDPATH=$PATH
AC_SUBST(PENCIL_OPTIMIZER)
AC_ARG_WITH([pencil-optimizer],
  [AS_HELP_STRING([--with-pencil-optimizer=program],[pencil-optimizer to use])],
  [
    abspath="`readlink -f $with_pencil_optimizer`"
    PATH="`dirname $abspath`:$PATH"
    PENCIL_OPTIMIZER_NAME="`basename $with_pencil_optimizer`"
  ],
  [PENCIL_OPTIMIZER_NAME="pencil-optimizer"]
)
AC_PATH_PROG([PENCIL_OPTIMIZER], $PENCIL_OPTIMIZER_NAME)
AS_IF([test ! -x "$PENCIL_OPTIMIZER"], [AC_MSG_ERROR([could not find pencil-optimizer])])
PATH=$OLDPATH

AC_CONFIG_FILES([vobla], [chmod +x vobla])
AC_CONFIG_FILES([global-vobla], [chmod +x global-vobla])
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([testsuite/blastest/Makefile])
AC_OUTPUT
