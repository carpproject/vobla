VOBLA (Vehicle for Optimized Basic Linear Algebra) repository

Tools
-----

 * VOBLA-to-PENCIL compiler

Coding Style
-----------
C      - please follow Linux kernel coding style [https://www.kernel.org/doc/Documentation/CodingStyle]
Scala  - please follow Scala Style Guide [ http://docs.scala-lang.org/style/scaladoc.htm]


Repository Contents
-------------------

 * etc/             Misc. files, e.g. Vim syntax files
 * grammar/         ANTLR3 Grammars for VOBLA
 * src/             Scala source code:
   * .../apps/        Tool-specific source code
   * .../frontends/   VOBLA frontend code
 * testsuite/       Test files:
   * blastest/        BLAS Library
   * code/            Unit tests
   * lib/             Testing infrastructure files


Building process
----------------
Summary:

autoconf
./configure --with-scala=$SCALA_HOME --with-antlr3=$ANTLR_HOME
make

For more information on what $SCALA_HOME and $ANTLR_HOME should point to,
refer to 'Build requirements' below.

If a pencil/ subdirectory exists, the configure step will automatically
run configure in that directory as well with the same arguments.


Build requirements
------------------
 * Antlr3 is installed and ANTLR_HOME is set:
    % ls $ANTLR_HOME
    ant-antlr3.jar  antlr3.jar
 * Scala tools are installed and SCALA_HOME is set:
    % echo $SCALA_HOME
    /usr/local/share/scala/
     % ls $SCALA_HOME/lib -1
     akka-actors.jar
     jline.jar
     scala-actors.jar
     scala-actors-migration.jar
     scala-compiler.jar
     scala-library.jar
     scala-partest.jar
     scalap.jar
     scala-reflect.jar
     scala-swing.jar
     typesafe-config.jar
  * Antlr3 tools are accessible via CLASSPATH:
    export CLASSPATH=$ANTLR_HOME/ant-antlr3.jar:$ANTLR_HOME/antlr3.jar
  * PENCIL core repository is cloned (as submodule, for example) to ./pencil directory.
    % ls
    build.xml  docs/  etc/  grammar/  Makefile  pencil  README  src/  testsuite/  vobla.doxygen
  * For BLAS testing BLAS testsuite must be downloaded:
    make -C testsuite/blastest/ blastestfiles

This build has been tested on the following configuration:
 % scalac -version
 Scala compiler version 2.10.1 -- Copyright 2002-2013, LAMP/EPFL
 % ant -version
 Apache Ant version 1.7.1 compiled on September 8 2010
 % java -jar $ANTLR_HOME/antlr3.jar -version
 ANTLR Parser Generator  Version 3.5

Running tools
-------------
Vobla compiler: vobla <files>
The tool supports -h for help
