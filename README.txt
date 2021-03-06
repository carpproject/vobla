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
./configure --with-scala=$SCALA_HOME --with-antlr3=$ANTLR_HOME --with-pencil=$PENCIL_HOME/pencil.jar --with-pencil-linker=$PENCIL_HOME/linker --with-pencil-optimizer=$PENCIL_HOME/optimizer
make

For more information on what $SCALA_HOME, $ANTLR_HOME and $PENCIL_HOME should
point to, refer to 'Build requirements' below.

Build requirements
------------------
 * Antlr3 is installed and ANTLR_HOME is set:
    % ANTLR_HOME=/opt/antlr # Or any other
    % mkdir -p $ANTLR_HOME
    % wget http://www.antlr3.org/download/antlr-3.5.2-complete-no-st3.jar -O $ANTLR_HOME/antlr3.jar
    % wget http://www.antlr3.org/share/1169924912745/antlr3-task.zip -O /tmp/antlr3-task.zip
    % unzip /tmp/antlr3-task.zip -d /tmp/antlr3-task
    % cp /tmp/antlr3-task/antlr3-task/ant-antlr3.jar $ANTLR_HOME/ant-antlr3.jar
    % rm -rf /tmp/antlr3-task /tmp/antlr3-task.zip
    % ls $ANTLR_HOME
    ant-antlr3.jar  antlr3.jar
 * Scala tools are installed and SCALA_HOME is set:
    % SCALA_HOME=/opt/scala # Or any other
    % mkdir -p $SCALA_HOME
    % wget http://www.scala-lang.org/files/archive/scala-2.10.4.tgz
    % tar xf scala-2.10.4.tgz --strip-component=1 -C $SCALA_HOME
    % rm scala-2.10.4.tgz
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
    % export CLASSPATH=$ANTLR_HOME/ant-antlr3.jar:$ANTLR_HOME/antlr3.jar
  * For BLAS testing BLAS testsuite must be downloaded:
    make -C testsuite/blastest/ blastestfiles
  * PENCIL library is installed:
    % git clone git@github.com:carpproject/pencil.git $PENCIL_HOME
    Build the library by following instructions in the README.txt

This build has been tested on the following configuration:
 % scalac -version
 Scala compiler version 2.10.4 -- Copyright 2002-2013, LAMP/EPFL
 % ant -version
 Apache Ant version 1.7.1 compiled on September 8 2010
 % java -jar $ANTLR_HOME/antlr3.jar -version
 ANTLR Parser Generator  Version 3.5.2

Running tools
-------------
Vobla compiler: vobla <files>
The tool supports -h for help

Known issues
--------------
Both VOBLA and PENCIL require each function to have exactly one return
statement for non-void functions, or zero or one for void functions.
The return statement must be the last statement of the function.

Currently the VOBLA compiler doesn't check this, so having multiple returns
would result in the PENCIL back-end throwing an error (instead of a nice error
message from the front-end).
