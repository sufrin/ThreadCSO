#  Common configuration for various scripts
#
#
echo --- Reading configuration
#
#
export JAVA_HOME=${JAVA_HOME:=$HOME/Downloads/jdk-20.jdk/Contents/Home}
echo JAVA_HOME=$JAVA_HOME
#
#
#
BUILD=BUILD-TEMP
LIBVERSION=1.3
SCALAVERSION=2.13
JAVAVERSION=jdk20
VERSION=$SCALAVERSION+$JAVAVERSION+$LIBVERSION
LIBJAR=threadcso-$VERSION.jar
EXAMPLES=./examples/target/scala-2.13/examples_2.13-1.3.1.jar
MANUALTESTS=./manualtests/target/scala-2.13/manualtests_2.13-1.3.jar
RUNTESTS=$MANUALTESTS:$EXAMPLES:$LIBJAR


