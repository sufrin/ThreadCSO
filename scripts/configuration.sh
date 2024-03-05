#  Common configuration for various scripts
#
#
echo --- Reading configuration
#
#
export JAVA_HOME=${JAVA_HOME:=$HOME/Libraryjdk-21.jdk/Contents/Home}
echo JAVA_HOME=$JAVA_HOME
LIBDIR=`pwd`
#
#
#
BUILD=$LIBDIR/BUILD-TEMP
LIBVERSION=2.0.0
SCALAVERSION=2.13
JAVAVERSION=jdk21
VERSION=$SCALAVERSION+$JAVAVERSION+$LIBVERSION
LIBJAR=$LIBDIR/threadcso-$VERSION.jar
EXAMPLESJAR=$LIBDIR/threadcso-examples-$VERSION.jar
RUNTESTS=$EXAMPLESJAR:$LIBJAR


