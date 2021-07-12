all: .compiled

SOURCE = $(wildcard src/*.scala)
CP     = lib/threadcso.jar:lib/jsoup.jar

.compiled: $(SOURCE);\
        mkdir -p bin;\
        $(SCALAC) -cp $(CP) -d bin $(SOURCE);\
        touch $@

# Regression tests
REGRESS = $(wildcard test/test*)

regress: force;\
        for t in $(REGRESS); do ./runtest $$t; done

TEST = $(wildcard test/Q_*.scala)

test: force;\
        for t in $(TEST); do scala -cp bin:../lib/scalacheck.jar $$t; done

clean: force;\
        rm -rf .compiled bin
        
force:

SCALAC = scalac


