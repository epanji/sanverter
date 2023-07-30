ECL=$(shell which ecl)

PROG = sanverter
DOTA = $(wildcard *.a)
DOTO = $(wildcard *.o)
SLCK = sanverter-*.txz

.PHONY : compile
compile : $(PROG)

$(PROG) : build.lsp
	$(ECL) -q --norc --shell build.lsp

.PHONY : slackbuild
slackbuild : $(SLCK)
$(SLCK) : slackbuild.sh
	sh slackbuild.sh

.PHONY : clean
clean :
	rm -rf SBo $(DOTO) $(DOTA)

.PHONY : clean-all
clean-all : clean
	rm $(PROG) $(SLCK)
