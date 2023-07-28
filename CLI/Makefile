ECL=$(shell which ecl)

PROG = sanverter
DOTA = $(wildcard *.a)
DOTO = $(wildcard *.o)

.PHONY : compile
compile : $(PROG)

$(PROG) : build.lsp
	$(ECL) -q --norc --shell build.lsp

.PHONY : clean
clean :
	rm $(PROG) $(DOTO) $(DOTA)
