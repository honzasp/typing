GHC_MAKE = ghc --make -Wall -fno-warn-unused-matches -fno-warn-name-shadowing
MAIN = Main.hs
EXEC = lambda~
SOURCES = $(shell find . -name '*.hs')

$(EXEC): $(SOURCES)
	$(GHC_MAKE) $(MAIN) -o $@

.PHONY: clean
clean:
	rm -f *.hi *.o $(EXEC)

