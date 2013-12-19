RUSTC = rustc
SRCS = $(shell find -name '*.rs')
MAIN = main.rs
EXEC = lambda~

$(EXEC): $(SRCS)
	$(RUSTC) $(MAIN) -o $@

.PHONY: clean
clean:
	rm *~
