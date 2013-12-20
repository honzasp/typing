RUSTC = rustc
SRCS = $(shell find -name '*.rs')
MAIN = main.rs
EXEC = lambda~
TEST = lambda-test~

$(EXEC): $(SRCS)
	$(RUSTC) $(MAIN) -o $@

$(TEST): $(SRCS)
	$(RUSTC) $(MAIN) -o $@ --test

.PHONY: test clean
test: $(TEST)
	RUST_THEADS=1 ./$(TEST)

clean:
	rm *~
