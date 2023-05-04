UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
ARCH := elf64
endif
ifeq ($(UNAME), Darwin)
ARCH := macho64
TARGET := --target x86_64-apple-darwin
endif

run:
	cargo run $(TARGET) -- -e tests/$(T).snek $(I)

tests/%.s: tests/%.snek src/main.rs
	cargo run $(TARGET) -- $< tests/$*.s

tests/%.run: tests/%.s runtime/start.rs
	nasm -f $(ARCH) tests/$*.s -o tests/$*.o
	ar rcs tests/lib$*.a tests/$*.o
	rustc $(TARGET) -L tests/ -lour_code:$* runtime/start.rs -o tests/$*.run

.PHONY: test
test:
	cargo build
	cargo test

.PRECIOUS: tests/%.run tests/%.s
clean:
	rm -f tests/*.{a,s,run,o} || exit 0

.PHONY: % interactive generate
%: tests/%.run
	./tests/$*.run

interactive:
	cargo run $(TARGET) -- -i

generate:
	python3 tests/auto_tests.py
