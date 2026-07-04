# sml-xlsx build (pure Office Open XML spreadsheet reader/writer)
#
#   make            build the test binary with MLton (default)
#   make test       build + run tests under MLton
#   make test-poly  run tests under Poly/ML (use-and-run; no link step)
#   make all-tests  run the suite under both compilers
#   make example    build + run the demo
#   make clean      remove build artifacts
#
# Layout B (dependent): own sources live in src/; sml-zip (which itself vendors
# sml-deflate + sml-codec) and sml-xml (which vendors sml-unicode) are vendored
# under lib/ and loaded first, in dependency order.

MLTON      ?= mlton
POLY       ?= poly
BIN        := bin
CODECDIR   := lib/github.com/sjqtentacles/sml-codec
DEFDIR     := lib/github.com/sjqtentacles/sml-deflate
ZIPDIR     := lib/github.com/sjqtentacles/sml-zip
UNIDIR     := lib/github.com/sjqtentacles/sml-unicode
XMLDIR     := lib/github.com/sjqtentacles/sml-xml
TEST_MLB   := test/test.mlb
SRCS       := $(wildcard $(CODECDIR)/* $(DEFDIR)/* $(ZIPDIR)/* $(UNIDIR)/* $(XMLDIR)/* src/* test/*.sml) $(TEST_MLB)

.PHONY: all test poly test-poly all-tests example clean

all: $(BIN)/test-mlton

example: $(BIN)/demo
	./$(BIN)/demo

$(BIN)/demo: $(SRCS) examples/demo.sml examples/sources.mlb | $(BIN)
	$(MLTON) -output $@ examples/sources.mlb

$(BIN)/test-mlton: $(SRCS) | $(BIN)
	$(MLTON) -output $@ $(TEST_MLB)

test: $(BIN)/test-mlton
	$(BIN)/test-mlton

# Poly/ML has no native .mlb support; the suite runs at top level and exits on
# its own. Load the vendored CRC-32 + inflate + deflate + zip, then unicode +
# xml, then the xlsx sources, then the test driver -- all in dependency order.
poly test-poly:
	printf 'use "$(CODECDIR)/crc32.sig";\nuse "$(CODECDIR)/crc32.sml";\nuse "$(DEFDIR)/inflate.sig";\nuse "$(DEFDIR)/inflate.sml";\nuse "$(DEFDIR)/deflate.sig";\nuse "$(DEFDIR)/deflate.sml";\nuse "$(ZIPDIR)/zip.sig";\nuse "$(ZIPDIR)/zip.sml";\nuse "$(UNIDIR)/data.sml";\nuse "$(UNIDIR)/unicode.sig";\nuse "$(UNIDIR)/unicode.sml";\nuse "$(XMLDIR)/xml.sig";\nuse "$(XMLDIR)/xml.sml";\nuse "src/xlsx.sig";\nuse "src/xlsx.sml";\nuse "test/harness.sml";\nuse "test/support.sml";\nuse "test/sample.sml";\nuse "test/test_golden.sml";\nuse "test/test_roundtrip.sml";\nuse "test/test_read.sml";\nuse "test/test_malformed.sml";\nuse "test/entry.sml";\nuse "test/main.sml";\n' | $(POLY) -q --error-exit

all-tests: test test-poly

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -f $(BIN)/test-mlton $(BIN)/demo
