
.PHONY: all test-parser

all: dist/build/parser/parser

test-parser: parser_tests/regular/parser parser_tests/fuzzying/parser
	cd parser_tests/regular; ./run_test
	cd parser_tests/fuzzying; ./run_test

dist/build/parser/parser: src/Parser.hs src/ParserMain.hs
	runhaskell Setup configure
	runhaskell Setup build

parser_tests/regular/parser: dist/build/parser/parser
	cp $< $@

parser_tests/fuzzying/parser: dist/build/parser/parser
	cp $< $@
