
.PHONY: all test-parser test-semchecker

all: dist/build/parser/parser dist/build/semchecker/semchecker

test-parser: parser_tests/regular/parser parser_tests/fuzzying/parser
	cd parser_tests/regular; ./run_test
	cd parser_tests/fuzzying; ./run_test

test-semchecker: parser_tests/semchecker
	cd parser_tests; ./do_semcheck

dist/build/parser/parser: src/Parser.hs src/ParserMain.hs
	runhaskell Setup configure
	runhaskell Setup build

dist/build/semchecker/semchecker: src/Parser.hs src/Semantic.hs src/SemanticMain.hs
	runhaskell Setup configure
	runhaskell Setup build

parser_tests/regular/parser: dist/build/parser/parser
	cp $< $@

parser_tests/fuzzying/parser: dist/build/parser/parser
	cp $< $@

parser_tests/semchecker: dist/build/semchecker/semchecker
	cp $< $@
