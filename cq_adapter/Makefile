all: compile

tests: src_tests
	erl -pz ebin -pz ebin_tests -b start_sasl -noshell -s init stop -eval 'test_suite:test().'

ebin:
	@mkdir ebin

ebin_tests:
	@mkdir ebin_tests

compile: ebin
	@cd src;erl -make
	@cp src/cq_adapter.app ebin

src_tests: ebin_tests
	@cd tests;erl -make

clean:
	@echo Removing:
	@rm -rfv ebin
	@rm -rfv ebin_tests