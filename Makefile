.PHONY: all deps clean test analyze

all: clean deps ebin/elwhc.app test

deps: rebar rebar.config
	@./rebar get-deps

clean:
	rm -f erl_crash.dump
	@./rebar clean

ebin/elwhc.app: rebar.config src/*.erl include/*
	@./rebar compile

test: ebin/elwhc.app test/*
	./rebar eunit skip_deps=true

.plt:
	@dialyzer --build_plt --statistics --output_plt .plt --apps erts kernel stdlib asn1 crypto public_key ssl

analyze: ebin/elwhc.app .plt
	@dialyzer ebin/ --statistics --plt .plt --fullpath --verbose -Wno_unused

#EOF
