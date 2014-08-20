.PHONY: all deps clean test

all: clean deps ebin/elwhc.app test

deps:
	./rebar get-deps

clean:
	rm -f erl_crash.dump
	./rebar clean

ebin/elwhc.app: rebar.config src/*.erl include/*
	./rebar compile

test: ebin/elwhc.app test/*
	./rebar eunit skip_deps=true

run: clean deps ebin/elwhc.app
	erl -pa ebin -sname elhwcdev@$(shell hostname -s) -s elwhc_app start_dev

#EOF
