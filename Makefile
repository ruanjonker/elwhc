.PHONY: all deps clean

all: clean deps ebin/elwhc.app

deps:
	./rebar get-deps

clean:
	rm -f erl_crash.dump
	./rebar clean

ebin/elwhc.app: rebar.config src/*.erl include/*
	./rebar compile

run: clean deps ebin/elwhc.app
	erl -pa ebin -sname elhwcdev@$(shell hostname -s) -s elwhc_app start_dev

#EOF
