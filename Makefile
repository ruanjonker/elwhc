.PHONY: all deps

all: clean deps bin/elwhc.app

deps:
	./rebar get-deps

clean:
	./rebar clean

bin/elwhc.app: rebar.config src/*.erl include/*
	./rebar compile

run: all
	erl -pa ebin -sname elhwcdev@$(shell hostname -s)

#EOF
