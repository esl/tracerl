.PHONY: all compile test clean

all: compile

compile: rebar
	./rebar get-deps compile

test: rebar compile
	./rebar skip_deps=true eunit

ct: rebar compile
	./rebar skip_deps=true ct -v

clean: rebar
	./rebar clean

rebar:
	git clone https://github.com/basho/rebar.git
	cd rebar && ./bootstrap
	mv rebar/rebar rebar.tmp
	rm -rf rebar
	mv rebar.tmp rebar
