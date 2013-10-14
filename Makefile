
REBAR = ./rebar

all: repl

clean:
	$(REBAR) clean

repl: compile
	erl `find . -iname 'ebin' -type d -exec echo '-pa ' '{}' ';'`

get-deps:
	$(REBAR) get-deps

compile: get-deps
	$(REBAR) compile



