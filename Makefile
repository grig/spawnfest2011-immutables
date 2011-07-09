all: compile

compile:
	./rebar compile skip_deps=true

.PHONY: all compile
