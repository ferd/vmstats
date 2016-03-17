REBAR ?= rebar

.PHONY: all test clean edoc compile

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) eunit skip_deps=true
