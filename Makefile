REBAR=./rebar

.PHONY: deps

all: deps compile

clean:
	@$(REBAR) clean
	@rm -rf deps ebin

compile:
	@echo "Running rebar compile..."
	@$(REBAR) compile

deps:
	@echo "Running rebar get-deps..."
	@$(REBAR) get-deps
	@$(REBAR) update-deps
