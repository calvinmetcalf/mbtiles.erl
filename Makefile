ERL          ?= erl
ERLC		     ?= erlc
APP          := mbtiles

.PHONY: deps doc

all: deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

doc:
	@rebar doc	
	
clean: 
	@./rebar clean
	@rm -f t/*.beam
	@rm -f doc/*.html doc/*.css doc/edoc-info doc/*.png

distclean: clean
	@./rebar delete-deps
	@rm -rf deps

dialyzer: compile
	@dialyzer -Wno_return -c ebin

