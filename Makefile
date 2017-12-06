REBAR = ./rebar3

DEPS_PATH = ./_build/default/lib/*/ebin

ERL_LIBS= ./lib

CT_OPTS = -cover test/cover.spec -erl_args -config test/test.config
CT_SUITES = bidrequest_parse_SUITE	


.PHONY: all compile clean distclean dialyze tests

all: compile

compile:
	$(REBAR) compile skip-deps=true

clean:
	rm -rf ebin/* test/*.beam logs log
	$(REBAR) clean

distclean: clean
	rm -rf _build priv/*.so logs log

dialyze:
	$(REBAR) dialyzer

tests: compile
	rm -rf test/*/output
	mkdir -p logs
	ct_run -dir test -suite $(CT_SUITES) -pa $(DEPS_PATH) -boot start_sasl -s gateway_app -logdir logs $(CT_OPTS)
	rm -rf test/*.beam

