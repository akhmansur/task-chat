REBAR=rebar3

.PHONY: deps test

rebar:
	wget https://s3.amazonaws.com/rebar3/rebar3
	chmod u+x rebar3

compile:
	${REBAR} compile

docs:
	${REBAR} edoc

deps:
	${REBAR} get-deps

clean:
	${REBAR} clean

test: compile
	${REBAR} eunit

server: compile
	erl -pa ebin _build/default/lib/*/ebin -eval "application:start(chat)." -config ./config/sys.config