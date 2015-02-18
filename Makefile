.PHONY: all compile deps clean dev

all: compile

compile:
	./rebar get-deps compile

deps:
	./rebar get-deps

clean:
	./rebar clean

dev: compile
	erl -pa ebin -pa deps/*/ebin \
	-eval "[application:start(A) || A <- [compiler, syntax_tools, goldrush, lager]], \
		pr_fsm:start_link(8999, [{state, $(state)}, {ip, $(ip)}, \
		{peer_ip, $(peer_ip)}, {iterations, 1}])"


rebar:
	wget -c http://github.com/rebar/rebar/wiki/rebar
	chmod +x $@
