NS = pair
INTF0 = veth0
INTF1 = veth1
IT=5
PORT = 8999

.PHONY: all compile deps clean dev

all: compile

compile:
	./rebar3 compile

clean:
	./rebar3 clean

dev: compile
	erl -pa _build/default/lib/*/ebin \
	-pair port $(port) \
	-pair state $(state) \
	-pair ip \"$(ip)\" \
	-pair peer_ip \"$(peer_ip)\" \
	-pair pair_no $(pair_no) \
	-pair intf_name $(intf) \
	-pair iterations $(it) \
	-eval "[application:start(A) || A <- [compiler, syntax_tools, \
		goldrush, lager, pair]]"

rel:
	./relx --vm_args config/vm.args

# has to be run with sudo
test_setup:
	ip netns add $(NS)
	ip link add $(INTF0) type veth peer name $(INTF1)
	ip link set $(INTF1) netns $(NS)
	ifconfig $(INTF0) 10.0.0.1/24 up
	ip netns exec $(NS) ifconfig $(INTF1) 10.0.0.2/24 up

# run with sudo
test_teardown:
	ip link del $(INTF0)
	ip netns delete $(NS)

test_pair1:
	make dev port=$(PORT) state=active ip="10.0.0.1" peer_ip="10.0.0.2" \
	pair_no=1 it=$(IT) intf=$(INTF0)

test_pair2:
	ip netns exec $(NS) make dev port=$(PORT) state=passive ip="10.0.0.2" \
	peer_ip="10.0.0.1" pair_no=1 it=$(IT) intf=$(INTF1)


rebar:
	wget -c https://s3.amazonaws.com/rebar3/rebar3
	chmod +x rebar3
