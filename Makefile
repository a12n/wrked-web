REBAR ?= ./rebar3

.PHONY: all clean distclean rel shell

all: $(REBAR)
	$(REBAR) compile

clean: $(REBAR)
	$(REBAR) clean

distclean: clean
	rm -rf _build/ rebar3

rel: $(REBAR)
	$(REBAR) release

shell: $(REBAR)
	$(REBAR) as test shell --config wrked.config

rebar3:
	wget "https://s3.amazonaws.com/rebar3/rebar3" -O $@-part
	chmod +x $@-part
	mv $@-part $@
