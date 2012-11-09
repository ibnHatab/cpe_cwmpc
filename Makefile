
REBAR='./rebar'

APP = cpe_cwmpc
VSN = $(shell sed -n 's/.*{vsn,.*"\(.*\)"}.*/\1/p' src/$(APP).app.src)

DOCDIR=$(APP)_info

all: compile

compile: deps
	$(REBAR) -v compile

app:
	$(REBAR) -v compile skip_deps=true

deps:
	$(REBAR) check-deps || $(REBAR) get-deps

# Documentation targets
#
$(DOCDIR):
	-@mkdir $(DOCDIR)

docs:   $(DOCDIR) orgs
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[{dir,"$(DOCDIR)"}, {def,{vsn,"$(VSN)"}}]'

#	@erl -noshell -run edoc_run file '"test/soap_parser_SUITE.erl"' '[{dir,"$(DOCDIR)"}, {def,{vsn,"$(VSN)"}}]'

orgs: orgs-doc orgs-README

orgs-doc: $(DOCDIR)
	@emacs -l orgbatch.el -batch --eval="(riak-export-doc-dir \"doc\" 'html)"
	@cp  doc/*.html $(DOCDIR)
	-@mkdir $(DOCDIR)/images
	@cp  doc/images/*.png $(DOCDIR)/images

orgs-README:
	@emacs -l orgbatch.el -batch --eval="(riak-export-doc-file \"README.org\" 'ascii)"
	@mv README.txt README


clean:
	$(REBAR) clean
	-@rm -rf $(DOCDIR)
	-@rm -rf logs
	-@rm -rf test/*.beam
	-@rm -rf .eunit
	-@rm -rf $(APP)_info
	-@rm doc/application.html
	-@rm README

distclean: clean
	$(REBAR) clean delete-deps

# unitary tests
utest:
	$(REBAR) -v eunit skip_deps=true 
#suite=cwmp_builder

ut-shell:
	exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin -pa $(PWD)/.eunit -boot start_sasl -s reloader 


# common tests
test.spec: test.spec.in
	cat test.spec.in | sed -e "s,@PATH@,$(PWD)," > $(PWD)/test.spec

ctest:  test.spec compile
	-@mkdir logs
	ct_run -pa $(PWD)/lib/*/ebin -pz ./ebin -spec test.spec

ct-shell:
	ct_run -shell -pa $(PWD)/deps/*/ebin -pz ./ebin -pz ./test -spec test.spec


test: app
	$(REBAR) -v ct skip_deps=true suites=hdm_trace case=hdm_trace_test_case

dialyzer-build:
	dialyzer --build_plt --verbose			\
	  --output_plt ~/.dialyzer-R15B.plt 		\
	  --apps kernel stdlib sasl erts ssl 	 	\
	    tools os_mon runtime_tools crypto 		\
	    inets xmerl public_key syntax_tools 	\
	    mnesia eunit et compiler			\
	    ./deps/*/ebin

dialyzer: compile
	dialyzer --plt ~/.dialyzer-R15B.plt \
	  -Wunmatched_returns 	\
	  -Werror_handling 	\
	  -Wrace_conditions 	\
	  ./ebin
