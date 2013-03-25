%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%  Internal data structures and macros
%%% @end
%%% Created : 24 Nov 2012 by vlad <lib.aca55a@gmail.com>

-ifndef(cwmpc_internal_hrl).
-define(cwmpc_internal_hrl, true).

-include("cpe_host/src/host_internal.hrl").

-define(cwmpri(Label, Content), ?report_important(Label, cwmp, Content)).
-define(cwmprv(Label, Content), ?report_verbose(Label,   cwmp, Content)).
-define(cwmprd(Label, Content), ?report_debug(Label,     cwmp, Content)).
-define(cwmprt(Label, Content), ?report_trace(Label,     cwmp, Content)).

-endif. % -ifdef(cwmpc_internal_hrl).


