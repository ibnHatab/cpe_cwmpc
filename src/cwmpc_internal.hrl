%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%  Internal data structures and macros
%%% @end
%%% Created : 24 Nov 2012 by vlad <lib.aca55a@gmail.com>

-ifndef(cwmpc_internal_hrl).
-define(cwmpc_internal_hrl, true).
-include("cpe_host/src/host_internal.hrl").

-define(SERVICE, cwmpc).
-define(cwmpri(Label, Content), ?report_important(Label, ?SERVICE, Content)).
-define(cwmprv(Label, Content), ?report_verbose(Label,   ?SERVICE, Content)).
-define(cwmprd(Label, Content), ?report_debug(Label,     ?SERVICE, Content)).
-define(cwmprt(Label, Content), ?report_trace(Label,     ?SERVICE, Content)).

-endif. % -ifdef(cwmpc_internal_hrl).


