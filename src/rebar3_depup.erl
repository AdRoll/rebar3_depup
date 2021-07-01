%%% @doc Main entry point for the rebar3 depup plugin
-module(rebar3_depup).

-export([init/1]).

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar3_depup_prv:init(State).
