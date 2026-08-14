-module(rebar3_depup).
-moduledoc false.

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar3_depup_prv:init(State).
