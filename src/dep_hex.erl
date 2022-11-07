%% @doc Interface with hex.pm
-module(dep_hex).

-behaviour(hex_http).

-export([request/5]).
-export([get_latest_vsn/1]).

%% @doc Returns the latest version of a package in hex.pm
-spec get_latest_vsn(atom()) -> binary() | undefined.
get_latest_vsn(Name) ->
    case hex_repo:get_package(config(), atom_to_binary(Name, utf8)) of
        {ok, {200, _, #{releases := [_ | _] = Versions}}} ->
            lists:last([Version || #{version := Version} <- Versions]);
        Other ->
            rebar_api:warn("Couldn't fetch latest version of ~p from hex.pm:\n~p", [Name, Other]),
            undefined
    end.

config() ->
    Config1 = hex_core:default_config(),
    Config2 = put_http_config(Config1),
    Config3 = maybe_put_api_key(Config2),
    Config3.

put_http_config(Config) ->
    Config#{http_user_agent_fragment => <<"(rebar3_depup/0.0.1) (httpc)">>,
            http_adapter => {?MODULE, #{}}}.

maybe_put_api_key(Config) ->
    case os:getenv("HEX_API_KEY") of
        false ->
            Config;
        Key ->
            maps:put(api_key, Key, Config)
    end.

%% @private
request(Method, URI, ReqHeaders, Body, _AdapterConfig) ->
    Request = build_request(URI, ReqHeaders, Body),
    SSLOpts = [{ssl, rebar_utils:ssl_opts(URI)}],
    case httpc:request(Method, Request, SSLOpts, [{body_format, binary}], default) of
        {ok, {{_, StatusCode, _}, _RespHeaders, RespBody}} ->
            {ok, {StatusCode, #{}, RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

build_request(URI, ReqHeaders, Body) ->
    build_request2(binary_to_list(URI), dump_headers(ReqHeaders), Body).

build_request2(URI, ReqHeaders, undefined) ->
    {URI, ReqHeaders};
build_request2(URI, ReqHeaders, {ContentType, Body}) ->
    {URI, ReqHeaders, ContentType, Body}.

dump_headers(Map) ->
    maps:fold(fun(K, V, Acc) -> [{binary_to_list(K), binary_to_list(V)} | Acc] end, [], Map).
