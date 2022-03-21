%%% @doc Plugin provider for rebar3 depup.
%%% @private
-module(rebar3_depup_prv).

-export([init/1, do/1, format_error/1]).

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([{name, 'update-deps'},
                          {module, rebar3_depup_prv},
                          {bare, true},
                          {deps, []},
                          {example, "rebar3 update-deps"},
                          {opts, opts()},
                          {short_desc, "A rebar plugin to update dependencies"},
                          {desc, "A rebar plugin to update dependencies"}]),
    {ok, rebar_state:add_provider(State, Provider)}.

opts() ->
    [{replace,
      $r,
      "replace",
      {boolean, false},
      "Directly replace values in rebar.config."
      " The default is to just show you what deps can be updated"
      " because this is an experimental feature and using it can mess up"
      " your formatting and comments."},
     {rebar_config, $c, "rebar-config", {string, "rebar.config"}, "File to analyze"},
     {update_approx,
      $a,
      "update-approx",
      {boolean, true},
      "Update requirements starting with '~>' as well as the ones with a specific version."},
     {just_deps,
      $d,
      "just-deps",
      {boolean, false},
      "Only update deps (i.e. ignore plugins and project_plugins)."},
     {just_plugins,
      $p,
      "just-plugins",
      {boolean, false},
      "Only update plugins and project_plugins (i.e. ignore deps)."},
     {just_hex,
      $h,
      "just-hex",
      {boolean, false},
      "Only update hex packages, ignore git repos."},
     {ignore, $i, "ignore", atom, "Ignore dep when updating (can be repeated)."},
     {only,
      $o,
      "only",
      {atom, none},
      "Only update if the specified SemVer component (major, minor, or patch) has changed."}].

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    Opts = parse_opts(State),
    rebar_api:debug("Opts: ~p", [Opts]),
    #{rebar_config := RebarConfig} = Opts,
    case file:consult(RebarConfig) of
        {error, enoent} ->
            {error, io_lib:format("~ts not found", [RebarConfig])};
        {ok, Config} ->
            rebar_api:info("Looking for dependencies to update in rebar.config...", []),
            case update_deps(Config, Opts) of
                Config ->
                    rebar_api:info("Nothing to update.", []),
                    {ok, State};
                NewConfig ->
                    {ok, Backup} = file:read_file(RebarConfig),
                    case dump_or_print(NewConfig, RebarConfig, Opts) of
                        ok ->
                            {ok, State};
                        error ->
                            ok = file:write_file(RebarConfig ++ ".backup", Backup),
                            {error,
                             io_lib:format("~ts was broken. Backup saved as ~ts.backup.",
                                           [RebarConfig, RebarConfig])}
                    end
            end
    end.

%% @private
-spec format_error(any()) -> binary().
format_error(Reason) ->
    unicode:characters_to_binary(
        io_lib:format("~tp", [Reason])).

parse_opts(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    IgnoreList =
        lists:usort(proplists:get_all_values(ignore, Args)
                    ++ proplists:get_value(ignore, rebar_state:get(State, depup, []), [])),
    Only = proplists:get_value(only, rebar_state:get(State, depup, []), none),
    #{replace => proplists:get_value(replace, Args),
      rebar_config => proplists:get_value(rebar_config, Args),
      update_approx => proplists:get_value(update_approx, Args),
      just_deps => proplists:get_value(just_deps, Args),
      just_plugins => proplists:get_value(just_plugins, Args),
      just_hex => proplists:get_value(just_hex, Args),
      ignore => IgnoreList,
      only => proplists:get_value(only, Args, Only)}.

update_deps(Config, Opts) ->
    update_deps(Config, default, Opts).

update_deps(Config, Profile, Opts) ->
    [{Section, update_deps(Section, Data, Profile, Opts)} || {Section, Data} <- Config].

update_deps(deps, [], _Profile, _Opts) ->
    [];
update_deps(deps, Deps, _Profile, #{just_plugins := true}) ->
    Deps;
update_deps(deps, Deps, _Profile, Opts) ->
    dep_updater:update(Deps, Opts);
update_deps(plugins, Deps, _Profile, #{just_deps := true}) ->
    Deps;
update_deps(plugins, Deps, _Profile, Opts) ->
    dep_updater:update(Deps, Opts);
update_deps(project_plugins, Deps, _Profile, #{just_deps := true}) ->
    Deps;
update_deps(project_plugins, Deps, _Profile, Opts) ->
    dep_updater:update(Deps, Opts);
update_deps(profiles, Profiles, default, Opts) ->
    [{Profile, update_deps(Config, Profile, Opts)} || {Profile, Config} <- Profiles];
update_deps(_Section, Data, _Profile, _Opts) ->
    Data.

dump_or_print(Sections, RebarConfig, #{replace := true}) ->
    Formatted = format(Sections, RebarConfig, erl_comment_scan:file(RebarConfig)),
    ok = file:write_file(RebarConfig, Formatted),
    case file:consult(RebarConfig) of
        {error, _} ->
            error;
        {ok, _} ->
            rebar_api:info("Dependencies updated in rebar.config."
                           " Don't forget to run rebar3 upgrade.",
                           []),
            ok
    end;
dump_or_print(_, _, #{replace := false}) ->
    rebar_api:info("After applying the changes listed above, don't forget to run rebar3 upgrade.",
                   []),
    ok.

format(Sections, _RebarConfig, []) ->
    Format = lists:flatmap(fun(_) -> "~p.\n\n" end, Sections),
    io_lib:format(Format, Sections);
format(Sections, RebarConfig, Comments) ->
    AST = fake_ast(RebarConfig, Sections),
    WithComments = erl_recomment:quick_recomment_forms(AST, Comments),
    Formatted = clean(erl_prettypr:format(WithComments)),
    case erl_comment_scan:string(binary_to_list(iolist_to_binary(Formatted))) of
        Comments ->
            ok;
        NewComments ->
            rebar_api:warn("Some comments could've been lost or misplaced."
                           " We moved from ~p to ~p",
                           [length(Comments), length(NewComments)])
    end,
    Formatted.

%% @doc Turns rebar.config sections into "a module" that can be parsed with epp_dodger.
fake_ast(RebarConfig, Sections) ->
    TmpFilename =
        RebarConfig ++ ".depup_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".tmp",
    TmpFormat = lists:flatmap(fun(_) -> "dEl3tE_me() -> ~p.\n\n" end, Sections),
    TmpFormatted = io_lib:format("\n\n" ++ TmpFormat, Sections),
    ok = file:write_file(TmpFilename, TmpFormatted),
    try epp_dodger:parse_file(TmpFilename, [{scan_opts, [text]}, no_fail]) of
        {ok, AST} ->
            AST
    after
        file:delete(TmpFilename)
    end.

clean(String) ->
    re:replace(String, "(^|\n)dEl3tE_me[(][)] -> ?", "", [unicode, global]).
