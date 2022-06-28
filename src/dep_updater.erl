%% @doc Core module of the system.
%%      Updates a list of dependencies according to the provided options.
-module(dep_updater).

-type deps() :: [atom() | tuple()].
-type opts() ::
    #{just_hex := boolean(),
      update_approx := boolean(),
      _ => _}.

-export_type([deps/0, opts/0]).

-export([update/2]).

%% @doc Updates each updatable dependency in Deps.
%%      <p> It will update all hex packages with fixed or similar (<code>~></code>)
%%      semver requirements and all deps linked to a particular tag in a git repository. </p>
%%      <p> If <code>#{just_hex := true} = Opts</code>,
%%          it only checks packages from hex.pm. </p>
%%      <p> If <code>#{update_approx := false} = Opts</code>,
%%          it only check exact versions. </p>
%%      <p> If <code>#{ignore := [atom,...]} = Opts</code>,
%%          the specified deps won't be updated. </p>
-spec update(deps(), opts()) -> deps().
update(Deps, Opts) ->
    DepsToIgnore = maps:get(ignore, Opts),
    lists:map(fun(Dep) ->
                 Name = dep_name(Dep),
                 case proplists:lookup(Name, DepsToIgnore) of
                     none ->
                         update_dep(Dep, Opts);
                     _Ignored ->
                         Dep
                 end
              end,
              Deps).

dep_name(Dep) when is_tuple(Dep) ->
    element(1, Dep);
dep_name(Dep) when is_atom(Dep) ->
    Dep.

%% @see rebar_app_utils:parse_dep/5.
update_dep(Dep = {_, _, {pkg, _}}, Opts) ->
    maybe_update_hex_dep(Dep, Opts);
update_dep(Dep = {_, {pkg, _}}, _Opts) ->
    Dep; %% Already using the latest version
update_dep(Dep = {_, Vsn}, Opts) when is_list(Vsn); is_binary(Vsn) ->
    maybe_update_hex_dep(Dep, Opts);
update_dep(Dep, _Opts) when is_atom(Dep); is_binary(Dep) ->
    Dep; %% Already using the latest version
update_dep(Dep, #{just_hex := true}) ->
    Dep;
update_dep(Dep = {Name, Source}, Opts) when is_tuple(Source) ->
    maybe_update_git_dep(Name, Source, Dep, Opts);
update_dep(Dep = {Name, _Vsn, Source}, Opts) when is_tuple(Source) ->
    maybe_update_git_dep(Name, Source, Dep, Opts);
update_dep(Dep = {Name, _Vsn, Source, RepoOpts}, Opts)
    when is_tuple(Source), is_list(RepoOpts) ->
    maybe_update_git_dep(Name, Source, Dep, Opts);
update_dep(Dep = {Name, Source, RepoOpts}, Opts)
    when is_tuple(Source), is_list(RepoOpts) ->
    maybe_update_git_dep(Name, Source, Dep, Opts);
update_dep(Dep, _Opts) ->
    rebar_api:warn("Unparsable dependency: ~p", [Dep]),
    Dep.

maybe_update_hex_dep({Name, Vsn, {pkg, Package}}, Opts) ->
    {Name, maybe_update_hex_dep(Vsn, Package, Opts), {pkg, Package}};
maybe_update_hex_dep({Name, Vsn}, Opts) ->
    {Name, maybe_update_hex_dep(Vsn, Name, Opts)}.

maybe_update_hex_dep(Vsn, Package, Opts) ->
    case dep_hex:get_latest_vsn(Package) of
        undefined ->
            rebar_api:info("Latest version for ~p not found, keeping ~ts", [Package, Vsn]),
            Vsn;
        NewVsn ->
            latest_version(Package, Vsn, NewVsn, Opts)
    end.

%% NOTE: This function only updates git deps when they use valid semver tags.
maybe_update_git_dep(Name, {git, Repo, {tag, Vsn}}, Dep, Opts) ->
    GitCmd =
        lists:flatten(
            io_lib:format("git -c 'versionsort.suffix=-' ls-remote "
                          "--sort=v:refname --refs --tags ~p '*.*.*'",
                          [Repo])),
    LatestVsn =
        case rebar_utils:sh(GitCmd, [return_on_error]) of
            {ok, ""} ->
                rebar_api:info("Latest version for ~p not found, keeping ~ts", [Name, Vsn]),
                Vsn;
            {ok, GitResult} ->
                LastTag =
                    lists:last(
                        string:tokens(GitResult, [$\n])),
                NewVsn =
                    lists:last(
                        string:tokens(LastTag, [$/])),
                latest_version(Name, Vsn, iolist_to_binary(NewVsn), Opts);
            {error, {_Code, Msg}} ->
                rebar_api:warn(Msg ++ "===> Skipping ~p", [Name]),
                Dep
        end,
    {Name, {git, Repo, {tag, LatestVsn}}};
maybe_update_git_dep(Name, _Source, Dep, _Opts) ->
    rebar_api:info("~p can't be updated because it's not using a tag", [Name]),
    Dep.

parse_versions(Current, Latest) ->
    case verl:parse(iolist_to_binary(Current)) of
        {ok, CurrentParsed} ->
            case verl:parse(iolist_to_binary(Latest)) of
                {ok, LatestParsed} ->
                    {ok, CurrentParsed, LatestParsed};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

check_only(Current, Latest, _) when Current =:= Latest ->
    {ok, Latest};
check_only(Current, Latest, #{only := patch}) ->
    case parse_versions(Current, Latest) of
        {ok,
         #{major := CurrentMajor, minor := CurrentMinor},
         #{major := LatestMajor, minor := LatestMinor}} ->
            case CurrentMajor =:= LatestMajor andalso CurrentMinor =:= LatestMinor of
                true ->
                    {ok, Latest};
                false ->
                    {ok, Current}
            end;
        {error, Error} ->
            {error, Error}
    end;
check_only(Current, Latest, #{only := minor}) ->
    case parse_versions(Current, Latest) of
        {ok, #{major := CurrentMajor}, #{major := LatestMajor}} ->
            case CurrentMajor =:= LatestMajor of
                true ->
                    {ok, Latest};
                false ->
                    {ok, Current}
            end;
        {error, Error} ->
            {error, Error}
    end;
check_only(Current, Latest, #{only := major}) ->
    case parse_versions(Current, Latest) of
        {ok, _, _} ->
            {ok, Latest};
        {error, Error} ->
            {error, Error}
    end;
check_only(_Current, Latest, #{only := none}) ->
    {ok, Latest};
check_only(_Current, Latest, _) ->
    {ok, Latest}.

-spec latest_version(atom(), binary(), binary(), opts()) -> binary();
                    (atom(), string(), binary(), opts()) -> string().
latest_version(Name, Vsn, NewVsn, Opts) when is_list(Vsn) ->
    BinVsn = iolist_to_binary(Vsn),
    case latest_version(Name, BinVsn, NewVsn, Opts) of
        BinVsn ->
            Vsn;
        OtherVsn ->
            binary_to_list(OtherVsn)
    end;
latest_version(Name, <<"v", Vsn/binary>>, <<"v", NewVsn/binary>>, Opts) ->
    %% Special case that we use a lot
    LatestVsn = latest_version(Name, Vsn, NewVsn, Opts),
    <<"v", LatestVsn/binary>>;
latest_version(Name, <<"~> ", Vsn/binary>>, NewVsn, Opts = #{update_approx := true}) ->
    %% Special case that we use a lot
    LatestVsn = latest_version(Name, Vsn, NewVsn, Opts),
    <<"~> ", LatestVsn/binary>>;
latest_version(_Name, <<"~> ", Vsn/binary>>, _NewVsn, _Opts) ->
    <<"~> ", Vsn/binary>>;
latest_version(Name, Vsn, NewVsn, Opts) ->
    CheckedVsn =
        case check_only(Vsn, NewVsn, Opts) of
            {ok, Version} ->
                Version;
            {error, Error} ->
                rebar_api:info("~p's version ~p can't be parsed, keeping ~p: ~p",
                               [Name, NewVsn, Vsn, Error]),
                Vsn
        end,
    case verl:compare(CheckedVsn, Vsn) of
        gt ->
            rebar_api:info("~p can be updated from ~ts to ~ts", [Name, Vsn, CheckedVsn]),
            CheckedVsn;
        _ ->
            rebar_api:debug("~p needs no update, keeping ~ts", [Name, Vsn]),
            Vsn
    end.
