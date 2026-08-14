-module(dep_updater).
-moduledoc false.

-type deps() :: [atom() | tuple()].
-type opts() ::
    #{
        just_hex := boolean(),
        update_approx := boolean(),
        _ => _
    }.

-export_type([deps/0, opts/0]).

-export([update/3]).

-ifdef(TEST).

-export([latest_tag/1]).

-endif.

-doc """
Updates each updatable dependency in Deps.
It will update all hex packages with fixed or similar (`~>`)
semver requirements and all deps linked to a particular tag in a git repository.
If `#{just_hex := true} = Opts`, it only checks packages from hex.pm.
If `#{update_approx := false} = Opts`, it only check exact versions.
If `#{ignore := [atom,...]} = Opts`, the specified deps won't be updated.
""".
-spec update(deps(), atom(), opts()) -> deps().
update(Deps, Profile, Opts) ->
    DepsToIgnore = maps:get(ignore, Opts),
    lists:map(
        fun(Dep) ->
            Name = dep_name(Dep),
            case proplists:lookup(Name, DepsToIgnore) of
                none ->
                    update_dep(Dep, Profile, Opts);
                _Ignored ->
                    Dep
            end
        end,
        Deps
    ).

dep_name(Dep) when is_tuple(Dep) ->
    element(1, Dep);
dep_name(Dep) when is_atom(Dep) ->
    Dep.

-doc """
See `rebar_app_utils:parse_dep/5`.
""".
update_dep({_, _, {pkg, _}} = Dep, Profile, Opts) ->
    maybe_update_hex_dep(Dep, Profile, Opts);
update_dep({_, {pkg, _}} = Dep, _Profile, _Opts) ->
    %% Already using the latest version
    Dep;
update_dep({_, Vsn} = Dep, Profile, Opts) when is_list(Vsn); is_binary(Vsn) ->
    maybe_update_hex_dep(Dep, Profile, Opts);
update_dep(Dep, _Profile, _Opts) when is_atom(Dep); is_binary(Dep) ->
    %% Already using the latest version
    Dep;
update_dep(Dep, _Profile, #{just_hex := true}) ->
    Dep;
update_dep({Name, Source} = Dep, Profile, Opts) when is_tuple(Source) ->
    maybe_update_git_dep(Name, Source, Dep, Profile, Opts);
update_dep({Name, _Vsn, Source} = Dep, Profile, Opts) when is_tuple(Source) ->
    maybe_update_git_dep(Name, Source, Dep, Profile, Opts);
update_dep({Name, _Vsn, Source, RepoOpts} = Dep, Profile, Opts) when
    is_tuple(Source), is_list(RepoOpts)
->
    maybe_update_git_dep(Name, Source, Dep, Profile, Opts);
update_dep({Name, Source, RepoOpts} = Dep, Profile, Opts) when
    is_tuple(Source), is_list(RepoOpts)
->
    maybe_update_git_dep(Name, Source, Dep, Profile, Opts);
update_dep(Dep, Profile, _Opts) ->
    rebar_api:warn("Unparsable dependency: ~p (profile ~p)", [Dep, Profile]),
    Dep.

maybe_update_hex_dep({Name, Vsn, {pkg, Package}}, Profile, Opts) ->
    {Name, maybe_update_hex_dep(Vsn, Package, Profile, Opts), {pkg, Package}};
maybe_update_hex_dep({Name, Vsn}, Profile, Opts) ->
    {Name, maybe_update_hex_dep(Vsn, Name, Profile, Opts)}.

maybe_update_hex_dep(Vsn, Package, Profile, Opts) ->
    case dep_hex:get_latest_vsn(Package, Profile) of
        undefined ->
            rebar_api:info(
                "Latest version for ~p (profile ~p) not found, keeping ~ts",
                [Package, Profile, Vsn]
            ),
            Vsn;
        NewVsn ->
            latest_version(Package, Vsn, NewVsn, Profile, Opts)
    end.

%% NOTE: This function only updates git deps when they use valid semver tags.
maybe_update_git_dep(Name, {git, Repo, {tag, Vsn}}, Dep, Profile, Opts) ->
    GitCmd =
        lists:flatten(
            io_lib:format("git ls-remote --refs --tags ~p '*.*.*'", [Repo])
        ),
    LatestVsn =
        case rebar_utils:sh(GitCmd, [return_on_error]) of
            {ok, GitResult} ->
                NewVsn = latest_tag(string:tokens(GitResult, [$\n])),
                latest_version(Name, Vsn, NewVsn, Profile, Opts);
            {error, {_Code, Msg}} ->
                rebar_api:warn(Msg ++ "===> Skipping ~p (profile ~p)", [Name, Profile]),
                Dep
        end,
    {Name, {git, Repo, {tag, LatestVsn}}};
maybe_update_git_dep(Name, _Source, Dep, Profile, _Opts) ->
    rebar_api:info(
        "~p (profile ~p) can't be updated because it's not using a tag",
        [Name, Profile]
    ),
    Dep.

-doc """
Highest semver tag in `git ls-remote` output, `undefined` if there is none.
We sort here instead of with git's `--sort=v:refname` because that one
orders refnames as text, so any tag that is not plain semver (e.g. v1.6.0)
lands after the actual latest one (e.g. 5.3.1).
""".
latest_tag(Refs) ->
    Versions =
        lists:filtermap(
            fun(Ref) ->
                Tag = iolist_to_binary(
                    lists:last(
                        string:tokens(Ref, [$/])
                    )
                ),
                Vsn = unprefix(Tag),
                case verl:parse(Vsn) of
                    {ok, _} ->
                        {true, {Vsn, Tag}};
                    {error, _} ->
                        false
                end
            end,
            Refs
        ),
    case lists:sort(fun({V1, _}, {V2, _}) -> verl:compare(V1, V2) =/= gt end, Versions) of
        [] ->
            undefined;
        Sorted ->
            element(2, lists:last(Sorted))
    end.

unprefix(<<"v", Vsn/binary>>) ->
    Vsn;
unprefix(Vsn) ->
    Vsn.

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
        {ok, #{major := CurrentMajor, minor := CurrentMinor}, #{
            major := LatestMajor, minor := LatestMinor
        }} ->
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

-spec latest_version
    (atom(), binary(), binary() | undefined, atom(), opts()) -> binary();
    (atom(), string(), binary() | undefined, atom(), opts()) -> string().
latest_version(Name, Vsn, undefined, Profile, _Opts) ->
    rebar_api:info(
        "Latest version for ~p (profile ~p) not found, keeping ~ts",
        [Name, Profile, Vsn]
    ),
    Vsn;
latest_version(Name, Vsn, NewVsn, Profile, Opts) when is_list(Vsn) ->
    BinVsn = iolist_to_binary(Vsn),
    case latest_version(Name, BinVsn, NewVsn, Profile, Opts) of
        BinVsn ->
            Vsn;
        OtherVsn ->
            binary_to_list(OtherVsn)
    end;
latest_version(Name, <<"v", Vsn/binary>>, <<"v", NewVsn/binary>>, Profile, Opts) ->
    %% Special case that we use a lot
    LatestVsn = latest_version(Name, Vsn, NewVsn, Profile, Opts),
    <<"v", LatestVsn/binary>>;
latest_version(
    Name,
    <<"~> ", Vsn/binary>>,
    NewVsn,
    Profile,
    #{update_approx := true} = Opts
) ->
    %% Special case that we use a lot
    LatestVsn = latest_version(Name, Vsn, NewVsn, Profile, Opts),
    <<"~> ", LatestVsn/binary>>;
latest_version(_Name, <<"~> ", Vsn/binary>>, _NewVsn, _Profile, _Opts) ->
    <<"~> ", Vsn/binary>>;
latest_version(Name, Vsn, NewVsn, Profile, Opts) ->
    CheckedVsn =
        case check_only(Vsn, NewVsn, Opts) of
            {ok, Version} ->
                Version;
            {error, Error} ->
                rebar_api:info(
                    "~p's (profile ~p) version ~p can't be parsed, keeping ~p: ~p",
                    [Name, Profile, NewVsn, Vsn, Error]
                ),
                Vsn
        end,
    case verl:compare(CheckedVsn, Vsn) of
        gt ->
            rebar_api:info(
                "~p (profile ~p) can be updated from ~ts to ~ts",
                [Name, Profile, Vsn, CheckedVsn]
            ),
            CheckedVsn;
        _ ->
            rebar_api:debug("~p (profile ~p) needs no update, keeping ~ts", [Name, Profile, Vsn]),
            Vsn
    end.
