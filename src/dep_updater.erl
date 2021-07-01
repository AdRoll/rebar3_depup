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

%% @doc Updates each updateable dependency in Deps.
%%      <p>It will update all hex packages with fixed or similar (<code>~></code>)
%%      semver requirements and all deps linked to a particular tag in a git repository.</p>
%%      <p>If <code>#{just_hex := true} = Opts</code>, it only checks pacakges from hex.pm.</p>
%%      <p>If <code>#{update_approx := false} = Opts</code>, it only check exact versions.</p>
-spec update(deps(), opts()) -> deps().
update(Deps, Opts) ->
    [update_dep(Dep, Opts) || Dep <- Deps].

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
    rebar_api:warn("Unparseable dependency: ~p", [Dep]),
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
maybe_update_git_dep(Name, {git, Repo, {tag, Vsn}}, _Dep, Opts) ->
    GitCmd =
        lists:flatten(
            io_lib:format("git -c 'versionsort.suffix=-' ls-remote --refs --tags ~p '*.*.*'",
                          [Repo])),
    LatestVsn =
        case rebar_utils:sh(GitCmd, []) of
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
                latest_version(Name, Vsn, iolist_to_binary(NewVsn), Opts)
        end,
    {Name, {git, Repo, {tag, LatestVsn}}};
maybe_update_git_dep(Name, _Source, Dep, _Opts) ->
    rebar_api:info("~p can't be updated because it's not using a tag", [Name]),
    Dep.

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
latest_version(Name, Vsn, NewVsn, _Opts) ->
    case verl:compare(NewVsn, Vsn) of
        gt ->
            rebar_api:info("~p can be updated from ~ts to ~ts", [Name, Vsn, NewVsn]),
            NewVsn;
        _ ->
            rebar_api:debug("~p needs no update, keeping ~ts", [Name, Vsn]),
            Vsn
    end.