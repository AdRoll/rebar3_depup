-module(depup_SUITE).

-export([all/0]).
-export([not_found/1, no_updates/1, no_replace/1, default_updates/1, profile_updates/1,
         no_approx/1, just_deps/1, just_plugins/1, just_hex/1, ignore/1, ignore_config/1,
         only_patch/1, only_minor/1, only_major/1]).

-behaviour(ct_suite).

all() ->
    [not_found,
     no_updates,
     no_replace,
     default_updates,
     profile_updates,
     no_approx,
     just_deps,
     just_plugins,
     just_hex,
     ignore,
     ignore_config,
     only_patch,
     only_minor,
     only_major].

%% @doc Can't find not_found.config
not_found(_) ->
    State = init([{rebar_config, "not_found.config"}], []),
    {error, Error} = rebar3_depup_prv:do(State),
    true = string:equal(Error, "not_found.config not found"),
    ok.

%% @doc None of the deps should be updatable.
no_updates(_) ->
    {OriginalConfig, OriginalConfig} = run_with("no_updates.config", [{replace, true}]),
    ok.

%% @doc Deps should be updated, but the file should remain unchanged
no_replace(_) ->
    {OriginalConfig, OriginalConfig} = run_with("no_replace.config", []),
    ok.

%% @doc This tests all the different kinds of possible updates in the default profile
default_updates(_) ->
    {OriginalConfig, UpdatedConfig} = run_with("default_updates.config", [{replace, true}]),
    [{erliam, {git, "https://github.com/AdRoll/erliam.git", {tag, _}}},
     {kinetic, {git, "https://github.com/AdRoll/kinetic.git", {tag, _}}},
     {lager, _},
     {zstd, {git, "https://github.com/AdRoll/zstd-erlang.git", {tag, _}}},
     {erlcloud, _, {pkg, nextroll_erlcloud}}] =
        lists:usort(proplists:get_value(deps, UpdatedConfig)
                    -- proplists:get_value(deps, OriginalConfig)),
    [{rebar3_gpb_plugin, _}] =
        lists:usort(proplists:get_value(plugins, UpdatedConfig)
                    -- proplists:get_value(plugins, OriginalConfig)),
    [{rebar3_format, "~> " ++ _},
     {rebar3_hank, {git, "https://github.com/AdRoll/rebar3_hank.git", {tag, _}}},
     {rebar3_lint, "~> " ++ _, {pkg, rebar3_lint}}] =
        lists:usort(proplists:get_value(project_plugins, UpdatedConfig)
                    -- proplists:get_value(project_plugins, OriginalConfig)),
    ok.

%% @doc This tests all the different kinds of possible updates in the default profile
profile_updates(_) ->
    {OriginalConfig, UpdatedConfig} = run_with("profile_updates.config", [{replace, true}]),
    OriginalProfiles = proplists:get_value(profiles, OriginalConfig),
    OriginalDConfig = proplists:get_value(d, OriginalProfiles),
    OriginalPConfig = proplists:get_value(p, OriginalProfiles),
    UpdatedProfiles = proplists:get_value(profiles, UpdatedConfig),
    UpdatedDConfig = proplists:get_value(d, UpdatedProfiles),
    UpdatedPConfig = proplists:get_value(p, UpdatedProfiles),
    [{lager, _}] =
        lists:usort(proplists:get_value(deps, UpdatedDConfig)
                    -- proplists:get_value(deps, OriginalDConfig)),
    [{rebar3_archive_plugin, _, {pkg, rebar3_archive_plugin}}] =
        lists:usort(proplists:get_value(plugins, UpdatedPConfig)
                    -- proplists:get_value(plugins, OriginalPConfig)),
    [{rebar3_format, "~> " ++ _}] =
        lists:usort(proplists:get_value(project_plugins, UpdatedPConfig)
                    -- proplists:get_value(project_plugins, OriginalPConfig)),
    ok.

%% @doc Deps using approx semver (i.e. ~> ...), should not be updated.
no_approx(_) ->
    {OriginalConfig, UpdatedConfig} =
        run_with("no_approx.config", [{replace, true}, {update_approx, false}]),
    [{meck, _}] =
        lists:usort(proplists:get_value(deps, UpdatedConfig)
                    -- proplists:get_value(deps, OriginalConfig)),
    ok.

%% @doc Only deps should be updated
just_deps(_) ->
    {OriginalConfig, UpdatedConfig} =
        run_with("just.config", [{replace, true}, {just_deps, true}]),
    [{recon, _}, {spillway, _}] =
        lists:usort(proplists:get_value(deps, UpdatedConfig)
                    -- proplists:get_value(deps, OriginalConfig)),
    [] =
        lists:usort(proplists:get_value(plugins, UpdatedConfig)
                    -- proplists:get_value(plugins, OriginalConfig)),
    [] =
        lists:usort(proplists:get_value(project_plugins, UpdatedConfig)
                    -- proplists:get_value(project_plugins, OriginalConfig)),
    ok.

%% @doc Only plugins should be updated
just_plugins(_) ->
    {OriginalConfig, UpdatedConfig} =
        run_with("just.config", [{replace, true}, {just_plugins, true}]),
    [] =
        lists:usort(proplists:get_value(deps, UpdatedConfig)
                    -- proplists:get_value(deps, OriginalConfig)),
    [{rebar3_proper, _}] =
        lists:usort(proplists:get_value(plugins, UpdatedConfig)
                    -- proplists:get_value(plugins, OriginalConfig)),
    [{rebar3_format, _}, {rebar3_hank, _}] =
        lists:usort(proplists:get_value(project_plugins, UpdatedConfig)
                    -- proplists:get_value(project_plugins, OriginalConfig)),
    ok.

%% @doc Only hex deps should be updated
just_hex(_) ->
    {OriginalConfig, UpdatedConfig} =
        run_with("just_hex.config", [{replace, true}, {just_hex, true}]),
    [{recon, _}] =
        lists:usort(proplists:get_value(deps, UpdatedConfig)
                    -- proplists:get_value(deps, OriginalConfig)),
    [{rebar3_auto, _}] =
        lists:usort(proplists:get_value(plugins, UpdatedConfig)
                    -- proplists:get_value(plugins, OriginalConfig)),
    [{rebar3_format, _}] =
        lists:usort(proplists:get_value(project_plugins, UpdatedConfig)
                    -- proplists:get_value(project_plugins, OriginalConfig)),
    ok.

%% @doc Don't update ignored deps on the command line
ignore(_) ->
    {OriginalConfig, UpdatedConfig} =
        run_with("ignore.config", [{replace, true}, {just_deps, true}, {ignore, spillway}]),
    [{rebar3_depup, _}] =
        lists:usort(proplists:get_value(deps, UpdatedConfig)
                    -- proplists:get_value(deps, OriginalConfig)),
    ok.

%% @doc Don't update ignored deps in the rebar.config
ignore_config(_) ->
    {OriginalConfig, UpdatedConfig} =
        run_with("ignore_config.config", [{replace, true}, {just_deps, true}]),
    [{rebar3_hank, _}] =
        lists:usort(proplists:get_value(deps, UpdatedConfig)
                    -- proplists:get_value(deps, OriginalConfig)),
    ok.

%% @doc Update if the patch version is the only change
only_patch(_) ->
    meck:new(dep_hex, [passthrough]),
    meck:expect(dep_hex,
                get_latest_vsn,
                %% will update from 1.1.1 to 1.1.3
                fun (will_update) ->
                        <<"1.1.3">>;
                    %% won't update from 1.1.1 to 1.3.3
                    (wont_update) ->
                        <<"1.3.3">>;
                    (Name) ->
                        meck:passthrough([Name])
                end),
    {OriginalConfig, UpdatedConfig} =
        run_with("only_patch.config", [{replace, true}, {just_deps, true}, {only, patch}]),
    [{will_update, _}] =
        lists:usort(proplists:get_value(deps, UpdatedConfig)
                    -- proplists:get_value(deps, OriginalConfig)),
    meck:unload(dep_hex),
    ok.

%% @doc Update if the minor (or patch) version is the only change
only_minor(_) ->
    meck:new(dep_hex, [passthrough]),
    meck:expect(dep_hex,
                get_latest_vsn,
                %% will update from 1.1.1 to 1.2.1
                fun (will_update) ->
                        <<"1.2.1">>;
                    %% won't update from 1.1.1 to 2.1.1
                    (wont_update) ->
                        <<"2.1.1">>;
                    (Name) ->
                        meck:passthrough([Name])
                end),
    {OriginalConfig, UpdatedConfig} =
        run_with("only_minor.config", [{replace, true}, {just_deps, true}, {only, minor}]),
    [{will_update, _}] =
        lists:usort(proplists:get_value(deps, UpdatedConfig)
                    -- proplists:get_value(deps, OriginalConfig)),
    meck:unload(dep_hex),
    ok.

%% @doc Update only if the version follows SemVer semantics
only_major(_) ->
    meck:new(dep_hex, [passthrough]),
    meck:expect(dep_hex,
                get_latest_vsn,
                %% will update from 1.1.1 to 2.2.1
                fun (will_update) ->
                        <<"2.2.1">>;
                    %% won't update from 1.1.1 to unparsable version
                    (wont_update) ->
                        <<"not-semver">>;
                    (Name) ->
                        meck:passthrough([Name])
                end),
    {OriginalConfig, UpdatedConfig} =
        run_with("only_major.config", [{replace, true}, {just_deps, true}, {only, major}]),
    [{will_update, _}] =
        lists:usort(proplists:get_value(deps, UpdatedConfig)
                    -- proplists:get_value(deps, OriginalConfig)),
    meck:unload(dep_hex),
    ok.

full_path(RebarConfig) ->
    filename:join(
        filename:dirname(
            code:which(?MODULE)),
        RebarConfig).

run_with(RebarConfig, Opts) ->
    FullPath = full_path(RebarConfig),
    {ok, OriginalConfig} = file:consult(FullPath),
    {ok, _NewState} =
        rebar3_depup_prv:do(init([{rebar_config, FullPath} | Opts], OriginalConfig)),
    {ok, UpdatedConfig} = file:consult(FullPath),
    {OriginalConfig, UpdatedConfig}.

init(Opts, Config) ->
    InitialState = rebar_state:new(Config),
    Args =
        proplists:compact(Opts
                          ++ [{rebar_config, "rebar.config"},
                              {replace, false},
                              {update_approx, true},
                              {just_deps, false},
                              {just_plugins, false},
                              {just_hex, false}]),
    StateWithConfig = rebar_state:command_parsed_args(InitialState, {Args, ""}),
    {ok, ReadyState} = rebar3_depup:init(StateWithConfig),
    ReadyState.
