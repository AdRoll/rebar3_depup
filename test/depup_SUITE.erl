-module(depup_SUITE).

-export([all/0]).
-export([not_found/1, no_updates/1, no_replace/1, default_updates/1, profile_updates/1,
         no_approx/1, just_deps/1, just_plugins/1, just_hex/1, ignore/1, ignore_config/1]).

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
     ignore_config].

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
    [{recon, _}] =
        lists:usort(proplists:get_value(deps, UpdatedConfig)
                    -- proplists:get_value(deps, OriginalConfig)),
    ok.

%% @doc Don't update ignored deps in the rebar.config
ignore_config(_) ->
    {OriginalConfig, UpdatedConfig} =
        run_with("ignore_config.config", [{replace, true}, {just_deps, true}]),
    [{recon, _}] =
        lists:usort(proplists:get_value(deps, UpdatedConfig)
                    -- proplists:get_value(deps, OriginalConfig)),
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
