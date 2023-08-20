# rebar3_depup

## Dependency updater for rebar3 managed projects

![Cinderella Upgrade](https://media1.tenor.com/images/5d0c66a25a24f1c89936b90ea54ac41a/tenor.gif?itemid=13582416)

## Usage

Add the plugin to your rebar.config:

```erlang
{project_plugins, [rebar3_depup]}.
```

You can add it to your _global `rebar.config`_ (e.g. `~/.config/rebar3/rebar.config`).

Then...

```bash
rebar3 update-deps
```

### ⚠️ Warning ⚠️

With the default options (see below), this project assumes that…

1. You have `rebar.config` file in your root folder
1. You do not use a `rebar.config.script`
1. The current OS user has access to the git repos that you have in
your `rebar.config` file.

### Command-Line Options

```bash
$ rebar3 help update-deps

A rebar plugin to update dependencies
Usage: rebar3 update-deps [-r [<replace>]] [-c [<rebar_config>]]
                          [-a [<update_approx>]] [-d [<just_deps>]]
                          [-p [<just_plugins>]] [-h [<just_hex>]]
                          [-i <ignore>] [-o [<only>]]

  -r, --replace        Directly replace values in rebar.config. The 
                       default is to just show you what deps can be 
                       updated because this is an experimental feature and 
                       using it can mess up your formatting and comments. 
                       [default: false]
  -c, --rebar-config   File to analyze [default: rebar.config]
  -a, --update-approx  Update requirements starting with '~>' as well as 
                       the ones with a specific version. [default: true]
  -d, --just-deps      Only update deps (i.e. ignore plugins and 
                       project_plugins). [default: false]
  -p, --just-plugins   Only update plugins and project_plugins (i.e. 
                       ignore deps). [default: false]
  -h, --just-hex       Only update hex packages, ignore git repos. 
                       [default: false]
  -i, --ignore         Ignore dep when updating (can be repeated).
  -o, --only           Only update if the specified SemVer component 
                       (major, minor, or patch) has changed. [default: 
                       none]
```

## Configuration

To automatically ignore updates for one or more deps, add the `ignore`
configuration to your `rebar.config`:

```erlang
%% Ignore any updates for eredis and lager.
{depup, [{ignore, [eredis, lager]}]}.
```

To only update if the specified SemVer component has changed, use `only`.
Note that this configuration can
be overridden by the `-o/--only` command-line argument:

```erlang
%% Ignore any updates that change more than the minor
%% (and patch) version of any dependency.
{depup, [{only, minor}].
```

With the above configuration in your `rebar.config`, running the following will
update all dependencies by overriding `minor` with `none`:

```bash
rebar3 update-deps --only none
```

## Build

```bash
rebar3 compile
```

## Test

```bash
rebar3 test
```
