-module(app_status).

-export([expect/2,
         wait/1,
         wait/2,
         init/1,
         init/2,
         ready/1,
         ready/2,
         dead/1,
         get_status/1,
         notify/1,
         denotify/1
        ]).

-export([i/0]).

-export_type([name/0,
              status/0,
              expectations/0
             ]).

-type name()            :: term().
-type internal_status() :: app_status_server:internal_status().
-type status()          :: {waiting, [name()]} | internal_status().
-type expectations()    :: name() | [name()].

-define(WAIT_TIMEOUT, 300000). %% 5m

%%
%% An API for applications
%%

-spec expect(name(), expectations()) -> ok | {error, deadlock}.
expect(Name, Expected) when is_list(Expected) ->
    app_status_server:expect(Name, Expected);
expect(Name, Expected) ->
    expect(Name, [Expected]).

-spec wait(name()) -> ok | {error, timeout}.
wait(Name) ->
    wait(Name, ?WAIT_TIMEOUT).

-spec wait(name(), timeout()) -> ok | {error, timeout}.
wait(Name, Timeout) ->
    app_status_server:wait(Name, Timeout).

-spec init(name()) -> ok.
init(Name) ->
    init(Name, []).

-spec init(name(), [no_demonitor | monitor | {monitor, pid()}]) -> ok.
init(Name, Params) ->
    lager:info("[app_status] `~p` switching it's status to init", [Name]),
    proplists:get_value(no_demonitor, Params, false)
        orelse app_status_monitor:demonitor_all(Name),
    case proplists:get_value(monitor, Params, false) of
        true ->
            app_status_monitor:monitor(Name, self());
        Pid when is_pid(Pid) ->
            app_status_monitor:monitor(Name, Pid);
        false ->
            nop
    end,
    app_status_server:set_status(Name, init).


-spec ready(name()) -> ok.
ready(Name) ->
    ready(Name, []).

-spec ready(name(), [demonitor | no_monitor]) -> ok.
ready(Name, Params) ->
    lager:info("[app_status] `~p` switching it's status to ready", [Name]),
    proplists:get_value(demonitor, Params, false)
        andalso app_status_monitor:demonitor_all(Name),
    proplists:get_value(no_monitor, Params, false)
        orelse app_status_monitor:monitor(Name, self()),
    app_status_server:set_status(Name, ready).

-spec dead(name()) -> ok.
dead(Name) ->
    lager:info("[app_status] `~p` now dead", [Name]),
    app_status_server:set_status(Name, dead),
    app_status_monitor:demonitor_all(Name).

-spec get_status(name()) -> status() | {error, not_started}.
get_status(Name) ->
    app_status_server:get_status(Name).

-spec notify(name()) -> ok.
notify(Name) ->
    app_status_server:notify(Name, self()).

-spec denotify(name()) -> ok.
denotify(Name) ->
    app_status_server:denotify(Name, self()).

%%
%% Debug API
%%

-spec i() -> [{name(), internal_status(), [name()]}].
i() ->
    app_status_server:i().

