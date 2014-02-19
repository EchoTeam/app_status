APP STATUS
==

The erlang library provides dependency tree with notifications useful for
controlling of load process.

Example usage:

    app1:start() ->
        app_status:initialization(app1),
        % ... some initialization
        % ... 
        app_status:ready(app1).

    app2:start() ->
        ok = app_status:expect_wait(app2, app1), % will wait for app1 ready up to 5m
        % ... some another initialization
        app_status:ready(app2).
        
APP STATUS does not stick to the OTP-application name, you could use any arbitrary term:

    app_status:dead({some_application, instance666}).

Every application could be in several states: initialization, ready, dead and not_seen.
You should manually change state of the applications by calling one of `app_status:{initialization,ready,dead}/1`.
You could switch the application state many times if you need.

Every application could have dependencies. You could specify one or many by calling:

    app_status:expect(my_application, [dep1, dep2, dep3]).

If the application have unready dependencies (e.g. `app_status:get_status(dep1)` is not `ready`) then
it goes to the special `waiting` state: `app_status:get_status(my_application)` is `{waiting, [dep1]}`.

APP STATUS also provides basic monitoring via `app_status:monitor/{1,2}` functions.

    app_status:monitor(my_application).

When process who calls `app_status:monitor/1` is died, `app_status:dead(my_application)` will be
performed automatically.

APP STATUS also supports notifications via `app_status:notify/{1,2}` functions.

    app_status:notify(my_application, Pid2).

This statement will send message `{app_status, my_application, NewStatus}` to the `Pid2` every time
when `my_application` will switch from the `ready` state to another and back.

There are functions `app_status:wait/{1,2}` whose syncronically waits for some specified application
will not be in status `{waiting, ...}`.

In order to obtain more information please refer the source code.

