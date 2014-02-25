-module(app_status_provider_application_tests).

-include_lib("eunit/include/eunit.hrl").

ycomb(Fun) -> Fun(Fun).
-define(untilEqual(A, B),
        (ycomb(fun (F) ->
                fun
                    (0) -> ?assertEqual(A, B);
                    (N) -> 
                        case ((A) == (B)) of
                            true -> true;
                            false -> timer:sleep(10), F(N - 1)
                        end
                end
        end))(50)).

provider_test() ->
    app_status_tests:setup(),
    app_status_provider_application:update(),
    case app_status:get_status({application, compiler}) of
        ready ->
            application:stop(compiler),
            app_status_provider_application:update(),
            ?untilEqual(not_seen, app_status:get_status(compiler)),
            application:start(compiler),
            app_status_provider_application:update(),
            ?untilEqual(ready, app_status:get_status(compiler)),
            ok;
        not_seen ->
            application:start(compiler),
            app_status_provider_application:update(),
            ?untilEqual(ready, app_status:get_status(compiler)),
            application:stop(compiler),
            app_status_provider_application:update(),
            ?untilEqual(not_seen, app_status:get_status(compiler)),
            ok
    end.

delete_stopped_apps_test() ->
    app_status_tests:setup(),
    app_status_provider_application:update(),
    app_status:ready({application, really_not_existed_application}),
    app_status_provider_application:update(),
    ?untilEqual(not_seen, app_status:get_status({application, really_not_existed_application})),
    ok.

interference_test() ->
    app_status_tests:setup(),
    app_status_provider_application:update(),
    app_status:ready(interference_test),
    ?assertEqual(ready, app_status:get_status(interference_test)), 
    app_status:ready({application, really_not_existed_application}),
    app_status_provider_application:update(),
    ?untilEqual(not_seen, app_status:get_status({application, really_not_existed_application})),
    ?assertEqual(ready, app_status:get_status(interference_test)), 
    ok.


