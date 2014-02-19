-module(app_status_tests).

-include_lib("eunit/include/eunit.hrl").


%%
%% Init
%%

setup() ->
    app_status_monitor:start_link(),
    app_status:start_link().

delayed(Fun) ->
    Self = self(),
    _Pid = spawn(fun () ->
                    receive
                        tick -> ok
                    end,
                    Fun(Self)
                end).

send_(Pid, Msg) ->
    Pid ! {?MODULE, Msg}.

recv_() ->
    receive
        {?MODULE, Msg} ->
            {recv, Msg}
    after 5000 ->
            {error, timeout}
    end.

ycomb(Fun) -> Fun(Fun).
-define(untilEqual(A, B),
        (ycomb(fun (F) ->
                fun
                    (0) -> ?assertEqual(A, B);
                    (N) -> 
                        case ((A) == (B)) of
                            true -> true;
                            false -> timer:sleep(40), F(N - 1)
                        end
                end
        end))(10)).

%%
%% Tests
%%

not_started_test() ->
    ?assertEqual({error, not_started}, app_status:get_status(not_started_test)),
    ok.

first_test() -> 
    setup(),
    app_status:expect(first_test1, first_test2),
    ?assertEqual({waiting, [first_test2]}, app_status:get_status(first_test1)),
    ?assertEqual(not_seen, app_status:get_status(first_test2)),
    app_status:ready(first_test1),
    ?assertEqual({waiting, [first_test2]}, app_status:get_status(first_test1)),
    ?assertEqual(not_seen, app_status:get_status(first_test2)),
    app_status:expect(first_test1, first_test2),
    ?assertEqual({waiting, [first_test2]}, app_status:get_status(first_test1)),
    ?assertEqual(not_seen, app_status:get_status(first_test2)),
    app_status:ready(first_test2),
    ?assertEqual(ready, app_status:get_status(first_test1)),
    ?assertEqual(ready, app_status:get_status(first_test2)),
    ok.

easy_deadlock_test() ->
    setup(),
    ?assertEqual(ok, app_status:expect(easy_deadlock_test1, easy_deadlock_test2)),
    ?assertEqual({error, deadlock}, app_status:expect(easy_deadlock_test2, easy_deadlock_test1)),
    ?assertEqual({waiting, [easy_deadlock_test2]}, app_status:get_status(easy_deadlock_test1)),
    ?assertEqual(not_seen, app_status:get_status(easy_deadlock_test2)),
    ok.

wait_test() ->
    setup(),
    Fun = fun (P) -> 
            send_(P, app_status:wait(wait_test1))
    end,
    delayed(Fun) ! tick,
    app_status:ready(wait_test1),
    ?assertEqual({recv, ok}, recv_()),
    ?assertEqual(ok, app_status:wait(wait_test1)),
    app_status:initializing(wait_test1),
    ?assertEqual(initializing, app_status:get_status(wait_test1)),
    delayed(Fun) ! tick,
    timer:sleep(10),
    app_status:ready(wait_test1),
    ?assertEqual({recv, ok}, recv_()),
    ok.

wait_timeout_test() ->
    setup(),
    {Time, Ans} = timer:tc(app_status, wait, [wait_timeout_test1, 100]),
    ?assertMatch(T when T > 100000, Time),
    ?assertEqual({error, timeout}, Ans),
    ?assertEqual(no_msg, receive X -> {msg, X} after 100 -> no_msg end),
    app_status:ready(wait_timeout_test1),
    ?assertEqual(no_msg, receive X -> {msg, X} after 100 -> no_msg end),
    ok.

monitor_test() ->
    setup(),
    Self = self(),
    Fun = fun (_) ->
            app_status:ready_monitor(monitor_test1),
            send_(Self, hi),
            recv_()
    end,
    Pid = delayed(Fun),
    Pid ! tick,
    recv_(),
    ?assertEqual(ready, app_status:get_status(monitor_test1)),
    send_(Pid, die),
    ?untilEqual(dead, app_status:get_status(monitor_test1)),
    ok.

dead_lock_test() ->
    setup(),
    app_status:expect(a, b),
    app_status:expect(b, c),
    app_status:expect(c, d),
    app_status:expect(c, e),
    ?assertEqual({waiting, [b]}, app_status:get_status(a)),
    ?assertEqual({waiting, [c]}, app_status:get_status(b)),
    ?assertEqual({waiting, [d, e]}, app_status:get_status(c)),
    ?assertEqual(not_seen, app_status:get_status(d)),
    ?assertEqual(not_seen, app_status:get_status(e)),
    ?assertEqual({error, deadlock}, app_status:expect(e, a)),
    ?assertEqual({error, deadlock}, app_status:expect(e, b)),
    ?assertEqual({error, deadlock}, app_status:expect(e, c)),
    ?assertEqual(ok, app_status:expect(e, d)),

    [?assertEqual({error, deadlock}, app_status:expect_wait(d, App)) 
     || App <- [a, b, c, d, e]],
    [app_status:ready(App) || App <- [a, b, c, d, e]],
    [?assertEqual(ready, app_status:get_status(App)) || App <- [a, b, c, d, e]],
    ok.

notify_test() ->
    setup(),
    Recv = fun () -> receive X -> {msg, X} after 50 -> no_msg end end,

    app_status:notify(notify_test1),
    ?assertEqual(no_msg, Recv()),
    app_status:expect(notify_test1, notify_test2),
    app_status:ready(notify_test2),
    ?assertEqual(no_msg, Recv()),
    app_status:ready(notify_test1),
    ?assertEqual({msg, {app_status, notify_test1, ready}}, Recv()),
    ?assertEqual(no_msg, Recv()),
    app_status:dead(notify_test1),
    ?assertEqual({msg, {app_status, notify_test1, dead}}, Recv()),
    ?assertEqual(no_msg, Recv()),
    app_status:ready(notify_test1),
    ?assertEqual({msg, {app_status, notify_test1, ready}}, Recv()),
    ?assertEqual(no_msg, Recv()),
    % Dependecy dead
    app_status:dead(notify_test2),
    ?assertEqual({msg, {app_status, notify_test1, {waiting, [notify_test2]}}}, Recv()),
    ?assertEqual(no_msg, Recv()),
    app_status:ready(notify_test2),
    ?assertEqual({msg, {app_status, notify_test1, ready}}, Recv()),
    ?assertEqual(no_msg, Recv()),
    ok.


hard_traverse_notify_test() ->
    Exp = fun (A, B) ->
            ?assertEqual(ok,
                         app_status:expect({hard_traverse_test, A}, [{hard_traverse_test, BItem} || BItem <- B]))
    end,
    Recv = fun () -> receive X -> {msg, X} after 50 -> no_msg end end,
    Exp(c2, [d1]),
    Exp(b2, [c1, c2]),
    Exp(c1, [d1]),
    Exp(b3, [c2]),
    Exp(b1, [c1]),
    Exp(a1, [b1, b2, b3]),
    [app_status:ready({hard_traverse_test, X}) 
     || X <- [b1, c1, d1, b2, c2, a1, b3]],
    [?assertEqual(ready, app_status:get_status({hard_traverse_test, X}))
     || X <- [b1, c1, d1, b2, c2, a1, b3]],

    %% notify test
    app_status:notify({hard_traverse_test, a1}),
    app_status:notify({hard_traverse_test, b3}),
    ?assertEqual(no_msg, Recv()),
    app_status:initializing({hard_traverse_test, c1}),
    ?assertMatch({msg, {app_status, {hard_traverse_test, a1}, {waiting, [{hard_traverse_test, B}]}}}
                 when B == b1; B == b2,
                 Recv()),
    ?assertEqual(no_msg, Recv()),
    app_status:initializing({hard_traverse_test, c2}),
    ?assertEqual({msg, {app_status, {hard_traverse_test, b3}, {waiting, [{hard_traverse_test, c2}]}}},
                 Recv()),
    ?assertEqual(no_msg, Recv()),
    app_status:ready({hard_traverse_test, c1}),
    ?assertEqual(no_msg, Recv()),
    app_status:ready({hard_traverse_test, c2}),
    [Msg1, Msg2] = lists:usort([Recv(), Recv()]),
    ?assertEqual({msg, {app_status, {hard_traverse_test, a1}, ready}}, Msg1),
    ?assertEqual({msg, {app_status, {hard_traverse_test, b3}, ready}}, Msg2),
    ?assertEqual(no_msg, Recv()),
    ok.
