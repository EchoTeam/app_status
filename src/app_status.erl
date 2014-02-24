-module(app_status).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([expect/2,
         wait/1,
         wait/2,
         initializing/1,
         ready/1,
         dead/1,
         get_status/1,
         notify/1,
         notify/2,
         monitor/1,
         monitor/2
        ]).

-export([i/0]).

-export([fold/2]).

-type name()            :: term().
-type internal_status() :: not_seen | initializing | ready | dead.
-type status()          :: {waiting, [name()]} | internal_status().
-type expectations()    :: name() | [name()].

-define(WAIT_TIMEOUT, 300000). %% 5m
-define(status_tab, app_status_tab).

%%
%% API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%
%% An API for applications
%%

-spec expect(name(), expectations()) -> ok | {error, deadlock}.
expect(Name, Expected) when is_list(Expected) ->
    gen_server:call(?MODULE, {expect, Name, lists:usort(Expected)});
expect(Name, Expected) ->
    expect(Name, [Expected]).

-spec wait(name()) -> ok | {error, timeout}.
wait(Name) ->
    wait(Name, ?WAIT_TIMEOUT).

-spec wait(name(), timeout()) -> ok | {error, timeout}.
wait(Name, Timeout) ->
    Ref = make_ref(),
    try gen_server:call(?MODULE, {wait_for, Name, Ref}, Timeout)
    catch
        exit:{timeout, _} ->
            _ = gen_server:call(?MODULE, {no_longer_wait_for, Name, Ref}),
            {error, timeout}
    end.

-spec initializing(name()) -> ok.
initializing(Name) ->
    lager:info("[app_status] `~p` switching it's status to initializing", [Name]),
    gen_server:call(?MODULE, {set_status, Name, initializing}).

-spec ready(name()) -> ok.
ready(Name) ->
    lager:info("[app_status] `~p` switching it's status to ready", [Name]),
    gen_server:call(?MODULE, {set_status, Name, ready}).

-spec dead(name()) -> ok.
dead(Name) ->
    lager:info("[app_status] `~p` now dead", [Name]),
    gen_server:call(?MODULE, {set_status, Name, dead}).

-spec get_status(name()) -> status() | {error, not_started}.
get_status(Name) ->
    try ets:lookup(?status_tab, Name) of
        [{_Name, Status, []}] ->
            Status;
        [{_Name, _Status, WaitingList}] ->
            {waiting, WaitingList};
        [] ->
            not_seen
    catch
        error:badarg ->
            {error, not_started}
    end.

-spec notify(name()) -> ok.
notify(Name) ->
    notify(Name, self()).

-spec notify(name(), pid()) -> ok.
notify(Name, Pid) ->
    gen_server:call(?MODULE, {notify, Name, Pid}).

-spec monitor(name()) -> ok.
monitor(Name) ->
    ?MODULE:monitor(Name, self()).

-spec monitor(name(), pid()) -> ok.
monitor(Name, Pid) ->
    app_status_monitor:monitor(Name, Pid).

%%
%% Debug API
%%

-spec i() -> [{name(), internal_status(), [name()]}].
i() ->
    ets:tab2list(?status_tab).

%%
%% Providers API
%%

-spec fold(Acc, fun(({name(), internal_status(), [name()]}, Acc) -> Acc)) -> Acc.
fold(InitAcc, Fun) when is_function(Fun, 2) ->
    ets:foldl(Fun, InitAcc, ?status_tab).


%%
%% GenServer's callbacks
%%

-record(state, {
        status :: ets:tab(), %% name() -> {internal_status(), [name()]}
        exps   :: ets:tab(), %% name() -> [name()]
        exps_r :: ets:tab(), %% name() -> [name()]
        waits  :: ets:tab(), %% name() -> [{ref, From}]
        waits_r:: ets:tab(), %% {name(), ref()} -> [From]
        notify :: ets:tab()  %% name() -> [pid()]
        }).

init(_) ->
    State = #state{
            status = ets:new(?status_tab, [named_table, protected]),
            exps   = ets:new(exps,   [bag, private]),
            exps_r = ets:new(exps_r, [bag, private]),
            waits  = ets:new(waits,  [bag, private]),
            waits_r= ets:new(waits,  [bag, private]),
            notify = ets:new(notify, [bag, private])
            },
    {ok, State}.

handle_call({expect, Name, ExpectList}, _From, State) ->
    NewExpected = ExpectList -- bag_lookup_element(State#state.exps, Name, 2),
    case is_deadlock_caused(Name, NewExpected, State#state.exps) of
        true ->
            {reply, {error, deadlock}, State};
        false ->
            NewState = commit_expect(Name, NewExpected, State),
            {reply, ok, NewState}
    end;
handle_call({set_status, Name, Status}, _From, State) ->
    update_tree(Name, Status, State),
    {reply, ok, State};
handle_call({wait_for, Name, Ref}, From, State) ->
    case get_unresolved_expectations(Name) of
        [] ->
            {reply, ok, State};
        [_|_] ->
            ets:insert(State#state.waits  , [{Name, {Ref, From}}]),
            ets:insert(State#state.waits_r, [{{Name, Ref}, From}]),
            {noreply, State}
    end;
handle_call({no_longer_wait_for, Name, Ref}, _From, State) ->
    case ets:lookup(State#state.waits_r, {Name, Ref}) of
        [{{Name, Ref}, From}] ->
            delete_from_waits(Name, Ref, From, State),
            {reply, ok, State};
        [] ->
            {reply, {error, already_sent}, State}
    end;
handle_call({notify, Name, Pid}, _From, State) ->
    ets:insert(State#state.notify, [{Name, Pid}]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    lager:info("[app_status] Unknown handle_call request from ~p: ~p", [_From, _Request]),
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    lager:info("[app_status] Unknown handle_cast message: ~p", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    lager:info("[app_status] Unknown handle_info message: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Internals
%%

-spec is_deadlock_caused(name(), [name()], ets:tab()) -> boolean().
is_deadlock_caused(Name, Expected, OldExpetedTab) ->
    traverse(OldExpetedTab, Expected, false,
        fun 
            (Elem, _) when Elem =:= Name -> {stop, true};
            (_, _) -> {continue, false}
        end).

-type traversorFun(E, A) :: fun((E, A) -> {continue | stop, A}).
-spec traverse(Tab :: ets:tab(), Backlog :: [name()], Res, Fun :: traversorFun(name(), Res)) 
    -> Res when Res :: any().
traverse(ExpectTab, Backlog, Init, Fun) when is_list(Backlog), is_function(Fun, 2) ->
    traverse(ExpectTab, Backlog, sets:new(), Init, Fun).

traverse(_Tab, [], _Visited, Acc, _Fun) -> Acc;
traverse(Tab, [Current | Backlog], VisitedSet, Acc, Fun) ->
    case sets:is_element(Current, VisitedSet) of
        true ->
            traverse(Tab, Backlog, VisitedSet, Acc, Fun);
        false ->
            case Fun(Current, Acc) of
                {stop, NewAcc} ->
                    NewAcc;
                {continue, NewAcc} ->
                    NewSet = sets:add_element(Current, VisitedSet),
                    Neighbors = bag_lookup_element(Tab, Current, 2),
                    traverse(Tab, Neighbors ++ Backlog, NewSet, NewAcc, Fun)
            end
    end.

-spec commit_expect(name(), [name()], #state{}) -> #state{}.
commit_expect(Name, ExpectedList, State) ->
    ets:insert(State#state.exps,   [{Name, Expected} || Expected <- ExpectedList]),
    ets:insert(State#state.exps_r, [{Expected, Name} || Expected <- ExpectedList]),
    NewStatus = case get_internal_status(Name) of
        not_seen -> initializing;
        S -> S
    end,
    update_tree(Name, NewStatus, State),
    State.

-spec update_tree(name(), internal_status(), #state{}) -> ok.
update_tree(Root, NewStatus, State) ->
    OldUnResolved = get_unresolved_expectations(Root),
    NewUnResolved = [Exp || Exp <- bag_lookup_element(State#state.exps, Root, 2),
                            get_status(Exp) /= ready],
    case {OldUnResolved, NewUnResolved} of
        {Same, Same} ->
            nop;
        {_Old, []} ->
            [begin
                delete_from_waits(Root, Ref, From, State),
                gen_server:reply(From, ok)
             end
             || {Ref, From} <- bag_lookup_element(State#state.waits, Root, 2)],
            set_unresolved_expectations(Root, [], State);
        {_Old, New} -> 
            set_unresolved_expectations(Root, New, State)
    end,
    OldStatus = get_internal_status(Root),
    OldStatus /= NewStatus andalso set_internal_status(Root, NewStatus, State),
    Escalate = case {OldStatus, OldUnResolved, NewStatus, NewUnResolved} of
        {ready, [], N, L} when N /= ready; L /= [] ->
            true;
        {N, L, ready, []} when N /= ready; L /= [] ->
            true;
        {_, _, _, _} ->
            false
    end,
    case Escalate of
        true ->
            notify_ll(Root, get_status(Root), State),
            [update_tree(User, get_internal_status(User), State) ||
                User <- bag_lookup_element(State#state.exps_r, Root, 2)],
            ok;
        false ->
            ok
    end.


-spec notify_ll(name(), any(), #state{}) -> ok.
notify_ll(Name, Msg, #state{notify = NotifyTab}) ->
    [Client ! {?MODULE, Name, Msg} || Client <- bag_lookup_element(NotifyTab, Name, 2)],
    ok.

-spec delete_from_waits(name(), reference(), gen_server:from(), #state{}) -> ok.
delete_from_waits(Name, Ref, From, State) ->
    ets:delete_object(State#state.waits  , {Name, {Ref, From}}),
    ets:delete_object(State#state.waits_r, {{Name, Ref}, From}),
    ok.

%%
%% Utils
%%

bag_lookup_element(Tab, Key, Pos) ->
    try ets:lookup_element(Tab, Key, Pos)
    catch
        error:badarg ->
            []
    end.


-spec set_internal_status(name(), internal_status(), #state{}) -> ok.
set_internal_status(Name, Status, _State) ->
    case ets:update_element(?status_tab, Name, {2, Status}) of
        false -> ets:insert(?status_tab, [{Name, Status, []}]);
        true  -> ok 
    end.

-spec get_internal_status(name()) -> internal_status().
get_internal_status(Name) ->
    case ets:lookup(?status_tab, Name) of
        [{_Name, Status, _Waiting}] ->
            Status;
        [] -> 
            not_seen
    end.

-spec get_unresolved_expectations(name()) -> [name()].
get_unresolved_expectations(Name) ->
    bag_lookup_element(?status_tab, Name, 3).

-spec set_unresolved_expectations(name(), [name()], #state{}) -> ok.
set_unresolved_expectations(Name, ExpList, _State) ->
    case ets:update_element(?status_tab, Name, {3, ExpList}) of
        true  -> ok;
        false ->
            ets:insert(?status_tab, [{Name, not_seen, ExpList}])
    end.

