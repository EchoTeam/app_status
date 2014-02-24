-module(app_status_monitor).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([monitor/2,
         demonitor/2,
         demonitor_all/1
        ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

monitor(Name, Pid) ->
    gen_server:call(?MODULE, {monitor, Name, Pid}).

demonitor(Name, Pid) ->
    gen_server:call(?MODULE, {demonitor, Name, Pid}).

demonitor_all(Name) ->
    gen_server:call(?MODULE, {demonitor_all, Name}).


%% 
%% GenServer's callbacks
%%

-record(state, {
        ref2name :: ets:tab(),   %% ref()  -> name()
        name2pidref :: ets:tab() %% name() -> {pid(), ref()}
        }).

init(_) ->
    State = #state{
            ref2name = ets:new(ref2name, [private]),
            name2pidref = ets:new(name2pidref, [bag, private])
            },
    {ok, State}.

handle_call({monitor, Name, Pid}, _From, State) ->
    Ref = erlang:monitor(process, Pid),
    ets:insert(State#state.ref2name, [{Ref, Name}]),
    ets:insert(State#state.name2pidref, [{Name, Pid, Ref}]),
    {reply, ok, State};
handle_call({demonitor, Name, Pid}, _From, State) ->
    List = ets:lookup(State#state.name2pidref, Name),
    [cleanup(Name, Pid, Ref, State) || {_Name, Pid_, Ref} <- List, Pid == Pid_],
    {reply, ok, State};
handle_call({demonitor_all, Name}, _From, State) ->
    List = ets:lookup(State#state.name2pidref, Name),
    [cleanup(Name, Pid, Ref, State) || {_Name, Pid, Ref} <- List],
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    case ets:lookup(State#state.ref2name, Ref) of
        [{_, Name}] ->
            cleanup(Name, Pid, Ref, State),
            spawn(app_status, dead, [Name]);
        _ ->
            lager:info("Unknown process ~p died under monitor", [Pid])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Internals
%%

cleanup(Name, Pid, Ref, State) ->
    ets:delete_object(State#state.name2pidref, {Name, Pid, Ref}),
    ets:delete_object(State#state.ref2name, {Ref, Name}),
    erlang:demonitor(Ref),
    ok.

