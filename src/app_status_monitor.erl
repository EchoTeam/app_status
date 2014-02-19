-module(app_status_monitor).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([monitor/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

monitor(Name, Pid) ->
    gen_server:call(?MODULE, {monitor, Name, Pid}).


%% 
%% GenServer's callbacks
%%

-record(state, {
        monitor :: ets:tab() %% ref()  -> name()
        }).

init([]) ->
    State = #state{
            monitor = ets:new(monitor, [private])
            },
    {ok, State}.

handle_call({monitor, Name, Pid}, _From, State) ->
    Ref = erlang:monitor(process, Pid),
    ets:insert(State#state.monitor, [{Ref, Name}]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    case ets:lookup(State#state.monitor, Ref) of
        [{_, Name}] ->
            ets:delete(State#state.monitor, Ref),
            app_status:dead(Name);
        _ ->
            lager:info("Unknown process ~p died under monitor", [_Pid])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

