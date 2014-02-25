-module(app_status_provider_application).
-behaviour(gen_server).

-export([start_link/0,
         update/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update() ->
    gen_server:call(?MODULE, update).

-record(state, {update_period :: non_neg_integer()}).

init([]) ->
    erlang:send_after(2000, self(), bump),
    {ok, #state{update_period = 100}}.

handle_call(update, _From, State) ->
    update_ll(),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    update_ll(),
    Time = State#state.update_period,
    erlang:send_after(Time, self(), bump),
    {noreply, State#state{update_period = erlang:min(10000, Time + erlang:abs(10500 - Time) div 10)}, hibernate}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    update_ll(),
    {ok, State}.


update_ll() ->
    StartedAppsSet = sets:from_list([element(1, App) || App <- application:which_applications()]),
    ToStartSet = app_status_server:fold(StartedAppsSet,
        fun
            ({{application, App}, _AnyState, []}, Set) ->
                case sets:is_element(App, Set) of
                    true ->
                        sets:del_element(App, Set);
                    false ->
                        app_status:dead({application, App}),
                        Set
                end;
            (_, Set) ->
                Set
        end),
    [app_status:ready({application, App}) || App <- sets:to_list(ToStartSet)],
    ok.


                    
