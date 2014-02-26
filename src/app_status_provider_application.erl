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


-ifdef(TEST).
-export([new_time/1]).
-endif.

%%
%% API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update() ->
    gen_server:call(?MODULE, update).

%%
%% GenServer's callbacks
%%

-record(state, {update_period :: non_neg_integer()}).

init([]) ->
    erlang:send_after(1000, self(), bump),
    {ok, #state{update_period = config(init_time)}}.

handle_call(update, _From, State) ->
    update_ll(),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    update_ll(),
    Time = State#state.update_period,
    erlang:send_after(Time, self(), bump),
    {noreply, State#state{update_period = new_time(Time)}, hibernate}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    update_ll(),
    {ok, State}.

%%
%% Internals
%%

get_env(Key, Default) ->
    case application:get_env(app_status, Key) of
        undefined -> Default;
        Value     -> Value
    end.

config(init_time)  -> get_env(provider_init_time, 100);
config(multiplier) -> get_env(provider_multiplier, 8 / 7);
config(max_time)   -> get_env(provider_max_time, 10000).

new_time(Old) ->
    NewTime = round(Old * config(multiplier)),
    erlang:min(NewTime, config(max_time)).

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
    [app_status:ready({application, App}, [no_monitor]) || App <- sets:to_list(ToStartSet)],
    ok.


% vim: ts=4 sw=4 et
