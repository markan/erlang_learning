%%%-------------------------------------------------------------------
%%% @author Mark Anderson <>
%%% @copyright (C) 2016, Mark Anderson
%%% @doc
%%%
%%% @end
%%% Created :  3 Oct 2016 by Mark Anderson <>
%%%-------------------------------------------------------------------
-module(fizzbuzz_gen_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
         ask/1,
         slow_ask/1,
         stats/0,
         clear_stats/0
        ]).


%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { requests,
                 replies  } ).

%%%===================================================================
%%% API
%%%===================================================================
ask(Number) ->
    gen_server:call(?MODULE, {isfizzbuzz, Number}).

slow_ask(Number) ->
    gen_server:call(?MODULE, {slow_isfizzbuzz, Number}).

stats() ->
    gen_server:call(?MODULE, stats).

clear_stats()->
    gen_server:cast(?MODULE, clear).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{ requests = 0, replies = 0 }}.

handle_call({isfizzbuzz, Number}, _From, #state{requests=Requests, replies=Replies}) ->
    State1 = #state{requests=Requests+1, replies=Replies+1},
    Result = {fizzbuzz(Number), Number},
    {reply, Result, State1};
handle_call({slow_isfizzbuzz, Number}, From, #state{requests=Requests} = State) ->
    spawn_link(fun () -> slow_fizzbuzz(self(), From, Number) end),
    State1 = State#state{requests=Requests+1},
    {noreply, State1};
handle_call(stats, _From, #state{requests=Requests, replies=Replies} = State) ->
    Reply = {stats, Requests, Replies},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    io:format("Unexpected call ~p ~p", [_Request, _From]),
    {noreply, State}.

handle_cast(clear, State) ->
    {noreply, State#state{requests=0, replies=0}};
handle_cast(_Msg, State) ->
    io:format("Unexpected cast ~p", [_Msg]),
    {noreply, State}.

handle_info({slow_fizzbuzz_result, Requester, Result}, #state{replies=Replies}=State) ->
    gen_server:reply(Requester, Result),
    {noreply, State#state{replies=Replies+1} };
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


fizzbuzz(Number) ->
    Fizz = (Number rem 5) == 0,
    Buzz = (Number rem 3) == 0,
    case {Fizz, Buzz} of
        {true, true} ->
            fizzbuzz;
        {true, false} ->
            fizz;
        {false, true} ->
            buzz;
        _ ->
            none
    end.

slow_fizzbuzz(Server, Requester, Number) ->
    timer:sleep(5*1000), % millisecs.
    Server ! {slow_fizzbuzz_result, Requester, {fizzbuzz(Number), Number} }.
