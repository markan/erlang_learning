
-module(simple_server).

-export([start/0,
         ask/2,
         slow_ask/2,
         stats/1,
         loop/1
        ]).


-record(state, { requests,
                 replies  } ).


start() ->
    erlang:spawn(?MODULE, loop, [#state{ requests = 0, replies = 0 }]).

ask(Pid, Number) ->
    Pid ! {isfizzbuzz, self(),  Number},
    receive
        { Answer, Number } ->
           Answer;
        X ->
           {unexpected, X}
    end.

slow_ask(Pid, Number) ->
    Pid ! {slow_isfizzbuzz, self(),  Number},
    receive
        { Answer, Number } ->
           Answer;
        X ->
           {unexpected, X}
    end.

stats(Pid) ->
    Pid ! {stats, self()},
    receive
        {stats, Requests, Replies} ->
            {stats, Requests, Replies};
        X ->
            {unexpected, X}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% implementation

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
    Server ! {slow_fizzbuzz_result, Requester, {fizzbuzz(Number), Number}}.

loop(#state{requests=Requests, replies=Replies}=State) ->
    State1 =
        receive
            {isfizzbuzz, From, Number} ->
                From ! {fizzbuzz(Number), Number},
                State#state{requests=Requests+1, replies=Replies+1};
            {slow_isfizzbuzz, From, Number} ->
                Self = self(),
                spawn_link(fun () -> slow_fizzbuzz(Self, From, Number) end),
                State#state{requests=Requests+1};
            {slow_fizzbuzz_result, Requester, Answer} ->
                Requester ! Answer,
                State#state{replies=Replies+1};
            {stats, From} ->
                From ! {stats, State#state.requests, State#state.replies},
                State
        end,
    loop(State1).
