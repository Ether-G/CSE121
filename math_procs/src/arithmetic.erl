-module(arithmetic).
-export([start_factorializer/0, start_adder/0, start_subtracter/0, start_multiplier/0, start_divider/0,
         factorializer/0, adder/0, subtracter/0, multiplier/0, divider/0,
         factorial_of/2, add/3, subtract/3, multiply/3, divide/3]).

%% Client Interface Functions

factorial_of(Server, N) when is_integer(N), N >= 0 ->
    case is_pid(Server) of
        true -> Server ! {self(), factorial, N};
        false -> whereis(Server) ! {self(), factorial, N}
    end,
    receive
        {_From, Result} -> Result
    after 5000 ->
        timeout
    end;
factorial_of(_Server, N) when is_integer(N) ->
    {fail, N, is_negative};
factorial_of(_Server, N) when is_float(N) ->
    {fail, N, is_not_integer};
factorial_of(_Server, N) ->
    {fail, N, is_not_integer}.

add(Server, X, Y) when is_number(X), is_number(Y) ->
    case is_pid(Server) of
        true -> Server ! {self(), add, X, Y};
        false -> whereis(Server) ! {self(), add, X, Y}
    end,
    receive
        {_From, Result} -> Result
    after 5000 ->
        timeout
    end;
add(_Server, X, _Y) when not is_number(X) ->
    {fail, X, is_not_number};
add(_Server, _X, Y) ->
    {fail, Y, is_not_number}.

subtract(Server, X, Y) when is_number(X), is_number(Y) ->
    case is_pid(Server) of
        true -> Server ! {self(), subtract, X, Y};
        false -> whereis(Server) ! {self(), subtract, X, Y}
    end,
    receive
        {_From, Result} -> Result
    after 5000 ->
        timeout
    end;
subtract(_Server, X, _Y) when not is_number(X) ->
    {fail, X, is_not_number};
subtract(_Server, _X, Y) ->
    {fail, Y, is_not_number}.

multiply(Server, X, Y) when is_number(X), is_number(Y) ->
    case is_pid(Server) of
        true -> Server ! {self(), multiply, X, Y};
        false -> whereis(Server) ! {self(), multiply, X, Y}
    end,
    receive
        {_From, Result} -> Result
    after 5000 ->
        timeout
    end;
multiply(_Server, X, _Y) when not is_number(X) ->
    {fail, X, is_not_number};
multiply(_Server, _X, Y) ->
    {fail, Y, is_not_number}.

divide(Server, X, Y) when is_number(X), is_number(Y) ->
    case is_pid(Server) of
        true -> Server ! {self(), divide, X, Y};
        false -> whereis(Server) ! {self(), divide, X, Y}
    end,
    receive
        {_From, Result} -> Result
    after 5000 ->
        timeout
    end;
divide(_Server, X, _Y) when not is_number(X) ->
    {fail, X, is_not_number};
divide(_Server, _X, Y) ->
    {fail, Y, is_not_number}.

%% Server Process Functions

factorializer() ->
    receive
        {From, factorial, N} ->
            Result = calc_factorial(N),
            From ! {self(), Result},
            factorializer();
        _ ->
            factorializer()
    end.

adder() ->
    receive
        {From, add, X, Y} ->
            From ! {self(), X + Y},
            adder();
        _ ->
            adder()
    end.

subtracter() ->
    receive
        {From, subtract, X, Y} ->
            From ! {self(), X - Y},
            subtracter();
        _ ->
            subtracter()
    end.

multiplier() ->
    receive
        {From, multiply, X, Y} ->
            From ! {self(), X * Y},
            multiplier();
        _ ->
            multiplier()
    end.

divider() ->
    receive
        {From, divide, X, Y} ->
            From ! {self(), X / Y},
            divider();
        _ ->
            divider()
    end.

%% Start Functions

start_factorializer() ->
    register(factorializer, spawn(?MODULE, factorializer, [])).

start_adder() ->
    register(adder, spawn(?MODULE, adder, [])).

start_subtracter() ->
    register(subtracter, spawn(?MODULE, subtracter, [])).

start_multiplier() ->
    register(multiplier, spawn(?MODULE, multiplier, [])).

start_divider() ->
    register(divider, spawn(?MODULE, divider, [])).

%% Helper Functions

calc_factorial(0) -> 1;
calc_factorial(N) -> N * calc_factorial(N-1).

-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%

-include_lib("eunit/include/eunit.hrl").

factorializer_test_() ->
{setup, 
    fun() -> % runs before any of the tests
            Pid = spawn(?MODULE, factorializer, []),     
            register(test_factorializer, Pid)
        end, 
    % fun(_)-> % runs after all of the tests
        % there is no teardown needed, so this fun doesn't need to be implemented.
    % end, 
    % factorializer tests start here
    [ ?_assertEqual(120,  factorial_of(test_factorializer,  5)),  % happy path, tests the obvious case.
      % test less obvious or edge cases
      ?_assertEqual(1, factorial_of(test_factorializer, 0)), 
      ?_assertEqual({fail, -3, is_negative}, factorial_of(test_factorializer, -3)), 
      ?_assertEqual({fail, bob, is_not_integer}, factorial_of(test_factorializer, bob)), 
      ?_assertEqual({fail, 5.0, is_not_integer}, factorial_of(test_factorializer, 5.0))
    ]
}.

adder_test_() ->
{setup, 
    fun()->%runs before any of the tests
            Pid = spawn(?MODULE, adder, []),     
            register(test_adder, Pid)
        end, 
    %fun(_)->%runs after all of the tests
        %there is no teardown needed, so this fun doesn't need to be implemented.
    %end, 
    [ ?_assertEqual(8, add(test_adder, 5, 3)), %happy path
      % test less obvious or edge cases
      ?_assertEqual(0, add(test_adder, 0, 0)), 
      ?_assertEqual(0.0, add(test_adder, 0.0, 0.0)), 
      ?_assertEqual(0, add(test_adder, -5, 5)), 
      ?_assertEqual(1.5, add(test_adder, 0.75, 0.75)), 
      ?_assertEqual({fail, bob, is_not_number}, add(test_adder, bob, 3)), 
      ?_assertEqual({fail, sue, is_not_number}, add(test_adder, 3, sue)), 
      ?_assertEqual({fail, bob, is_not_number}, add(test_adder, bob, sue))
    ]
}.

subtracter_test_() ->
{setup, 
    fun()->%runs before any of the tests
            Pid = spawn(?MODULE, subtracter, []),     
            register(test_subtracter, Pid)
        end, 
    [ ?_assertEqual(2, subtract(test_subtracter, 5, 3)), %happy path
      % test less obvious or edge cases
      ?_assertEqual(0, subtract(test_subtracter, 0, 0)), 
      ?_assertEqual(0.0, subtract(test_subtracter, 0.0, 0.0)), 
      ?_assertEqual(-10, subtract(test_subtracter, -5, 5)), 
      ?_assertEqual(0.75, subtract(test_subtracter, 1.5, 0.75)), 
      ?_assertEqual({fail, bob, is_not_number}, subtract(test_subtracter, bob, 3)), 
      ?_assertEqual({fail, sue, is_not_number}, subtract(test_subtracter, 3, sue)), 
      ?_assertEqual({fail, bob, is_not_number}, subtract(test_subtracter, bob, sue))
    ]
}.

multiplier_test_() ->
{setup, 
    fun()->%runs before any of the tests
            Pid = spawn(?MODULE, multiplier, []),     
            register(test_multiplier, Pid)
        end, 
    [ ?_assertEqual(15, multiply(test_multiplier, 5, 3)), %happy path
      % test less obvious or edge cases
      ?_assertEqual(0, multiply(test_multiplier, 0, 0)), 
      ?_assertEqual(0.0, multiply(test_multiplier, 0.0, 0.0)), 
      ?_assertEqual(-25, multiply(test_multiplier, -5, 5)), 
      ?_assertEqual(1.125, multiply(test_multiplier, 1.5, 0.75)), 
      ?_assertEqual({fail, bob, is_not_number}, multiply(test_multiplier, bob, 3)), 
      ?_assertEqual({fail, sue, is_not_number}, multiply(test_multiplier, 3, sue)), 
      ?_assertEqual({fail, bob, is_not_number}, multiply(test_multiplier, bob, sue))
    ]
}.

divider_test_() ->
{setup, 
    fun()->%runs before any of the tests
            Pid = spawn(?MODULE, divider, []),     
            register(test_divider, Pid)
        end, 
    [ ?_assert((1.6 < divide(test_divider, 5, 3)) and (divide(test_divider, 5, 3) < 1.7)), %happy path
      % test less obvious or edge cases
      ?_assertEqual(-1.0, divide(test_divider, -5, 5)), 
      ?_assertEqual(2.0, divide(test_divider, 1.5, 0.75)), 
      ?_assertEqual({fail, bob, is_not_number}, divide(test_divider, bob, 3)), 
      ?_assertEqual({fail, sue, is_not_number}, divide(test_divider, 3, sue)), 
      ?_assertEqual({fail, bob, is_not_number}, divide(test_divider, bob, sue))
    ]
}.

-endif.