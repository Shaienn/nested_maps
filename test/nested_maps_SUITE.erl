-module(nested_maps_SUITE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([
    all/0
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction and Destruction for the Test Suite
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([
    init_per_suite/1,
    end_per_suite/1
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([
    test_of_combinations/1,
    simple_get_operation/1,
    wildcard_get_operation/1,
    group_get_operation/1,
    simple_get_width_operation/1,
    wildcard_get_with_operations/1,
    group_get_with_operations/1,
    simple_put_operations/1,
    wildcard_put_operations/1,
    group_put_operations/1,
    simple_put_with_operations/1,
    wildcard_put_with_operations/1,
    group_put_with_operations/1,
    simple_keys_operations/1,
    wildcard_keys_operations/1,
    group_keys_operations/1,
    simple_keys_with_operations/1,
    wildcard_keys_with_operations/1,
    group_keys_with_operations/1,
    simple_update_operations/1,
    wildcard_update_operations/1,
    group_update_operations/1
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Include Files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("common_test/include/ct.hrl").

-define(TRY(ExpectedResult, CodeBlock),
    [eq(ExpectedResult, Result) ||
        Result <- [try CodeBlock catch error:Reason -> Reason end]]).

-define(TRYV(ExpectedResult, CodeBlock),
    [eq2(ExpectedResult, Result) ||
        Result <- [try CodeBlock catch error:Reason -> Reason end]]).

-define(FUN_UPDATE, fun(_Key, Value) when Value > 20 -> Value + 1;
                       (_Key, Value) -> Value - 1 end).
-define(BAD_FUN, fun(_Key, Value) when Value > 10 -> 1 / 0;
                    (_Key, _Value) -> 1 / 0 end).

-define(BAD_FUN_THROW, fun(_, _) -> throw(got_throw) end).
-define(BAD_FUN_ERROR, fun(_, _) -> error(got_error) end).
-define(BAD_FUN_EXIT, fun(_, _) -> exit(got_exit) end).

-define(BAD_FUN_THROW3, fun(_, _, _) -> throw(got_throw) end).
-define(BAD_FUN_ERROR3, fun(_, _, _) -> error(got_error) end).
-define(BAD_FUN_EXIT3, fun(_, _, _) -> exit(got_exit) end).

-define(FUN_PUT, fun(_Key, Value, true) when Value > 10 -> Value + 1;
                    (_Key, Value, true) -> Value - 1;
                    (_Key, _Value, false) -> 100 end).
-define(FUN_KEYS, fun(_K, V) when is_integer(V), V > 10 -> true;
                     (_, _) -> false end).
-define(FUN_GET_REMOVE_TAKE, fun(_Key, Value) -> Value > 20 end).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [
        %%% GET

        simple_get_operation,
        wildcard_get_operation,
        group_get_operation,

        %%% GET_WITH

        simple_get_width_operation,
        wildcard_get_with_operations,
        group_get_with_operations,

        %%% PUT

        simple_put_operations,
        wildcard_put_operations,
        group_put_operations,

        %%% PUT_WITH

        simple_put_with_operations,
        wildcard_put_with_operations,
        group_put_with_operations,

        %%% KEYS
 
        simple_keys_operations,
        wildcard_keys_operations,
        group_keys_operations,

        %%% KEYS_WITH

        simple_keys_with_operations,
        wildcard_keys_with_operations,
        group_keys_with_operations,

        %%% UPDATE

        simple_update_operations,
        wildcard_update_operations,
        group_update_operations
        % test_of_combinations
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction and Destruction for the Test Suite
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_of_combinations(_Config) ->

    Map = reference_map(),
    IntegerMap = reference_map(integer_values),

    %%% UPDATE_WITH
    simple_strict_update_with_operations(IntegerMap),
    simple_non_strict_update_with_operations(IntegerMap),
    simple_strict_update_with_exceptions(IntegerMap),
    simple_non_strict_update_with_exceptions(IntegerMap),

    wildcard_strict_update_with_operations(IntegerMap),
    wildcard_non_strict_update_with_operations(IntegerMap),
    wildcard_strict_update_with_exceptions(IntegerMap),
    wildcard_non_strict_update_with_exceptions(IntegerMap),

    group_strict_update_with_operations(IntegerMap),
    group_non_strict_update_with_operations(IntegerMap),
    group_strict_update_with_exceptions(IntegerMap),
    group_non_strict_update_with_exceptions(IntegerMap),

    %%% REMOVE

    simple_strict_remove_operations(Map),
    simple_non_strict_remove_operations(Map),
    simple_strict_remove_exceptions(Map),
    simple_non_strict_remove_exceptions(Map),

    wildcard_strict_remove_operations(Map),
    wildcard_non_strict_remove_operations(Map),
    wildcard_strict_remove_exceptions(Map),
    wildcard_non_strict_remove_exceptions(Map),

    group_strict_remove_operations(Map),
    group_strict_remove_exceptions(Map),
    group_non_strict_remove_operations(Map),
    group_non_strict_remove_exceptions(Map),

    %%% REMOVE_WITH

    simple_strict_remove_with_operations(IntegerMap),
    simple_non_strict_remove_with_operations(IntegerMap),
    simple_strict_remove_with_exceptions(IntegerMap),
    simple_non_strict_remove_with_exceptions(IntegerMap),

    wildcard_strict_remove_with_operations(IntegerMap),
    wildcard_non_strict_remove_with_operations(IntegerMap),
    wildcard_strict_remove_with_exceptions(IntegerMap),
    wildcard_non_strict_remove_with_exceptions(IntegerMap),

    group_strict_remove_with_operations(IntegerMap),
    group_non_strict_remove_with_operations(IntegerMap),
    group_strict_remove_with_exceptions(IntegerMap),
    group_non_strict_remove_with_exceptions(IntegerMap),

    %%% TAKE

    simple_strict_take_operations(Map),
    simple_non_strict_take_operations(Map),
    simple_strict_take_exceptions(Map),
    simple_non_strict_take_exceptions(Map),

    wildcard_strict_take_operations(Map),
    wildcard_non_strict_take_operations(Map),
    wildcard_strict_take_exceptions(Map),
    wildcard_non_strict_take_exceptions(Map),

    group_strict_take_operations(Map),
    group_strict_take_exceptions(Map),
    group_non_strict_take_operations(Map),
    group_non_strict_take_exceptions(Map),

    %%% TAKE_WITH

    simple_strict_take_with_operations(IntegerMap),
    simple_non_strict_take_with_operations(IntegerMap),
    simple_strict_take_with_exceptions(IntegerMap),
    simple_non_strict_take_with_exceptions(IntegerMap),

    wildcard_strict_take_with_operations(IntegerMap),
    wildcard_non_strict_take_with_operations(IntegerMap),
    wildcard_strict_take_with_exceptions(IntegerMap),
    wildcard_non_strict_take_with_exceptions(IntegerMap),

    group_strict_take_with_operations(IntegerMap),
    group_non_strict_take_with_operations(IntegerMap),
    group_strict_take_with_exceptions(IntegerMap),
    group_non_strict_take_with_exceptions(IntegerMap),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GET Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% SIMPLE

simple_get_operation(_) ->
    Map = reference_map(),
    TestFunctions = [
        fun simple_strict_get_operations/2,
        fun simple_non_strict_get_operations/2,
        fun simple_strict_get_exceptions/2,
        fun simple_non_strict_get_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.


simple_strict_get_operations(Map, Parameters) ->
    ok = common_simple_get_operations(Map, Parameters#{strict => true}).

simple_non_strict_get_operations(Map, Parameters) ->
    ok = common_simple_get_operations(Map, Parameters#{strict => false}),

    ?TRY([], nested_maps:get([[ne_key], [k2_1], [k3_1]], Map, #{strict => false})),
    ?TRY([], nested_maps:get([[ne_key], [k2_1], [ne_key]], Map, #{strict => false})),
    ?TRY([], nested_maps:get([[ne_key], [ne_key], [ne_key]], Map, #{strict => false})),
    ?TRY([], nested_maps:get([[k1_2], [ne_key], [ne_key]], Map, #{strict => false})),
    ?TRY([], nested_maps:get([[k1_2], [k2_1], [ne_key]], Map, #{strict => false})),
    ?TRY([], nested_maps:get([[k1_2], [ne_key]], Map, #{strict => false})),
    ?TRY([], nested_maps:get([[ne_key]], Map, #{strict => false})),

    ?TRY([], nested_maps:get([[]], Map, #{strict => false})),
    ?TRY([], nested_maps:get([[k1_2], []], Map, #{strict => false})),
    ?TRY([], nested_maps:get([[k1_2], [], [k3_1]], Map, #{strict => false})),

    ?TRY([], nested_maps:get([], Map, #{strict => false})),
    ok.

common_simple_get_operations(Map, Parameters) ->
    ?TRY([v1_1], nested_maps:get([[k1_1]], Map, Parameters)),
    ?TRY([v3_1_1], nested_maps:get([[k1_2], [k2_1], [k3_1]], Map, Parameters)),
    ?TRY([v3_2_1], nested_maps:get([[k1_2], [k2_1], [k3_2]], Map, Parameters)),
    ?TRY([v3_2_2], nested_maps:get([[k1_2], [k2_2], [k3_2]], Map, Parameters)),
    ?TRY([v3_3_2], nested_maps:get([[k1_2], [k2_2], [k3_3]], Map, Parameters)),
    ?TRY([#{}], nested_maps:get([[k1_3]], Map, Parameters)),

    ?TRY([#{k3_1 => v3_1_1, k3_2 => v3_2_1}], nested_maps:get([[k1_2], [k2_1]], Map, Parameters)),
    ?TRY([#{k3_2 => v3_2_2, k3_3 => v3_3_2}], nested_maps:get([[k1_2], [k2_2]], Map, Parameters)),
    ?TRY([#{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
            k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}}], nested_maps:get([[k1_2]], Map, Parameters)),
    ok.

simple_strict_get_exceptions(Map, Parameters) ->
    ok = common_simple_get_exceptions(Map, Parameters#{strict => true}),
    ?TRY({badarg, {'Address', []}}, nested_maps:get([], Map, Parameters#{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key], [k2_1], [k3_2]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key], [k2_1], [ne_key]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key], [ne_key], [ne_key]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [ne_key], [ne_key]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [k2_1], [ne_key]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [ne_key]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key]], Map, Parameters#{strict => true})),

    ?TRY({bad_address, [[]]}, nested_maps:get([[]], Map, Parameters#{strict => true})),
    ?TRY({bad_address, [[k1_2], []]}, nested_maps:get([[k1_2], []], Map, Parameters#{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [k3_2]]}, nested_maps:get([[k1_2], [], [k3_2]], Map, Parameters#{strict => true})),
    ok.

simple_non_strict_get_exceptions(Map, Parameters) ->
    ok = common_simple_get_exceptions(Map, Parameters#{strict => false}),
    ok.

common_simple_get_exceptions(Map, Parameters) ->
    ?TRY({badmap, {[k2_1], v1_1}, Map}, nested_maps:get([[k1_1], [k2_1]], Map, Parameters)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:get([[k1_2], [k2_1], [k3_2]], not_a_map, Parameters)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:get(not_a_list, Map, Parameters)),
    ?TRY({badarg, {'Address', [[k1_2], not_a_list, [k3_2]]}},
         nested_maps:get([[k1_2], not_a_list, [k3_2]], Map, Parameters)),
    ?TRY({badarg, {'Options', not_strict}}, nested_maps:get([[k1_2], [k2_1]], Map, not_strict)),
    ok.

%%% WILDCARD

wildcard_get_operation(_) ->
    Map = reference_map(),
    TestFunctions = [   
        fun wildcard_strict_get_operations/2,
        fun wildcard_non_strict_get_operations/2,
        fun wildcard_strict_get_exceptions/2,
        fun wildcard_non_strict_get_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

wildcard_strict_get_operations(Map, Parameters) ->
    ok = common_wildcard_get_operations(Map, Parameters#{strict => true}).

wildcard_non_strict_get_operations(Map, Parameters) ->
    ok = common_wildcard_get_operations(Map, Parameters#{strict => false}),

    ?TRY([], nested_maps:get([[k1_2], [ne_key], '*'], Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:get([[k1_2], '*', [ne_key]], Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:get([[ne_key], [ne_key], '*'], Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:get([[ne_key], [k2_1], '*'], Map, Parameters#{strict => false})),


    ?TRY([v3_1_1], nested_maps:get([[k1_2], '*', [k3_1]], Map, Parameters#{strict => false})),
    ?TRY([v3_3_2], nested_maps:get([[k1_2], '*', [k3_3]], Map, Parameters#{strict => false})),

    ?TRY([], nested_maps:get([[k1_3], '*', [k3_2]], Map, Parameters#{strict => false})),
    ok.

common_wildcard_get_operations(Map, Parameters) ->
    ?TRY([v3_2_2, v3_3_2], nested_maps:get([[k1_2], [k2_2], '*'], Map, Parameters)),
    ?TRY([v3_1_1, v3_2_1, v3_2_2, v3_3_2], nested_maps:get([[k1_2], '*', '*'], Map, Parameters)),
    ?TRY([v3_2_1, v3_2_2], nested_maps:get([[k1_2], '*', [k3_2]], Map, Parameters)),

    ?TRY([#{k3_1 => v3_1_1, k3_2 => v3_2_1}, #{k3_2 => v3_2_2, k3_3 => v3_3_2}],
         nested_maps:get([[k1_2], '*'], Map, Parameters)),
    ?TRY([v1_1, #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                  k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}}, #{}
         ], nested_maps:get(['*'], Map, Parameters)),
    ok.

wildcard_strict_get_exceptions(Map, Parameters) ->
    ok = common_wildcard_get_exceptions(Map, Parameters#{strict => true}),
    ?TRY({badkey, k3_1}, nested_maps:get([[k1_2], '*', [k3_1]], Map, Parameters#{strict => true})),
    ?TRY({badkey, k3_3}, nested_maps:get([[k1_2], '*', [k3_3]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [ne_key], '*'], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], '*', [ne_key]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key], [ne_key], '*'], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key], [k2_1], '*'], Map, Parameters#{strict => true})),
    ?TRY({unreachable_address, [[k1_3], '*', [k3_2]]}, nested_maps:get([[k1_3], '*', [k3_2]], Map, Parameters#{strict => true})),
    ok.

wildcard_non_strict_get_exceptions(Map, Parameters) ->
    ok = common_wildcard_get_exceptions(Map, Parameters#{strict => false}),
    ok.

common_wildcard_get_exceptions(Map, Parameters) ->
    ?TRY({badmap, {'*', v1_1}, Map}, nested_maps:get([[k1_1], '*'], Map, Parameters)),
    case maps:get(parallel, Parameters) of
        true ->
            ?TRYV([{badmap, {'*', v1_1}, Map}, {unreachable_address, ['*', '*', '*']}], nested_maps:get(['*', '*', '*'], Map, Parameters));
        false ->
            ?TRY({badmap, {'*', v1_1}, Map}, nested_maps:get(['*', '*', '*'], Map, Parameters))
    end,
    ok.

%%% GROUP
group_get_operation(_) ->
    Map = reference_map(),
    TestFunctions = [   
        fun group_strict_get_operations/2,
        fun group_strict_get_exceptions/2,
        fun group_non_strict_get_operations/2,
        fun group_non_strict_get_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

group_strict_get_operations(Map, Parameters) ->
    ok = common_group_get_operations(Map, Parameters#{strict => true}).

group_non_strict_get_operations(Map, Parameters) ->
    ok = common_group_get_operations(Map, Parameters#{strict => false}),

    ?TRY([v3_2_1, v3_2_2, v3_3_2], nested_maps:get([[k1_2], [k2_1, k2_2], [k3_2, k3_3]], Map, Parameters#{strict => false})),

    ?TRY([v3_1_1], nested_maps:get([[k1_2], [k2_1], [k3_1, ne_key]], Map, Parameters#{strict => false})),
    ?TRY([v3_2_2], nested_maps:get([[k1_2], [k2_2], [ne_key, k3_2]], Map, Parameters#{strict => false})),
    ?TRY([v3_3_2], nested_maps:get([[k1_2], [k2_2, ne_key], [k3_3]], Map, Parameters#{strict => false})),
    ?TRY([v3_2_1], nested_maps:get([[k1_2, ne_key], [k2_1], [k3_2]], Map, Parameters#{strict => false})),

    ?TRY([v3_1_1], nested_maps:get([[k1_2], [k2_1, ne_key], [k3_1, ne_key]], Map, Parameters#{strict => false})),
    ?TRY([v3_3_2], nested_maps:get([[k1_2], [ne_key, k2_2], [ne_key, k3_3]], Map, Parameters#{strict => false})),

    ?TRY([v3_1_1], nested_maps:get([[k1_2, ne_key], [k2_1, ne_key], [k3_1, ne_key]], Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:get([[ne_key, k1_3], [ne_key, k2_2], [ne_key, k3_3]], Map, Parameters#{strict => false})),
    ok.

common_group_get_operations(Map, Parameters) ->
    ?TRY([v3_1_1, v3_2_1], nested_maps:get([[k1_2], [k2_1], [k3_1, k3_2]], Map, Parameters)),
    ?TRY([v3_2_2, v3_3_2], nested_maps:get([[k1_2], [k2_2], [k3_2, k3_3]], Map, Parameters)),
    ?TRY([v3_2_1, v3_2_2], nested_maps:get([[k1_2], [k2_1, k2_2], [k3_2]], Map, Parameters)),
    ok.

group_strict_get_exceptions(Map, Parameters) ->
    ok = common_group_get_exceptions(Map, Parameters#{strict => true}),

    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [k2_1], [k3_1, ne_key]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [k2_1, ne_key], [k3_1]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [ne_key, k2_2], [k3_2]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2, ne_key], [k2_1], [k3_1]], Map, Parameters#{strict => true})),
    
    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key, k1_3], [k2_2], [k3_2]], Map, Parameters#{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [k2_1, ne_key], [k3_1, ne_key]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [ne_key, k2_2], [ne_key, k3_2]], Map, Parameters#{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2, ne_key], [k2_1, ne_key], [k3_1, ne_key]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key, k1_3], [ne_key, k2_2], [ne_key, k3_2]], Map, Parameters#{strict => true})),
    ok.

group_non_strict_get_exceptions(Map, Parameters) ->
    ok = common_group_get_exceptions(Map, Parameters#{strict => false}),
    ok.

common_group_get_exceptions(Map, Parameters) ->
    ?TRY({badmap, {[k2_1, k2_2], v1_1}, Map}, nested_maps:get([[k1_1], [k2_1, k2_2], [k3_1]], Map, Parameters)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% GET_WITH Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Simple
simple_get_width_operation(_) ->
    Map = reference_map(integer_values),
    TestFunctions = [   
        fun simple_strict_get_with_operations/2,
        fun simple_non_strict_get_with_operations/2,
        fun simple_strict_get_with_exceptions/2,
        fun simple_non_strict_get_with_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.
simple_strict_get_with_operations(Map, Parameters) ->
    ok = common_simple_get_with_operations(Map, Parameters#{strict => true}).

simple_non_strict_get_with_operations(Map, Parameters) ->
    ok = common_simple_get_with_operations(Map, Parameters#{strict => false}),

    ?TRY([], nested_maps:get_with([[k1_2], [k2_1], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:get_with([], ?FUN_GET_REMOVE_TAKE, Map, Parameters#{strict => false})),
    ok.

common_simple_get_with_operations(Map, Parameters) ->
    ?TRY([], nested_maps:get_with([[k1_2], [k2_1], [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Parameters)),
    ?TRY([30], nested_maps:get_with([[k1_2], [k2_2], [k3_2]], ?FUN_GET_REMOVE_TAKE, Map, Parameters)),
    ok.

simple_strict_get_with_exceptions(Map, Parameters) ->
    ok = common_simple_get_with_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:get_with([[k1_2], [k2_1], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, Parameters#{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [ne_key]]},
         nested_maps:get_with([[k1_2], [], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, Parameters#{strict => true})),

    ?TRY({badarg, {'Address', []}}, nested_maps:get_with([], ?FUN_GET_REMOVE_TAKE, Map, Parameters#{strict => true})),
    ok.

simple_non_strict_get_with_exceptions(Map, Parameters) ->
    ok = common_simple_get_with_exceptions(Map, Parameters#{strict => false}),
    ok.

common_simple_get_with_exceptions(Map, Parameters) ->
    Fun1 = fun(V) when V > 20 -> true; (_) -> false end,
    ?TRY({badarg, {'Function', Fun1}}, nested_maps:get_with([[k1_2], [k2_1], [k3_1]], Fun1, Map, Parameters)),

    Fun2 = fun(_K, V) when V > 20 -> not_true; (_, _) -> not_false end,
    ?TRY({badarg, {'Function', Fun2}}, nested_maps:get_with([[k1_2], [k2_1], [k3_1]], Fun2, Map, Parameters)),

    Fun3 = fun(_K, V) when V > 20 -> 1 / 0; (_, _) -> false end,
    ?TRY({badfun, {badarith, '*'}}, nested_maps:get_with([[k1_2], [k2_2], [k3_2]], Fun3, Map, Parameters)),

    ?TRY({badfun, {got_throw, '*'}}, nested_maps:get_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_THROW, Map, Parameters)),
    ?TRY({badfun, {got_error, '*'}}, nested_maps:get_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_ERROR, Map, Parameters)),
    ?TRY({badfun, {got_exit, '*'}}, nested_maps:get_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_EXIT, Map, Parameters)),

    ?TRY({badmap, {[k2_1],v1_1}, Map}, nested_maps:get_with([[k1_1], [k2_1], [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Parameters)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:get_with(not_a_list, ?FUN_GET_REMOVE_TAKE, Map, Parameters)),
    ?TRY({badarg, {'Map', not_a_map}},
         nested_maps:get_with([[k1_2], [k2_1], [k3_1]], ?FUN_GET_REMOVE_TAKE, not_a_map, Parameters)),
    ?TRY({badarg, {'Address', [[k1_2], not_a_list, [k3_2]]}},
         nested_maps:get_with([[k1_2], not_a_list, [k3_2]], ?FUN_GET_REMOVE_TAKE, Map, Parameters)),
    ok.

%%% Wildcard
wildcard_get_with_operations(_) ->
    Map = reference_map(integer_values),
    TestFunctions = [
        fun wildcard_strict_get_with_operations/2,
        fun wildcard_non_strict_get_with_operations/2,
        fun wildcard_strict_get_with_exceptions/2,
        fun wildcard_non_strict_get_with_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.
wildcard_strict_get_with_operations(Map, Parameters) ->
    ok = common_wildcard_get_with_operations(Map, Parameters#{strict => true}).

wildcard_non_strict_get_with_operations(Map, Parameters) ->
    ok = common_wildcard_get_with_operations(Map, Parameters#{strict => false}),
    ?TRY([], nested_maps:get_with([[k1_2], [ne_key], '*'], ?FUN_GET_REMOVE_TAKE, Map, Parameters#{strict => false})),

    ok.

common_wildcard_get_with_operations(Map, Parameters) ->
    ?TRY([], nested_maps:get_with([[k1_2], [k2_1], '*'], ?FUN_GET_REMOVE_TAKE, Map, Parameters)),
    ?TRY([30, 40], nested_maps:get_with([[k1_2], [k2_2], '*'], ?FUN_GET_REMOVE_TAKE, Map, Parameters)),
    ok.

wildcard_strict_get_with_exceptions(Map, Parameters) ->
    ok = common_wildcard_get_with_exceptions(Map, Parameters#{strict => true}),

    ?TRY({unreachable_address, [[k1_3], '*', [k3_1]]},
         nested_maps:get_with([[k1_3], '*', [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Parameters#{strict => true})),

    ok.

wildcard_non_strict_get_with_exceptions(Map, Parameters) ->
    ok = common_wildcard_get_with_exceptions(Map, Parameters#{strict => false}),
    ok.

common_wildcard_get_with_exceptions(_Map, _Parameters) ->
    ok.

%%% Group
group_get_with_operations(_) ->
    Map = reference_map(integer_values),
    TestFunctions = [
        fun group_strict_get_with_operations/2,
        fun group_non_strict_get_with_operations/2,
        fun group_strict_get_with_exceptions/2,
        fun group_non_strict_get_with_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.
group_strict_get_with_operations(Map, Parameters) ->
    ok = common_group_get_with_operations(Map, Parameters#{strict => true}).

group_non_strict_get_with_operations(Map, Parameters) ->
    ok = common_group_get_with_operations(Map, Parameters#{strict => false}),
    ?TRY([30, 40], nested_maps:get_with([[k1_2], [ne_key, k2_2], '*'], ?FUN_GET_REMOVE_TAKE, Map, Parameters#{strict => false})),
    ok.

common_group_get_with_operations(Map, Parameters) ->
    ?TRY([20, 30, 40], nested_maps:get_with([[k1_2], [k2_1, k2_2], '*'], ?FUN_KEYS, Map, Parameters)),
    ok.

group_strict_get_with_exceptions(Map, Parameters) ->
    ok = common_group_get_with_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:get_with([[k1_2], [k2_1, k2_2], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, Parameters#{strict => true})),
    ok.

group_non_strict_get_with_exceptions(Map, Parameters) ->
    ok = common_group_get_with_exceptions(Map, Parameters#{strict => false}),
    ok.

common_group_get_with_exceptions(_Map, _Parameters) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PUT Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% SIMPLE
simple_put_operations(_) ->
    Map = reference_map(),
    TestFunctions = [   
        fun simple_strict_put_operations/2,
        fun simple_non_strict_put_operations/2,
        fun simple_strict_put_exceptions/2,
        fun simple_non_strict_put_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

simple_strict_put_operations(Map, Parameters) ->
    ok = common_simple_put_operations(Map, Parameters#{strict => true}).

simple_non_strict_put_operations(Map, Parameters) ->
    ok = common_simple_put_operations(Map, Parameters#{strict => false}),

    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{},
           k1_4 => #{k4_1 => #{k3_1 => v3_1}}}, nested_maps:put([[k1_4], [k4_1], [k3_1]], v3_1, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:put([[k1_4], [], [k3_1]], v3_1, Map, Parameters#{strict => false})),

    ?TRY(Map, nested_maps:put([], v3_1, Map, Parameters#{strict => false})),
    ok.

common_simple_put_operations(Map, Parameters) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1_new, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1], [k3_1]], v3_1_1_new, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2_new}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_2], [k3_3]], v3_3_2_new, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => v2_1,
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1]], v2_1, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => v1_2,
           k1_3 => #{}}, nested_maps:put([[k1_2]], v1_2, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{},
           k1_4 => v1_4}, nested_maps:put([[k1_4]], v1_4, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1, k3_3 => v3_3_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1], [k3_3]], v3_3_1, Map, Parameters)),
    ok.

simple_strict_put_exceptions(Map, Parameters) ->
    ok = common_simple_put_exceptions(Map, Parameters#{strict => true}),
    ?TRY({badarg, {'Address', []}}, nested_maps:put([], v3_1_1_new, Map, Parameters#{strict => true})),
    ?TRY({badkey, k1_4}, nested_maps:put([[k1_4], [k4_1], [k3_1]], v3_1, Map, Parameters#{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [k3_1]]}, nested_maps:put([[k1_2], [], [k3_1]], v3_1_1_new, Map, Parameters#{strict => true})),
    ok.

simple_non_strict_put_exceptions(Map, Parameters) ->
    ok = common_simple_put_exceptions(Map, Parameters#{strict => false}),
    ok.

common_simple_put_exceptions(Map, Parameters) ->
    ?TRY({badmap, {[k4_1],v1_1}, Map}, nested_maps:put([[k1_1], [k4_1], [k3_1]], v3_1, Map, Parameters)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:put([[k1_2], [k2_1], [k3_2]], v3_1, not_a_map, Parameters)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:put(not_a_list, v3_1, Map, Parameters)),
    ?TRY({badarg, {'Address', [[k1_2], not_a_list, [k3_2]]}},
         nested_maps:put([[k1_2], not_a_list, [k3_2]], v3_1, Map, Parameters)),
    ok.

%%% WILDCARD
wildcard_put_operations(_) ->
    Map = reference_map(),
    TestFunctions = [   
        fun wildcard_strict_put_operations/2,
        fun wildcard_non_strict_put_operations/2,
        fun wildcard_strict_put_exceptions/2,
        fun wildcard_non_strict_put_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

wildcard_strict_put_operations(Map, Parameters) ->
    ok = common_wildcard_put_operations(Map, Parameters#{strict => true}),
    ok.

wildcard_non_strict_put_operations(Map, Parameters) ->
    ok = common_wildcard_put_operations(Map, Parameters#{strict => false}),

    ?TRY(Map, nested_maps:put([[k1_3], '*', [k3_1]], v3_1, Map, Parameters#{strict => false})),
    ok.

common_wildcard_put_operations(Map, Parameters) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_new},
                     k2_2 => #{k3_2 => v3_new, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:put([[k1_2], '*', [k3_2]], v3_new, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_new, k3_2 => v3_new},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1], '*'], v3_new, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_new, k3_2 => v3_new},
                     k2_2 => #{k3_2 => v3_new, k3_3 => v3_new}},
           k1_3 => #{}}, nested_maps:put([[k1_2], '*', '*'], v3_new, Map, Parameters)),
    ok.

wildcard_strict_put_exceptions(Map, Parameters) ->
    ok = common_wildcard_put_exceptions(Map, Parameters#{strict => true}),
    ?TRY({unreachable_address, [[k1_3], '*', [k3_1]]}, nested_maps:put([[k1_3], '*', [k3_1]], v3_1, Map, Parameters#{strict => true})),
    ok.

wildcard_non_strict_put_exceptions(Map, Parameters) ->
    ok = common_wildcard_put_exceptions(Map, Parameters#{strict => false}),
    ok.

common_wildcard_put_exceptions(_Map, _Parameters) ->
    ok.

%%% GROUP

group_put_operations(_) ->
    Map = reference_map(),
    TestFunctions = [   
        fun group_strict_put_operations/2,
        fun group_strict_put_exceptions/2,
        fun group_non_strict_put_operations/2,
        fun group_non_strict_put_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

group_strict_put_operations(Map, Parameters) ->
    ok = common_group_put_operations(Map, Parameters#{strict => true}),
    ok.

group_non_strict_put_operations(Map, Parameters) ->
    ok = common_group_put_operations(Map, Parameters#{strict => false}),

    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_new, k3_2 => v3_2_1},
                     k2_2 => #{k3_1 => v3_new, k3_2 => v3_2_2, k3_3 => v3_3_2},
                     k2_3 => #{k3_1 => v3_new}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1, k2_2, k2_3], [k3_1]], v3_new, Map, Parameters#{strict => false})),
    ok.

common_group_put_operations(Map, Parameters) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_new},
                     k2_2 => #{k3_2 => v3_new, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1, k2_2], [k3_2]], v3_new, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_new, k3_2 => v3_new},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1], [k3_1, k3_2]], v3_new, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_new, k3_2 => v3_new, k3_3 => v3_new},
                     k2_2 => #{k3_1 => v3_new, k3_2 => v3_new, k3_3 => v3_new}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1, k2_2], [k3_1, k3_2, k3_3]], v3_new, Map, Parameters)),
    ok.

group_strict_put_exceptions(Map, Parameters) ->
    ?TRY({badkey, k2_3}, nested_maps:put([[k1_2], [k2_1, k2_2, k2_3], [k3_1]], v3_new, Map, Parameters#{strict => true})),
    ok.
group_non_strict_put_exceptions(_Map, _Parameters) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PUT_WITH Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% SIMPLE

simple_put_with_operations(_) ->
    Map = reference_map(integer_values),
    TestFunctions = [   
        fun simple_strict_put_with_operations/2,
        fun simple_non_strict_put_with_operations/2,
        fun simple_strict_put_with_exceptions/2,
        fun simple_non_strict_put_with_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

simple_strict_put_with_operations(Map, Parameters) ->
    ok = common_simple_put_with_operations(Map, Parameters#{strict => true}).

simple_non_strict_put_with_operations(Map, Parameters) ->
    ok = common_simple_put_with_operations(Map, Parameters#{strict => false}),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                     k2_2 => #{k3_2 => 30, k3_3 => 40}},
           k1_3 => #{},
           k1_4 => #{k4_1 => #{k3_1 => 100}}}, nested_maps:put_with([[k1_4], [k4_1], [k3_1]], ?FUN_PUT, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:put_with([[k1_4], [], [k3_1]], ?FUN_PUT, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:put_with([], ?FUN_PUT, Map, Parameters#{strict => false})),
    ok.

common_simple_put_with_operations(Map, Parameters) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 20},
                     k2_2 => #{k3_2 => 30, k3_3 => 40}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], [k2_1], [k3_1]], ?FUN_PUT, Map, Parameters)),


    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                     k2_2 => #{k3_2 => 30, k3_3 => 41}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], [k2_2], [k3_3]], ?FUN_PUT, Map, Parameters)),


    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                     k2_2 => #{k3_2 => 30, k3_3 => 40, k3_4 => 100}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], [k2_2], [k3_4]], ?FUN_PUT, Map, Parameters)),
    ok.

simple_strict_put_with_exceptions(Map, Parameters) ->
    ok = common_simple_put_with_exceptions(Map, Parameters#{strict => true}),

    ?TRY({badkey, k1_4}, nested_maps:put_with([[k1_4], [k4_1], [k3_1]], ?FUN_PUT, Map, Parameters#{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [k3_1]]}, nested_maps:put_with([[k1_2], [], [k3_1]], ?FUN_PUT, Map, Parameters#{strict => true})),
    ?TRY({badarg, {'Address', []}}, nested_maps:put_with([], ?FUN_PUT, Map, Parameters#{strict => true})),
    ok.

simple_non_strict_put_with_exceptions(Map, Parameters) ->
    ok = common_simple_put_with_exceptions(Map, Parameters#{strict => false}),
    ok.

common_simple_put_with_exceptions(Map, Parameters) ->
    Bad_fun = fun(_Key, Value, true) when Value > 10 -> 1 / 0;
                 (_Key, _Value, true) -> 1 / 0;
                 (_Key, _Value, false) -> 1 / 0 end,
    ?TRY({badfun, {badarith, '*'}}, nested_maps:put_with([[k1_2], [k2_1], [k3_2]], Bad_fun, Map, Parameters)),

    ?TRY({badfun, {got_throw, '*'}}, nested_maps:put_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_THROW3, Map, Parameters)),
    ?TRY({badfun, {got_error, '*'}}, nested_maps:put_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_ERROR3, Map, Parameters)),
    ?TRY({badfun, {got_exit, '*'}}, nested_maps:put_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_EXIT3, Map, Parameters)),

    ?TRY({badmap, {[k4_1],v1_1}, Map}, nested_maps:put_with([[k1_1], [k4_1], [k3_1]], ?FUN_PUT, Map, Parameters)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:put_with([[k1_2], [k2_1], [k3_2]], ?FUN_PUT, not_a_map, Parameters)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:put_with(not_a_list, ?FUN_PUT, Map, Parameters)),
    ?TRY({badarg, {'Address', [[k1_2], not_a_list, [k3_2]]}},
         nested_maps:put_with([[k1_2], not_a_list, [k3_2]], ?FUN_PUT, Map, Parameters)),
    ok.

%%% WILDCARD

wildcard_put_with_operations(_) ->
    Map = reference_map(integer_values),
    TestFunctions = [   
        fun wildcard_strict_put_with_operations/2,
        fun wildcard_non_strict_put_with_operations/2,
        fun wildcard_strict_put_with_exceptions/2,
        fun wildcard_non_strict_put_with_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

wildcard_strict_put_with_operations(Map, Parameters) ->
    ok = common_wildcard_put_with_operations(Map, Parameters#{strict => true}),
    ok.

wildcard_non_strict_put_with_operations(Map, Parameters) ->
    ok = common_wildcard_put_with_operations(Map, Parameters#{strict => false}),
    ?TRY(Map, nested_maps:put_with([[k1_3], '*', [k3_1]], ?FUN_PUT, Map, Parameters#{strict => false})),
    ok.

common_wildcard_put_with_operations(Map, Parameters) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 21},
                     k2_2 => #{k3_2 => 31, k3_3 => 40}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], '*', [k3_2]], ?FUN_PUT, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 21},
                     k2_2 => #{k3_2 => 30, k3_3 => 40}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], [k2_1], '*'], ?FUN_PUT, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 21},
                     k2_2 => #{k3_2 => 31, k3_3 => 41}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], '*', '*'], ?FUN_PUT, Map, Parameters)),
    ok.

wildcard_strict_put_with_exceptions(Map, Parameters) ->
    ok = common_wildcard_put_with_exceptions(Map, Parameters#{strict => true}),
    ?TRY({unreachable_address, [[k1_3], '*', [k3_1]]},
         nested_maps:put_with([[k1_3], '*', [k3_1]], ?FUN_PUT, Map, Parameters#{strict => true})),
    ok.

wildcard_non_strict_put_with_exceptions(Map, Parameters) ->
    ok = common_wildcard_put_with_exceptions(Map, Parameters#{strict => false}),
    ok.

common_wildcard_put_with_exceptions(_Map, _Parameters) ->
    ok.

%%% GROUP
group_put_with_operations(_) ->
    Map = reference_map(integer_values),
    TestFunctions = [   
        fun group_strict_put_with_operations/2,
        fun group_strict_put_with_exceptions/2,
        fun group_non_strict_put_with_operations/2,
        fun group_non_strict_put_with_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

group_strict_put_with_operations(Map, Parameters) ->
    ok = common_group_put_with_operations(Map, Parameters#{strict => true}),
    ok.

group_non_strict_put_with_operations(Map, Parameters) ->
    ok = common_group_put_with_operations(Map, Parameters#{strict => false}),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 20},
                     k2_2 => #{k3_1 => 100, k3_2 => 30, k3_3 => 40},
                     k2_3 => #{k3_1 => 100}},
           k1_3 => #{}},
         nested_maps:put_with([[k1_2], [k2_1, k2_2, k2_3], [k3_1]], ?FUN_PUT, Map, Parameters#{strict => false})),
    ok.

common_group_put_with_operations(Map, Parameters) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 21},
                     k2_2 => #{k3_2 => 31, k3_3 => 40}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], [k2_1, k2_2], [k3_2]], ?FUN_PUT, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 21},
                     k2_2 => #{k3_2 => 30, k3_3 => 40}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], [k2_1], [k3_1, k3_2]], ?FUN_PUT, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 21, k3_3 => 100},
                     k2_2 => #{k3_1 => 100, k3_2 => 31, k3_3 => 41}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], [k2_1, k2_2], [k3_1, k3_2, k3_3]], ?FUN_PUT, Map, Parameters)),
    ok.

group_strict_put_with_exceptions(Map, Parameters) ->
    ?TRY({badkey, k2_3}, nested_maps:put_with([[k1_2], [k2_1, k2_2, k2_3], [k3_1]], ?FUN_PUT, Map, Parameters#{strict => true})),
    ok.
group_non_strict_put_with_exceptions(_Map, _Parameters) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% KEYS Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% SIMPLE
simple_keys_operations(_) ->
    Map = reference_map(),
    TestFunctions = [   
        fun simple_strict_keys_operations/2,
        fun simple_non_strict_keys_operations/2,
        fun simple_strict_keys_exceptions/2,
        fun simple_non_strict_keys_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

simple_strict_keys_operations(Map, Parameters) ->
    ok = common_simple_keys_operations(Map, Parameters#{strict => true}).

simple_non_strict_keys_operations(Map, Parameters) ->
    ok = common_simple_keys_operations(Map, Parameters#{strict => false}),

    ?TRY([], nested_maps:keys([], Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:keys([[k1_1]], Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:keys([[]], Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:keys([[k1_2], []], Map, Parameters#{strict => false})),

    ?TRY([], nested_maps:keys([[k1_3]], Map, Parameters#{strict => false})),

    ?TRY([], nested_maps:keys([[ne_key], [k2_1], [k3_2]], Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:keys([[k1_2], [ne_key], [k3_2]], Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:keys([[k1_2], [k2_1], [ne_key]], Map, Parameters#{strict => false})),
    ok.

simple_strict_keys_exceptions(Map, Parameters) ->
    ok = common_simple_keys_exceptions(Map, Parameters#{strict => true}),
    ?TRY({bad_address, [[]]}, nested_maps:keys([[]], Map, Parameters#{strict => true})),
    ?TRY({bad_address, [[k1_2], []]}, nested_maps:keys([[k1_2], []], Map, Parameters#{strict => true})),
    ?TRY({badarg, {'Address', []}}, nested_maps:keys([], Map, Parameters#{strict => true})),
    ?TRY({badkey, k1_3}, nested_maps:keys([[k1_3]], Map, Parameters#{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:keys([[ne_key], [k2_1], [k3_2]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:keys([[k1_2], [ne_key], [k3_2]], Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:keys([[k1_2], [k2_1], [ne_key]], Map, Parameters#{strict => true})),

    ok.

simple_non_strict_keys_exceptions(Map, Parameters) ->
    ok = common_simple_keys_exceptions(Map, Parameters#{strict => false}).

common_simple_keys_operations(Map, Parameters) ->
    ?TRY([k2_1, k2_2], nested_maps:keys([[k1_2]], Map, Parameters)),
    ?TRY([k3_1, k3_2], nested_maps:keys([[k1_2], [k2_1]], Map, Parameters)),
    ?TRY([k3_2, k3_3], nested_maps:keys([[k1_2], [k2_2]], Map, Parameters)),
    ok.

common_simple_keys_exceptions(Map, Parameters) ->
    ?TRY({badmap, {[k2_1], v1_1}, Map}, nested_maps:keys([[k1_1], [k2_1]], Map, Parameters)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:keys([[k1_2], [k2_1], [k3_2]], not_a_map, Parameters)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:keys(not_a_list, Map, Parameters)),
    ?TRY({badarg, {'Options', not_strict}}, nested_maps:keys([[k1_2], [k2_1]], Map, not_strict)),
    ok.

%%% WILDCARD

wildcard_keys_operations(_) ->
    Map = reference_map(),
    TestFunctions = [   
        fun wildcard_strict_keys_operations/2,
        fun wildcard_non_strict_keys_operations/2,
        fun wildcard_strict_keys_exceptions/2,
        fun wildcard_non_strict_keys_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

wildcard_strict_keys_operations(Map, Parameters) ->
    ok = common_wildcard_keys_operations(Map, Parameters#{strict => true}).

wildcard_non_strict_keys_operations(Map, Parameters) ->
    ok = common_wildcard_keys_operations(Map, Parameters#{strict => false}),

    ?TRY([k2_1, k2_2], nested_maps:keys(['*'], Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:keys([[k1_3], '*'], Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:keys(['*'], #{}, Parameters#{strict => false})),
    ok.

wildcard_strict_keys_exceptions(Map, Parameters) ->
    ok = common_wildcard_keys_exceptions(Map, Parameters#{strict => true}),

    ?TRY({unreachable_address, ['*']}, nested_maps:keys(['*'], #{}, Parameters#{strict => true})),
    ?TRY({badmap, v1_1, Map}, nested_maps:keys(['*'], Map, Parameters#{strict => true})),
    ok.

wildcard_non_strict_keys_exceptions(Map, Parameters) ->
    ok = common_wildcard_keys_exceptions(Map, Parameters#{strict => false}).

common_wildcard_keys_operations(Map, Parameters) ->
    ?TRY([k3_1, k3_2, k3_2, k3_3], nested_maps:keys([[k1_2], '*'], Map, Parameters)),
    ok.

common_wildcard_keys_exceptions(_Map, _Parameters) ->
    ok.

%%% GROUP
group_keys_operations(_) ->
    Map = reference_map(),
    TestFunctions = [   
        fun group_strict_keys_operations/2,
        fun group_strict_keys_exceptions/2,
        fun group_non_strict_keys_operations/2,
        fun group_non_strict_keys_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

group_strict_keys_operations(Map, Parameters) ->
    ok = common_group_keys_operations(Map, Parameters#{strict => true}).
group_non_strict_keys_operations(Map, Parameters) ->
    ok = common_group_keys_operations(Map, Parameters#{strict => false}),
    ?TRY([k3_1, k3_2], nested_maps:keys([[k1_2], [k2_1, ne_key]], Map, Parameters#{strict => false})),
    ok.

group_strict_keys_exceptions(Map, Parameters) ->
    ok = common_group_keys_exceptions(Map, Parameters#{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:keys([[k1_2], [k2_1, ne_key]], Map, Parameters#{strict => true})),
    ok.

group_non_strict_keys_exceptions(Map, Parameters) ->
    ok = common_group_keys_exceptions(Map, Parameters#{strict => false}).

common_group_keys_operations(Map, Parameters) ->
    ?TRY([k3_1, k3_2, k3_2, k3_3], nested_maps:keys([[k1_2], [k2_1, k2_2]], Map, Parameters)),
    ok.

common_group_keys_exceptions(_Map, _Parameters) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% KEYS_WITH Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% SIMPLE

simple_keys_with_operations(_) ->
    Map = reference_map(integer_values),
    TestFunctions = [   
        fun simple_strict_keys_with_operations/2,
        fun simple_non_strict_keys_with_operations/2,
        fun simple_strict_keys_with_exceptions/2,
        fun simple_non_strict_keys_with_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

simple_strict_keys_with_operations(Map, Parameters) ->
    ok = common_simple_keys_with_operations(Map, Parameters#{strict => true}).

simple_non_strict_keys_with_operations(Map, Parameters) ->
    ok = common_simple_keys_with_operations(Map, Parameters#{strict => false}),

    ?TRY([], nested_maps:keys_with([[k1_1]], ?FUN_KEYS, Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:keys_with([[]], ?FUN_KEYS, Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:keys_with([[k1_2], []], ?FUN_KEYS, Map, Parameters#{strict => false})),

    ?TRY([], nested_maps:keys_with([], ?FUN_KEYS, Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:keys_with([[k1_3]], ?FUN_KEYS, Map, Parameters#{strict => false})),

    ?TRY([], nested_maps:keys_with([[ne_key], [k2_1], [k3_2]], ?FUN_KEYS, Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:keys_with([[k1_2], [ne_key], [k3_2]], ?FUN_KEYS, Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:keys_with([[k1_2], [k2_1], [ne_key]], ?FUN_KEYS, Map, Parameters#{strict => false})),
    ok.

simple_strict_keys_with_exceptions(Map, Parameters) ->
    ok = common_simple_keys_with_exceptions(Map, Parameters#{strict => true}),

    ?TRY({bad_address, [[]]}, nested_maps:keys_with([[]], ?FUN_KEYS, Map, Parameters#{strict => true})),
    ?TRY({bad_address, [[k1_2], []]}, nested_maps:keys_with([[k1_2], []], ?FUN_KEYS, Map, Parameters#{strict => true})),
    ?TRY({badmap, v1_1, Map}, nested_maps:keys_with([[k1_1]], ?FUN_KEYS, Map, Parameters#{strict => true})),
    ?TRY({badarg, {'Address', []}}, nested_maps:keys_with([], ?FUN_KEYS, Map, Parameters#{strict => true})),

    ?TRY({badkey, k1_3}, nested_maps:keys_with([[k1_3]], ?FUN_KEYS, Map, Parameters#{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:keys_with([[ne_key], [k2_1], [k3_2]], ?FUN_KEYS, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:keys_with([[k1_2], [ne_key], [k3_2]], ?FUN_KEYS, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:keys_with([[k1_2], [k2_1], [ne_key]], ?FUN_KEYS, Map, Parameters#{strict => true})),


    ?TRY({badarg, {'Address', []}}, nested_maps:keys_with([], ?FUN_KEYS, Map, Parameters#{strict => true})),
    ok.

simple_non_strict_keys_with_exceptions(Map, Parameters) ->
    ok = common_simple_keys_with_exceptions(Map, Parameters#{strict => false}).

common_simple_keys_with_operations(Map, Parameters) ->
    ?TRY([], nested_maps:keys_with([[k1_2]], ?FUN_KEYS, Map, Parameters)),
    ?TRY([k3_2], nested_maps:keys_with([[k1_2], [k2_1]], ?FUN_KEYS, Map, Parameters)),
    ?TRY([k3_2, k3_3], nested_maps:keys_with([[k1_2], [k2_2]], ?FUN_KEYS, Map, Parameters)),
    ok.

common_simple_keys_with_exceptions(Map, Parameters) ->
    ?TRY({badmap, {[k2_1],v1_1}, Map}, nested_maps:keys_with([[k1_1], [k2_1]], ?FUN_KEYS, Map, Parameters)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:keys_with([[k1_2], [k2_1], [k3_2]], ?FUN_KEYS, not_a_map, Parameters)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:keys_with(not_a_list, ?FUN_KEYS, Map, Parameters)),
    ?TRY({badarg, {'Options', not_strict}}, nested_maps:keys_with([[k1_2], [k2_1]], ?FUN_KEYS, Map, not_strict)),

    Bad_fun1 = fun(_K, V) when V > 20 -> not_true; (_, _) -> not_false end,
    ?TRY({badarg, {'Function', Bad_fun1}}, nested_maps:keys_with([[k1_2], [k2_2]], Bad_fun1, Map, Parameters)),

    Bad_fun2 = fun(_K, V) when V > 20 -> 1 / 0; (_, _) -> false end,
    ?TRY({badfun, {badarith, '*'}}, nested_maps:keys_with([[k1_2], [k2_2]], Bad_fun2, Map, Parameters)),

    ?TRY({badfun, {got_throw, '*'}}, nested_maps:keys_with([[k1_2], [k2_2]], ?BAD_FUN_THROW, Map, Parameters)),
    ?TRY({badfun, {got_error, '*'}}, nested_maps:keys_with([[k1_2], [k2_2]], ?BAD_FUN_ERROR, Map, Parameters)),
    ?TRY({badfun, {got_exit, '*'}}, nested_maps:keys_with([[k1_2], [k2_2]], ?BAD_FUN_EXIT, Map, Parameters)),

    Bad_fun3 = fun(V) when is_integer(V), V > 10 -> true; (_) -> false end,
    ?TRY({badarg, {'Function', Bad_fun3}}, nested_maps:keys_with([[k1_2], [k2_2]], Bad_fun3, Map, Parameters)),

    ok.

%%% WILDCARD

wildcard_keys_with_operations(_) ->
    Map = reference_map(integer_values),
    TestFunctions = [   
        fun wildcard_strict_keys_with_operations/2,
        fun wildcard_non_strict_keys_with_operations/2,
        fun wildcard_strict_keys_with_exceptions/2,
        fun wildcard_non_strict_keys_with_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

wildcard_strict_keys_with_operations(Map, Parameters) ->
    ok = common_wildcard_keys_with_operations(Map, Parameters#{strict => true}).

wildcard_non_strict_keys_with_operations(Map, Parameters) ->
    ok = common_wildcard_keys_with_operations(Map, Parameters#{strict => false}),

    ?TRY([], nested_maps:keys_with(['*'], ?FUN_KEYS, Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:keys_with([[k1_3], '*'], ?FUN_KEYS, Map, Parameters#{strict => false})),
    ?TRY([], nested_maps:keys_with(['*'], ?FUN_KEYS, #{}, Parameters#{strict => false})),
    ok.

wildcard_strict_keys_with_exceptions(Map, Parameters) ->
    ok = common_wildcard_keys_with_exceptions(Map, Parameters#{strict => true}),

    ?TRY({unreachable_address, ['*']}, nested_maps:keys_with(['*'], ?FUN_KEYS, #{}, Parameters#{strict => true})),
    ?TRY({badmap, v1_1, Map}, nested_maps:keys_with(['*'], ?FUN_KEYS, Map, Parameters#{strict => true})),
    ok.

wildcard_non_strict_keys_with_exceptions(Map, Parameters) ->
    ok = common_wildcard_keys_with_exceptions(Map, Parameters#{strict => false}).

common_wildcard_keys_with_operations(Map, Parameters) ->
    ?TRY([k3_3, k3_2, k3_2], nested_maps:keys_with([[k1_2], '*'], ?FUN_KEYS, Map, Parameters)),
    ok.

common_wildcard_keys_with_exceptions(_Map, _Parameters) ->
    ok.

%%% GROUP

group_keys_with_operations(_) ->
    Map = reference_map(integer_values),
    TestFunctions = [   
        fun group_strict_keys_with_operations/2,
        fun group_strict_keys_with_exceptions/2,
        fun group_non_strict_keys_with_operations/2,
        fun group_non_strict_keys_with_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

group_strict_keys_with_operations(Map, Parameters) ->
    ok = common_group_keys_with_operations(Map, Parameters#{strict => true}).
group_non_strict_keys_with_operations(Map, Parameters) ->
    ok = common_group_keys_with_operations(Map, Parameters#{strict => false}),
    ?TRY([k3_2], nested_maps:keys_with([[k1_2], [k2_1, ne_key]], ?FUN_KEYS, Map, Parameters#{strict => false})),
    ok.

group_strict_keys_with_exceptions(Map, Parameters) ->
    ok = common_group_keys_with_exceptions(Map, Parameters#{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:keys_with([[k1_2], [k2_1, ne_key]], ?FUN_KEYS, Map, Parameters#{strict => true})),
    ok.

group_non_strict_keys_with_exceptions(Map, Parameters) ->
    ok = common_group_keys_with_exceptions(Map, Parameters#{strict => false}).

common_group_keys_with_operations(Map, Parameters) ->
    ?TRY([k3_3, k3_2, k3_2], nested_maps:keys_with([[k1_2], [k2_1, k2_2]], ?FUN_KEYS, Map, Parameters)),
    ok.

common_group_keys_with_exceptions(_Map, _Parameters) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% UPDATE Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Simple
simple_update_operations(_) ->
    Map = reference_map(),
    TestFunctions = [
        fun simple_strict_update_operations/2,
        fun simple_non_strict_update_operations/2,
        fun simple_strict_update_exceptions/2,
        fun simple_non_strict_update_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

simple_strict_update_operations(Map, Parameters) ->
    ok = common_simple_update_operations(Map, Parameters#{strict => true}).

simple_non_strict_update_operations(Map, Parameters) ->
    ok = common_simple_update_operations(Map, Parameters#{strict => false}),

    ?TRY(Map, nested_maps:update([[new_key], [k2_1], [k3_1]], new_value, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:update([[new_key], [k2_1], [new_key]], new_value, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:update([[new_key], [new_key], [new_key]], new_value, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:update([[k1_2], [new_key], [new_key]], new_value, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:update([[k1_2], [k2_1], [new_key]], new_value, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:update([[k1_2], [new_key]], new_value, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:update([[new_key]], new_value, Map, Parameters#{strict => false})),

    ?TRY(Map, nested_maps:update([[]], new_value, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:update([[k1_2], []], new_value, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:update([[k1_2], [], [k3_1]], new_value, Map, Parameters#{strict => false})),

    ?TRY(Map, nested_maps:update([], new_value, Map, Parameters#{strict => false})),
    ok.

common_simple_update_operations(Map, Parameters) ->
    ?TRY(#{k1_1 => new_value,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_1]], new_value, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => new_value, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_1], [k3_1]], new_value, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => new_value},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_1], [k3_2]], new_value, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => new_value, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_2], [k3_2]], new_value, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => new_value}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_2], [k3_3]], new_value, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{k2_3 => new_value}}, nested_maps:update([[k1_3]], #{k2_3 => new_value}, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => new_value,
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_1]], new_value, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => new_value},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_2]], new_value, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{},
           k1_3 => #{}}, nested_maps:update([[k1_2]], #{}, Map, Parameters)),
    ok.
simple_strict_update_exceptions(Map, Parameters) ->
    % Input parameters
    ok = common_simple_update_exceptions(Map, Parameters#{strict => true}),

    % Undefined keys
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key], [k2_1], [k3_2]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key], [k2_1], [ne_key]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key], [ne_key], [ne_key]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [ne_key], [ne_key]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [k2_1], [ne_key]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [ne_key]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key]], new_value, Map, Parameters#{strict => true})),

% Missing keys
    ?TRY({badarg, {'Address', []}}, nested_maps:update([], new_value, Map, Parameters#{strict => true})),
    ?TRY({bad_address, [[]]}, nested_maps:update([[]], new_value, Map, Parameters#{strict => true})),
    ?TRY({bad_address, [[k1_2], []]}, nested_maps:update([[k1_2], []], new_value, Map, Parameters#{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [k3_2]]}, nested_maps:update([[k1_2], [], [k3_2]], new_value, Map, Parameters#{strict => true})),
    ok.

simple_non_strict_update_exceptions(Map, Parameters) ->
    ok = common_simple_update_exceptions(Map, Parameters#{strict => false}),
    ok.

common_simple_update_exceptions(Map, Parameters) ->
    ?TRY({badmap, {[k2_1],v1_1}, Map}, nested_maps:update([[k1_1], [k2_1]], new_value, Map, Parameters)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:update([[k1_2], [k2_1], [k3_2]], new_value, not_a_map, Parameters)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:update(not_a_list, new_value, Map, Parameters)),
    ?TRY({badarg, {'Options', not_strict}}, nested_maps:update([[k1_2], [k2_1]], new_value, Map, not_strict)),
    ok.

%%% Wildcard

wildcard_update_operations(_) ->
    Map = reference_map(),
    TestFunctions = [
        fun wildcard_strict_update_operations/2,
        fun wildcard_non_strict_update_operations/2,
        fun wildcard_strict_update_exceptions/2,
        fun wildcard_non_strict_update_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

wildcard_strict_update_operations(Map, Parameters) ->
    ok = common_wildcard_update_operations(Map, Parameters#{strict => true}).

wildcard_non_strict_update_operations(Map, Parameters) ->
    ok = common_wildcard_update_operations(Map, Parameters#{strict => false}),

    ?TRY(Map, nested_maps:update([[k1_2], [ne_key], '*'], new_value, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:update([[k1_2], '*', [ne_key]], new_value, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:update([[ne_key], [ne_key], '*'], new_value, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:update([[ne_key], [k2_1], '*'], new_value, Map, Parameters#{strict => false})),


    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => new_value, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], '*', [k3_1]], new_value, Map, Parameters#{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => new_value}},
           k1_3 => #{}}, nested_maps:update([[k1_2], '*', [k3_3]], new_value, Map, Parameters#{strict => false})),

    ?TRY(Map, nested_maps:update([[k1_3], '*', [k3_2]], new_value, Map, Parameters#{strict => false})),
    ok.

common_wildcard_update_operations(Map, Parameters) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => new_value, k3_3 => new_value}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_2], '*'], new_value, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => new_value, k3_2 => new_value},
                     k2_2 => #{k3_2 => new_value, k3_3 => new_value}},
           k1_3 => #{}}, nested_maps:update([[k1_2], '*', '*'], new_value, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => new_value},
                     k2_2 => #{k3_2 => new_value, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], '*', [k3_2]], new_value, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => new_value,
                     k2_2 => new_value},
           k1_3 => #{}}, nested_maps:update([[k1_2], '*'], new_value, Map, Parameters)),
    ?TRY(#{k1_1 => new_value,
           k1_2 => new_value,
           k1_3 => new_value}, nested_maps:update(['*'], new_value, Map, Parameters)),
    ok.
wildcard_strict_update_exceptions(Map, Parameters) ->
    ok = common_wildcard_update_exceptions(Map, Parameters#{strict => true}),
    ?TRY({badkey, k3_1}, nested_maps:update([[k1_2], '*', [k3_1]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, k3_3}, nested_maps:update([[k1_2], '*', [k3_3]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [ne_key], '*'], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], '*', [ne_key]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key], [ne_key], '*'], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key], [k2_1], '*'], new_value, Map, Parameters#{strict => true})),
    ?TRY({unreachable_address, [[k1_3], '*', [k3_2]]},
         nested_maps:update([[k1_3], '*', [k3_2]], new_value, Map, Parameters#{strict => true})),
    ok.

wildcard_non_strict_update_exceptions(Map, Parameters) ->
    ok = common_wildcard_update_exceptions(Map, Parameters#{strict => false}),
    ok.

common_wildcard_update_exceptions(Map, Parameters) ->
    ?TRY({badmap, {'*',v1_1}, Map}, nested_maps:update([[k1_1], '*'], new_value, Map, Parameters)),
    case maps:get(parallel, Parameters) of
        true ->
            ?TRYV([{badmap, {'*', v1_1}, Map}, {unreachable_address, ['*', '*', '*']}], nested_maps:update(['*', '*', '*'], new_value, Map, Parameters));
        false ->
            ?TRY({badmap, {'*', v1_1}, Map}, nested_maps:update(['*', '*', '*'], new_value, Map, Parameters))
    end,
    ok.

%%% Group

group_update_operations(_) ->
    Map = reference_map(),
    TestFunctions = [
        fun group_strict_update_operations/2,
        fun group_strict_update_exceptions/2,
        fun group_non_strict_update_operations/2,
        fun group_non_strict_update_exceptions/2
    ],

    [Fun(Map, #{parallel => Parallel}) || Parallel <- [false, true], Fun <- TestFunctions],
    ok.

group_strict_update_operations(Map, Parameters) ->
    ok = common_group_update_operations(Map, Parameters#{strict => true}).

group_non_strict_update_operations(Map, Parameters) ->
    ok = common_group_update_operations(Map, Parameters#{strict => false}),

    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => new_value, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_1], [k3_1, ne_key]], new_value, Map, Parameters#{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => new_value, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_2], [ne_key, k3_2]], new_value, Map, Parameters#{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => new_value}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_2, ne_key], [k3_3]], new_value, Map, Parameters#{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => new_value},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2, ne_key], [k2_1], [k3_2]], new_value, Map, Parameters#{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => new_value, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_1, ne_key], [k3_1, ne_key]], new_value, Map, Parameters#{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => new_value}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [ne_key, k2_2], [ne_key, k3_3]], new_value, Map, Parameters#{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => new_value, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2, ne_key], [k2_1, ne_key], [k3_1, ne_key]], new_value, Map, Parameters#{strict => false})),
    ?TRY(Map, nested_maps:update([[ne_key, k1_3], [ne_key, k2_2], [ne_key, k3_3]], new_value, Map, Parameters#{strict => false})),
    ok.

common_group_update_operations(Map, Parameters) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => new_value, k3_2 => new_value},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_1], [k3_1, k3_2]], new_value, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => new_value, k3_3 => new_value}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_2], [k3_2, k3_3]], new_value, Map, Parameters)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => new_value},
                     k2_2 => #{k3_2 => new_value, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_1, k2_2], [k3_2]], new_value, Map, Parameters)),
    ok.

group_strict_update_exceptions(Map, Parameters) ->
    ok = common_group_update_exceptions(Map, Parameters#{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [k2_1], [k3_1, ne_key]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [k2_1, ne_key], [k3_1]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [ne_key, k2_2], [k3_2]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2, ne_key], [k2_1], [k3_1]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key, k1_3], [k2_2], [k3_2]], new_value, Map, Parameters#{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [k2_1, ne_key], [k3_1, ne_key]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [ne_key, k2_2], [ne_key, k3_2]], new_value, Map, Parameters#{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2, ne_key], [k2_1, ne_key], [k3_1, ne_key]], new_value, Map, Parameters#{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key, k1_3], [ne_key, k2_2], [ne_key, k3_2]], new_value, Map, Parameters#{strict => true})),
    ok.

group_non_strict_update_exceptions(Map, Parameters) ->
    ok = common_group_update_exceptions(Map, Parameters#{strict => false}),
    ok.

common_group_update_exceptions(Map, Parameters) ->
    ?TRY({badmap, {[k2_1,k2_2],v1_1}, Map}, nested_maps:update([[k1_1], [k2_1, k2_2], [k3_1]], new_value, Map, Parameters)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% UPDATE_WITH Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Simple
simple_strict_update_with_operations(Map) ->
    ok = common_simple_update_with_operations(Map, #{strict => true}).

simple_non_strict_update_with_operations(Map) ->
    ok = common_simple_update_with_operations(Map, #{strict => false}),
    ?TRY(Map, nested_maps:update_with([], ?FUN_UPDATE, Map, #{strict => false})),
    ?TRY(Map, nested_maps:update_with([[k1_2], [k2_1], [ne_key]], ?FUN_UPDATE, Map, #{strict => false})),
    ok.

common_simple_update_with_operations(Map, Options) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 20},
                     k2_2 => #{k3_2 => 30, k3_3 => 40}},
           k1_3 => #{}}, nested_maps:update_with([[k1_2], [k2_1], [k3_1]], ?FUN_UPDATE, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                     k2_2 => #{k3_2 => 31, k3_3 => 40}},
           k1_3 => #{}}, nested_maps:update_with([[k1_2], [k2_2], [k3_2]], ?FUN_UPDATE, Map, Options)),
    ok.

simple_strict_update_with_exceptions(Map) ->
    ok = common_simple_update_with_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:update_with([[k1_2], [k2_1], [ne_key]], ?FUN_UPDATE, Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [ne_key]]},
         nested_maps:update_with([[k1_2], [], [ne_key]], ?FUN_UPDATE, Map, #{strict => true})),
    ?TRY({badarg, {'Address', []}}, nested_maps:update_with([], ?FUN_UPDATE, Map, #{strict => true})),
    ok.

simple_non_strict_update_with_exceptions(Map) ->
    ok = common_simple_update_with_exceptions(Map, #{strict => false}),
    ok.

common_simple_update_with_exceptions(Map, Options) ->
    ?TRY({badfun, {badarith, '*'}}, nested_maps:update_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN, Map, Options)),

    ?TRY({badfun, {got_throw, '*'}}, nested_maps:update_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_THROW, Map, Options)),
    ?TRY({badfun, {got_error, '*'}}, nested_maps:update_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_ERROR, Map, Options)),
    ?TRY({badfun, {got_exit, '*'}}, nested_maps:update_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_EXIT, Map, Options)),

    ?TRY({badmap, {[k4_1],v1_1}, Map}, nested_maps:update_with([[k1_1], [k4_1], [k3_1]], ?FUN_UPDATE, Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}},
         nested_maps:update_with([[k1_2], [k2_1], [k3_2]], ?FUN_UPDATE, not_a_map, Options)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:update_with(not_a_list, ?FUN_UPDATE, Map, Options)),
    ?TRY({badarg, {'Address', [[k1_2], not_a_list, [k3_2]]}},
         nested_maps:update_with([[k1_2], not_a_list, [k3_2]], ?FUN_UPDATE, Map, Options)),
    ?TRY({badarg, {'Options', not_strict}}, nested_maps:update_with([[k1_2], [k2_1]], ?FUN_UPDATE, Map, not_strict)),
    ok.

%%% Wildcard

wildcard_strict_update_with_operations(Map) ->
    ok = common_wildcard_update_with_operations(Map, #{strict => true}).

wildcard_non_strict_update_with_operations(Map) ->
    ok = common_wildcard_update_with_operations(Map, #{strict => false}),
    ?TRY(Map, nested_maps:update_with([[k1_2], [ne_key], '*'], ?FUN_UPDATE, Map, #{strict => false})),
    ok.

common_wildcard_update_with_operations(Map, Options) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 19},
                     k2_2 => #{k3_2 => 30, k3_3 => 40}},
           k1_3 => #{}}, nested_maps:update_with([[k1_2], [k2_1], '*'], ?FUN_UPDATE, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                     k2_2 => #{k3_2 => 31, k3_3 => 41}},
           k1_3 => #{}}, nested_maps:update_with([[k1_2], [k2_2], '*'], ?FUN_UPDATE, Map, Options)),
    ok.

wildcard_strict_update_with_exceptions(Map) ->
    ok = common_wildcard_update_with_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:update_with([[k1_2], '*', [ne_key]], ?FUN_UPDATE, Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], [], '*']}, nested_maps:update_with([[k1_2], [], '*'], ?FUN_UPDATE, Map, #{strict => true})),
    ?TRY({unreachable_address, ['*']}, nested_maps:update_with(['*'], ?FUN_UPDATE, #{}, #{strict => true})),
    ok.

wildcard_non_strict_update_with_exceptions(Map) ->
    ok = common_wildcard_update_with_exceptions(Map, #{strict => false}),
    ok.

common_wildcard_update_with_exceptions(Map, Options) ->
    ?TRY({badmap, {'*',v1_1}, Map}, nested_maps:update_with([[k1_1], '*', '*'], ?FUN_UPDATE, Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:update_with([[k1_2], '*', [k3_2]], ?FUN_UPDATE, not_a_map, Options)),
    ?TRY({badarg, {'Address', [[k1_2], not_a_list, '*']}},
         nested_maps:update_with([[k1_2], not_a_list, '*'], ?FUN_UPDATE, Map, Options)),
    ok.

%%% Group

group_strict_update_with_operations(Map) ->
    ok = common_group_update_with_operations(Map, #{strict => true}).

group_non_strict_update_with_operations(Map) ->
    ok = common_group_update_with_operations(Map, #{strict => false}),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                     k2_2 => #{k3_2 => 31, k3_3 => 41}},
           k1_3 => #{}}, nested_maps:update_with([[k1_2], [ne_key, k2_2], '*'], ?FUN_UPDATE, Map, #{strict => false})),
    ok.

common_group_update_with_operations(Map, Options) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 19},
                     k2_2 => #{k3_2 => 31, k3_3 => 41}},
           k1_3 => #{}}, nested_maps:update_with([[k1_2], [k2_1, k2_2], '*'], ?FUN_UPDATE, Map, Options)),
    ok.

group_strict_update_with_exceptions(Map) ->
    ok = common_group_update_with_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:update_with([[k1_2], [k2_1, k2_2], [ne_key]], ?FUN_UPDATE, Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], [k2_1, k2_2], []]},
         nested_maps:update_with([[k1_2], [k2_1, k2_2], []], ?FUN_UPDATE, Map, #{strict => true})),
    ok.

group_non_strict_update_with_exceptions(Map) ->
    ok = common_group_update_with_exceptions(Map, #{strict => false}),
    ok.

common_group_update_with_exceptions(Map, Options) ->
    ?TRY({badmap, {[k2_1,k2_2],v1_1}, Map}, nested_maps:update_with([[k1_1], [k2_1, k2_2], [k3_1]], ?FUN_UPDATE, Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}},
         nested_maps:update_with([[k1_1], [k2_1, k2_2], [k3_1]], ?FUN_UPDATE, not_a_map, Options)),
    ?TRY({badarg, {'Address', [[k1_2], [k2_1, k2_2], not_a_list]}},
         nested_maps:update_with([[k1_2], [k2_1, k2_2], not_a_list], ?FUN_UPDATE, Map, Options)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% REMOVE Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Simple

simple_strict_remove_operations(Map) ->
    ok = common_simple_remove_operations(Map, #{strict => true}).

simple_non_strict_remove_operations(Map) ->
    ok = common_simple_remove_operations(Map, #{strict => false}),

    ?TRY(Map, nested_maps:remove([[ne_key], [k2_1], [k3_1]], Map, #{strict => false})),
    ?TRY(Map, nested_maps:remove([[ne_key], [k2_1], [ne_key]], Map, #{strict => false})),
    ?TRY(Map, nested_maps:remove([[ne_key], [ne_key], [ne_key]], Map, #{strict => false})),
    ?TRY(Map, nested_maps:remove([[k1_2], [ne_key], [ne_key]], Map, #{strict => false})),
    ?TRY(Map, nested_maps:remove([[k1_2], [k2_1], [ne_key]], Map, #{strict => false})),
    ?TRY(Map, nested_maps:remove([[k1_2], [ne_key]], Map, #{strict => false})),
    ?TRY(Map, nested_maps:remove([[ne_key]], Map, #{strict => false})),

    ?TRY(Map, nested_maps:remove([[]], Map, #{strict => false})),
    ?TRY(Map, nested_maps:remove([[k1_2], []], Map, #{strict => false})),
    ?TRY(Map, nested_maps:remove([[k1_2], [], [k3_1]], Map, #{strict => false})),

    ?TRY(Map, nested_maps:remove([], Map, #{strict => false})),
    ok.

common_simple_remove_operations(Map, Options) ->
    ?TRY(#{k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_1]], Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [k2_1], [k3_1]], Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [k2_1], [k3_2]], Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [k2_2], [k3_2]], Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [k2_2], [k3_3]], Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}}
         }, nested_maps:remove([[k1_3]], Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [k2_1]], Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [k2_2]], Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_3 => #{}}, nested_maps:remove([[k1_2]], Map, Options)),
    ok.
simple_strict_remove_exceptions(Map) ->
    % Input parameters
    ok = common_simple_remove_exceptions(Map, #{strict => true}),

    % Undefined keys
    ?TRY({badkey, ne_key}, nested_maps:remove([[ne_key], [k2_1], [k3_2]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[ne_key], [k2_1], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[ne_key], [ne_key], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[k1_2], [ne_key], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[k1_2], [k2_1], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[k1_2], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[ne_key]], Map, #{strict => true})),

% Missing keys
    ?TRY({badarg, {'Address', []}}, nested_maps:remove([], Map, #{strict => true})),
    ?TRY({bad_address, [[]]}, nested_maps:remove([[]], Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], []]}, nested_maps:remove([[k1_2], []], Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [k3_2]]}, nested_maps:remove([[k1_2], [], [k3_2]], Map, #{strict => true})),
    ok.

simple_non_strict_remove_exceptions(Map) ->
    ok = common_simple_remove_exceptions(Map, #{strict => false}),
    ok.

common_simple_remove_exceptions(Map, Options) ->
    ?TRY({badmap, {[k2_1],v1_1}, Map}, nested_maps:remove([[k1_1], [k2_1]], Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:remove([[k1_2], [k2_1], [k3_2]], not_a_map, Options)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:remove(not_a_list, Map, Options)),
    ?TRY({badarg, {'Options', not_strict}}, nested_maps:remove([[k1_2], [k2_1]], Map, not_strict)),
    ok.

%%% Wildcard

wildcard_strict_remove_operations(Map) ->
    ok = common_wildcard_remove_operations(Map, #{strict => true}).

wildcard_non_strict_remove_operations(Map) ->
    ok = common_wildcard_remove_operations(Map, #{strict => false}),

    ?TRY(Map, nested_maps:remove([[k1_2], [ne_key], '*'], Map, #{strict => false})),
    ?TRY(Map, nested_maps:remove([[k1_2], '*', [ne_key]], Map, #{strict => false})),
    ?TRY(Map, nested_maps:remove([[ne_key], [ne_key], '*'], Map, #{strict => false})),
    ?TRY(Map, nested_maps:remove([[ne_key], [k2_1], '*'], Map, #{strict => false})),

    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], '*', [k3_1]], Map, #{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], '*', [k3_3]], Map, #{strict => false})),
    ?TRY(Map, nested_maps:remove([[k1_3], '*', [k3_2]], Map, #{strict => false})),
    ok.

common_wildcard_remove_operations(Map, Options) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [k2_2], '*'], Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{}, k2_2 => #{}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], '*', '*'], Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1},
                     k2_2 => #{k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], '*', [k3_2]], Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{},
           k1_3 => #{}}, nested_maps:remove([[k1_2], '*'], Map, Options)),
    ?TRY(#{}, nested_maps:remove(['*'], Map, Options)),
    ok.
wildcard_strict_remove_exceptions(Map) ->
    ok = common_wildcard_remove_exceptions(Map, #{strict => true}),
    ?TRY({badkey, k3_1}, nested_maps:remove([[k1_2], '*', [k3_1]], Map, #{strict => true})),
    ?TRY({badkey, k3_3}, nested_maps:remove([[k1_2], '*', [k3_3]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[k1_2], [ne_key], '*'], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[k1_2], '*', [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[ne_key], [ne_key], '*'], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[ne_key], [k2_1], '*'], Map, #{strict => true})),
    ?TRY({unreachable_address, [[k1_3], '*', [k3_2]]}, nested_maps:remove([[k1_3], '*', [k3_2]], Map, #{strict => true})),
    ok.

wildcard_non_strict_remove_exceptions(Map) ->
    ok = common_wildcard_remove_exceptions(Map, #{strict => false}),
    ok.

common_wildcard_remove_exceptions(Map, Options) ->
    ?TRY({badmap, {'*',v1_1}, Map}, nested_maps:remove([[k1_1], '*'], Map, Options)),
    ?TRY({badmap, {'*',v1_1}, Map}, nested_maps:remove(['*', '*', '*'], Map, Options)),
    ok.

%%% Group

group_strict_remove_operations(Map) ->
    ok = common_group_remove_operations(Map, #{strict => true}).

group_non_strict_remove_operations(Map) ->
    ok = common_group_remove_operations(Map, #{strict => false}),

    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [k2_1], [k3_1, ne_key]], Map, #{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [k2_2], [ne_key, k3_2]], Map, #{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [k2_2, ne_key], [k3_3]], Map, #{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2, ne_key], [k2_1], [k3_2]], Map, #{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [k2_1, ne_key], [k3_1, ne_key]], Map, #{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [ne_key, k2_2], [ne_key, k3_3]], Map, #{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2, ne_key], [k2_1, ne_key], [k3_1, ne_key]], Map, #{strict => false})),
    ?TRY(Map, nested_maps:remove([[ne_key, k1_3], [ne_key, k2_2], [ne_key, k3_3]], Map, #{strict => false})),
    ok.

common_group_remove_operations(Map, Options) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [k2_1], [k3_1, k3_2]], Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [k2_2], [k3_2, k3_3]], Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1},
                     k2_2 => #{k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:remove([[k1_2], [k2_1, k2_2], [k3_2]], Map, Options)),
    ok.

group_strict_remove_exceptions(Map) ->
    ok = common_group_remove_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:remove([[k1_2], [k2_1], [k3_1, ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[k1_2], [k2_1, ne_key], [k3_1]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[k1_2], [ne_key, k2_2], [k3_2]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[k1_2, ne_key], [k2_1], [k3_1]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[ne_key, k1_3], [k2_2], [k3_2]], Map, #{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:remove([[k1_2], [k2_1, ne_key], [k3_1, ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[k1_2], [ne_key, k2_2], [ne_key, k3_2]], Map, #{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:remove([[k1_2, ne_key], [k2_1, ne_key], [k3_1, ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:remove([[ne_key, k1_3], [ne_key, k2_2], [ne_key, k3_2]], Map, #{strict => true})),
    ok.

group_non_strict_remove_exceptions(Map) ->
    ok = common_group_remove_exceptions(Map, #{strict => false}),
    ok.

common_group_remove_exceptions(Map, Options) ->
    ?TRY({badmap, {[k2_1,k2_2],v1_1}, Map}, nested_maps:remove([[k1_1], [k2_1, k2_2], [k3_1]], Map, Options)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% REMOVE_WITH Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Simple
simple_strict_remove_with_operations(Map) ->
    ok = common_simple_remove_with_operations(Map, #{strict => true}).

simple_non_strict_remove_with_operations(Map) ->
    ok = common_simple_remove_with_operations(Map, #{strict => false}),

    ?TRY(Map, nested_maps:remove_with([], ?FUN_GET_REMOVE_TAKE, Map, #{strict => false})),
    ?TRY(Map, nested_maps:remove_with([[k1_2], [k2_1], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => false})),
    ok.

common_simple_remove_with_operations(Map, Options) ->
    ?TRY(Map, nested_maps:remove_with([[k1_2], [k2_1], [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                     k2_2 => #{k3_3 => 40}},
           k1_3 => #{}}, nested_maps:remove_with([[k1_2], [k2_2], [k3_2]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ok.

simple_strict_remove_with_exceptions(Map) ->
    ok = common_simple_remove_with_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:remove_with([[k1_2], [k2_1], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [ne_key]]},
         nested_maps:remove_with([[k1_2], [], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ?TRY({badarg, {'Address', []}}, nested_maps:remove_with([], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ok.

simple_non_strict_remove_with_exceptions(Map) ->
    ok = common_simple_remove_with_exceptions(Map, #{strict => false}),
    ok.

common_simple_remove_with_exceptions(Map, Options) ->
    Bad_fun = fun(_K, V) when V > 20 -> not_true; (_, _) -> not_false end,
    ?TRY({badarg, {'Function', Bad_fun}}, nested_maps:remove_with([[k1_2], [k2_1], [k3_1]], Bad_fun, Map, Options)),

    Bad_fun2 = fun(_K, _V) -> 1 / 0 end,
    ?TRY({badfun, {badarith, '*'}}, nested_maps:remove_with([[k1_2], [k2_2], [k3_2]], Bad_fun2, Map, Options)),

    ?TRY({badfun, {got_throw, '*'}}, nested_maps:remove_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_THROW, Map, Options)),
    ?TRY({badfun, {got_error, '*'}}, nested_maps:remove_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_ERROR, Map, Options)),
    ?TRY({badfun, {got_exit, '*'}}, nested_maps:remove_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_EXIT, Map, Options)),

    ?TRY({badmap, {[k2_1],v1_1}, Map}, nested_maps:remove_with([[k1_1], [k2_1], [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:remove_with(not_a_list, ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}},
         nested_maps:remove_with([[k1_2], [k2_1], [k3_1]], ?FUN_GET_REMOVE_TAKE, not_a_map, Options)),
    ?TRY({badarg, {'Address', [[k1_2], not_a_list, [k3_2]]}},
         nested_maps:remove_with([[k1_2], not_a_list, [k3_2]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY({badarg, {'Options', not_strict}},
         nested_maps:remove_with([[k1_2], [k2_1]], ?FUN_GET_REMOVE_TAKE, Map, not_strict)),

    ok.

%%% Wildcard

wildcard_strict_remove_with_operations(Map) ->
    ok = common_wildcard_remove_with_operations(Map, #{strict => true}).

wildcard_non_strict_remove_with_operations(Map) ->
    ok = common_wildcard_remove_with_operations(Map, #{strict => false}),
    ?TRY(Map, nested_maps:remove_with([[k1_2], [ne_key], '*'], ?FUN_GET_REMOVE_TAKE, Map, #{strict => false})),
    ok.

common_wildcard_remove_with_operations(Map, Options) ->
    ?TRY(Map, nested_maps:remove_with([[k1_2], [k2_1], '*'], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                     k2_2 => #{}},
           k1_3 => #{}}, nested_maps:remove_with([[k1_2], [k2_2], '*'], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ok.

wildcard_strict_remove_with_exceptions(Map) ->
    ok = common_wildcard_remove_with_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:remove_with([[k1_2], '*', [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], '*', []]},
         nested_maps:remove_with([[k1_2], '*', []], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ?TRY({unreachable_address, [[k1_3], '*', [k3_1]]},
         nested_maps:remove_with([[k1_3], '*', [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ok.

wildcard_non_strict_remove_with_exceptions(Map) ->
    ok = common_wildcard_remove_with_exceptions(Map, #{strict => false}),
    ok.

common_wildcard_remove_with_exceptions(Map, Options) ->
    ?TRY({badmap, {'*',v1_1}, Map}, nested_maps:remove_with([[k1_1], '*', '*'], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}},
         nested_maps:remove_with([[k1_2], '*', [k3_2]], ?FUN_GET_REMOVE_TAKE, not_a_map, Options)),
    ?TRY({badarg, {'Address', [[k1_2], not_a_list, '*']}},
         nested_maps:remove_with([[k1_2], not_a_list, '*'], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ok.

%%% Group

group_strict_remove_with_operations(Map) ->
    ok = common_group_remove_with_operations(Map, #{strict => true}).

group_non_strict_remove_with_operations(Map) ->
    ok = common_group_remove_with_operations(Map, #{strict => false}),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                     k2_2 => #{}},
           k1_3 => #{}}, nested_maps:remove_with([[k1_2], [ne_key, k2_2], '*'], ?FUN_GET_REMOVE_TAKE, Map, #{strict => false})),
    ok.

common_group_remove_with_operations(Map, Options) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                     k2_2 => #{}},
           k1_3 => #{}}, nested_maps:remove_with([[k1_2], [k2_1, k2_2], '*'], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ok.

group_strict_remove_with_exceptions(Map) ->
    ok = common_group_remove_with_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:remove_with([[k1_2], [k2_1, k2_2], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ok.

group_non_strict_remove_with_exceptions(Map) ->
    ok = common_group_remove_with_exceptions(Map, #{strict => false}),
    ok.

common_group_remove_with_exceptions(Map, Options) ->
    ?TRY({badmap, {[k2_1,k2_2],v1_1}, Map}, nested_maps:remove_with([[k1_1], [k2_1, k2_2], [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}},
         nested_maps:remove_with([[k1_2], [k2_1, k2_2], [k3_1]], ?FUN_GET_REMOVE_TAKE, not_a_map, Options)),
    ?TRY({badarg, {'Address', [[k1_2], [k2_1, k2_2], not_a_list]}},
         nested_maps:remove_with([[k1_2], [k2_1, k2_2], not_a_list], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% TAKE Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Simple

simple_strict_take_operations(Map) ->
    ok = common_simple_take_operations(Map, #{strict => true}).

simple_non_strict_take_operations(Map) ->
    ok = common_simple_take_operations(Map, #{strict => false}),

    ?TRY({[], Map}, nested_maps:take([[ne_key], [k2_1], [k3_1]], Map, #{strict => false})),
    ?TRY({[], Map}, nested_maps:take([[ne_key], [k2_1], [ne_key]], Map, #{strict => false})),
    ?TRY({[], Map}, nested_maps:take([[ne_key], [ne_key], [ne_key]], Map, #{strict => false})),
    ?TRY({[], Map}, nested_maps:take([[k1_2], [ne_key], [ne_key]], Map, #{strict => false})),
    ?TRY({[], Map}, nested_maps:take([[k1_2], [k2_1], [ne_key]], Map, #{strict => false})),
    ?TRY({[], Map}, nested_maps:take([[k1_2], [ne_key]], Map, #{strict => false})),
    ?TRY({[], Map}, nested_maps:take([[ne_key]], Map, #{strict => false})),

    ?TRY({[], Map}, nested_maps:take([[]], Map, #{strict => false})),
    ?TRY({[], Map}, nested_maps:take([[k1_2], []], Map, #{strict => false})),
    ?TRY({[], Map}, nested_maps:take([[k1_2], [], [k3_1]], Map, #{strict => false})),

    ?TRY({[], Map}, nested_maps:take([], Map, #{strict => false})),
    ok.

common_simple_take_operations(Map, Options) ->
    ?TRY({[v1_1], #{k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                              k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
                    k1_3 => #{}}}, nested_maps:take([[k1_1]], Map, Options)),
    ?TRY({[v3_1_1], #{k1_1 => v1_1,
                      k1_2 => #{k2_1 => #{k3_2 => v3_2_1},
                                k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
                      k1_3 => #{}}}, nested_maps:take([[k1_2], [k2_1], [k3_1]], Map, Options)),
    ?TRY({[v3_2_1], #{k1_1 => v1_1,
                      k1_2 => #{k2_1 => #{k3_1 => v3_1_1},
                                k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
                      k1_3 => #{}}}, nested_maps:take([[k1_2], [k2_1], [k3_2]], Map, Options)),
    ?TRY({[v3_2_2], #{k1_1 => v1_1,
                      k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                                k2_2 => #{k3_3 => v3_3_2}},
                      k1_3 => #{}}}, nested_maps:take([[k1_2], [k2_2], [k3_2]], Map, Options)),
    ?TRY({[v3_3_2], #{k1_1 => v1_1,
                      k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                                k2_2 => #{k3_2 => v3_2_2}},
                      k1_3 => #{}}}, nested_maps:take([[k1_2], [k2_2], [k3_3]], Map, Options)),
    ?TRY({[#{}], #{k1_1 => v1_1,
                   k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                             k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}}
    }},  nested_maps:take([[k1_3]], Map, Options)),
    ?TRY({[#{k3_1 => v3_1_1, k3_2 => v3_2_1}], #{k1_1 => v1_1,
                                                 k1_2 => #{k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
                                                 k1_3 => #{}}}, nested_maps:take([[k1_2], [k2_1]], Map, Options)),
    ?TRY({[#{k3_2 => v3_2_2, k3_3 => v3_3_2}], #{k1_1 => v1_1,
                                                 k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1}},
                                                 k1_3 => #{}}}, nested_maps:take([[k1_2], [k2_2]], Map, Options)),
    ?TRY({[#{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
             k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}}], #{k1_1 => v1_1, k1_3 => #{}}},
         nested_maps:take([[k1_2]], Map, Options)),
    ok.
simple_strict_take_exceptions(Map) ->
    % Input parameters
    ok = common_simple_take_exceptions(Map, #{strict => true}),

    % Undefined keys
    ?TRY({badkey, ne_key}, nested_maps:take([[ne_key], [k2_1], [k3_2]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[ne_key], [k2_1], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[ne_key], [ne_key], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[k1_2], [ne_key], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[k1_2], [k2_1], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[k1_2], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[ne_key]], Map, #{strict => true})),

% Missing keys
    ?TRY({badarg, {'Address', []}}, nested_maps:take([], Map, #{strict => true})),
    ?TRY({bad_address, [[]]}, nested_maps:take([[]], Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], []]}, nested_maps:take([[k1_2], []], Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [k3_2]]}, nested_maps:take([[k1_2], [], [k3_2]], Map, #{strict => true})),
    ok.

simple_non_strict_take_exceptions(Map) ->
    ok = common_simple_take_exceptions(Map, #{strict => false}),
    ok.

common_simple_take_exceptions(Map, Options) ->
    ?TRY({badmap, {[k2_1],v1_1}, Map}, nested_maps:take([[k1_1], [k2_1]], Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:take([[k1_2], [k2_1], [k3_2]], not_a_map, Options)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:take(not_a_list, Map, Options)),
    ?TRY({badarg, {'Options', not_strict}}, nested_maps:take([[k1_2], [k2_1]], Map, not_strict)),
    ok.

%%% Wildcard

wildcard_strict_take_operations(Map) ->
    ok = common_wildcard_take_operations(Map, #{strict => true}).

wildcard_non_strict_take_operations(Map) ->
    ok = common_wildcard_take_operations(Map, #{strict => false}),

    ?TRY({[], Map}, nested_maps:take([[k1_2], [ne_key], '*'], Map, #{strict => false})),
    ?TRY({[], Map}, nested_maps:take([[k1_2], '*', [ne_key]], Map, #{strict => false})),
    ?TRY({[], Map}, nested_maps:take([[ne_key], [ne_key], '*'], Map, #{strict => false})),
    ?TRY({[], Map}, nested_maps:take([[ne_key], [k2_1], '*'], Map, #{strict => false})),

    ?TRY({[v3_1_1],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{k3_2 => v3_2_1}, k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
            k1_3 => #{}}}, nested_maps:take([[k1_2], '*', [k3_1]], Map, #{strict => false})),
    ?TRY({[v3_3_2],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1}, k2_2 => #{k3_2 => v3_2_2}},
            k1_3 => #{}}}, nested_maps:take([[k1_2], '*', [k3_3]], Map, #{strict => false})),
    ?TRY({[], Map}, nested_maps:take([[k1_3], '*', [k3_2]], Map, #{strict => false})),
    ok.

common_wildcard_take_operations(Map, Options) ->
    ?TRY({[v3_3_2, v3_2_2],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1}, k2_2 => #{}},
            k1_3 => #{}}}, nested_maps:take([[k1_2], [k2_2], '*'], Map, Options)),
    ?TRY({[v3_3_2, v3_2_2, v3_2_1, v3_1_1],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{}, k2_2 => #{}},
            k1_3 => #{}}}, nested_maps:take([[k1_2], '*', '*'], Map, Options)),
    ?TRY({[v3_2_2, v3_2_1],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{k3_1 => v3_1_1},
                      k2_2 => #{k3_3 => v3_3_2}},
            k1_3 => #{}}}, nested_maps:take([[k1_2], '*', [k3_2]], Map, Options)),
    ?TRY({[#{k3_2 => v3_2_2, k3_3 => v3_3_2},
           #{k3_1 => v3_1_1, k3_2 => v3_2_1}],
          #{k1_1 => v1_1,
            k1_2 => #{},
            k1_3 => #{}}}, nested_maps:take([[k1_2], '*'], Map, Options)),
    ?TRY({[#{},
           #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
             k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           v1_1],
          #{}}, nested_maps:take(['*'], Map, Options)),
    ok.
wildcard_strict_take_exceptions(Map) ->
    ok = common_wildcard_take_exceptions(Map, #{strict => true}),
    ?TRY({badkey, k3_1}, nested_maps:take([[k1_2], '*', [k3_1]], Map, #{strict => true})),
    ?TRY({badkey, k3_3}, nested_maps:take([[k1_2], '*', [k3_3]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[k1_2], [ne_key], '*'], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[k1_2], '*', [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[ne_key], [ne_key], '*'], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[ne_key], [k2_1], '*'], Map, #{strict => true})),
    ?TRY({unreachable_address, [[k1_3], '*', [k3_2]]}, nested_maps:take([[k1_3], '*', [k3_2]], Map, #{strict => true})),
    ok.

wildcard_non_strict_take_exceptions(Map) ->
    ok = common_wildcard_take_exceptions(Map, #{strict => false}),
    ok.

common_wildcard_take_exceptions(Map, Options) ->
    ?TRY({badmap, {'*',v1_1}, Map}, nested_maps:take([[k1_1], '*'], Map, Options)),
    ?TRY({badmap, {'*',v1_1}, Map}, nested_maps:take(['*', '*', '*'], Map, Options)),
    ok.

%%% Group

group_strict_take_operations(Map) ->
    ok = common_group_take_operations(Map, #{strict => true}).

group_non_strict_take_operations(Map) ->
    ok = common_group_take_operations(Map, #{strict => false}),

    ?TRY({[v3_1_1],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{k3_2 => v3_2_1},
                      k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
            k1_3 => #{}}}, nested_maps:take([[k1_2], [k2_1], [k3_1, ne_key]], Map, #{strict => false})),
    ?TRY({[v3_2_2],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                      k2_2 => #{k3_3 => v3_3_2}},
            k1_3 => #{}}}, nested_maps:take([[k1_2], [k2_2], [ne_key, k3_2]], Map, #{strict => false})),
    ?TRY({[v3_3_2],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                      k2_2 => #{k3_2 => v3_2_2}},
            k1_3 => #{}}}, nested_maps:take([[k1_2], [k2_2, ne_key], [k3_3]], Map, #{strict => false})),
    ?TRY({[v3_2_1],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{k3_1 => v3_1_1},
                      k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
            k1_3 => #{}}}, nested_maps:take([[k1_2, ne_key], [k2_1], [k3_2]], Map, #{strict => false})),
    ?TRY({[v3_1_1],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{k3_2 => v3_2_1},
                      k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
            k1_3 => #{}}}, nested_maps:take([[k1_2], [k2_1, ne_key], [k3_1, ne_key]], Map, #{strict => false})),
    ?TRY({[v3_3_2],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                      k2_2 => #{k3_2 => v3_2_2}},
            k1_3 => #{}}}, nested_maps:take([[k1_2], [ne_key, k2_2], [ne_key, k3_3]], Map, #{strict => false})),
    ?TRY({[v3_1_1],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{k3_2 => v3_2_1},
                      k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
            k1_3 => #{}}}, nested_maps:take([[k1_2, ne_key], [k2_1, ne_key], [k3_1, ne_key]], Map, #{strict => false})),
    ?TRY({[], Map}, nested_maps:take([[ne_key, k1_3], [ne_key, k2_2], [ne_key, k3_3]], Map, #{strict => false})),
    ok.

common_group_take_operations(Map, Options) ->
    ?TRY({[v3_2_1, v3_1_1],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{},
                      k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
            k1_3 => #{}}}, nested_maps:take([[k1_2], [k2_1], [k3_1, k3_2]], Map, Options)),
    ?TRY({[v3_3_2, v3_2_2],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                      k2_2 => #{}},
            k1_3 => #{}}}, nested_maps:take([[k1_2], [k2_2], [k3_2, k3_3]], Map, Options)),
    ?TRY({[v3_2_2, v3_2_1],
          #{k1_1 => v1_1,
            k1_2 => #{k2_1 => #{k3_1 => v3_1_1},
                      k2_2 => #{k3_3 => v3_3_2}},
            k1_3 => #{}}}, nested_maps:take([[k1_2], [k2_1, k2_2], [k3_2]], Map, Options)),
    ok.

group_strict_take_exceptions(Map) ->
    ok = common_group_take_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:take([[k1_2], [k2_1], [k3_1, ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[k1_2], [k2_1, ne_key], [k3_1]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[k1_2], [ne_key, k2_2], [k3_2]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[k1_2, ne_key], [k2_1], [k3_1]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[ne_key, k1_3], [k2_2], [k3_2]], Map, #{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:take([[k1_2], [k2_1, ne_key], [k3_1, ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[k1_2], [ne_key, k2_2], [ne_key, k3_2]], Map, #{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:take([[k1_2, ne_key], [k2_1, ne_key], [k3_1, ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:take([[ne_key, k1_3], [ne_key, k2_2], [ne_key, k3_2]], Map, #{strict => true})),
    ok.

group_non_strict_take_exceptions(Map) ->
    ok = common_group_take_exceptions(Map, #{strict => false}),
    ok.

common_group_take_exceptions(Map, Options) ->
    ?TRY({badmap, {[k2_1,k2_2],v1_1}, Map}, nested_maps:take([[k1_1], [k2_1, k2_2], [k3_1]], Map, Options)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% REMOVE_WITH Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Simple
simple_strict_take_with_operations(Map) ->
    ok = common_simple_take_with_operations(Map, #{strict => true}).

simple_non_strict_take_with_operations(Map) ->
    ok = common_simple_take_with_operations(Map, #{strict => false}),

    ?TRY({[], Map}, nested_maps:take_with([], ?FUN_GET_REMOVE_TAKE, Map, #{strict => false})),
    ?TRY({[], Map}, nested_maps:take_with([[k1_2], [k2_1], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => false})),
    ok.

common_simple_take_with_operations(Map, Options) ->
    ?TRY({[], Map}, nested_maps:take_with([[k1_2], [k2_1], [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY({[30], #{k1_1 => v1_1,
                  k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                            k2_2 => #{k3_3 => 40}},
                  k1_3 => #{}}}, nested_maps:take_with([[k1_2], [k2_2], [k3_2]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ok.

simple_strict_take_with_exceptions(Map) ->
    ok = common_simple_take_with_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:take_with([[k1_2], [k2_1], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [ne_key]]},
         nested_maps:take_with([[k1_2], [], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ?TRY({badarg, {'Address', []}}, nested_maps:take_with([], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ok.

simple_non_strict_take_with_exceptions(Map) ->
    ok = common_simple_take_with_exceptions(Map, #{strict => false}),
    ok.

common_simple_take_with_exceptions(Map, Options) ->
    Bad_fun = fun(_K, V) when V > 20 -> not_true; (_, _) -> not_false end,
    ?TRY({badarg, {'Function', Bad_fun}}, nested_maps:take_with([[k1_2], [k2_1], [k3_1]], Bad_fun, Map, Options)),

    Bad_fun2 = fun(_K, _V) -> 1 / 0 end,
    ?TRY({badfun, {badarith, '*'}}, nested_maps:take_with([[k1_2], [k2_2], [k3_2]], Bad_fun2, Map, Options)),

    ?TRY({badfun, {got_throw, '*'}}, nested_maps:take_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_THROW, Map, Options)),
    ?TRY({badfun, {got_error, '*'}}, nested_maps:take_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_ERROR, Map, Options)),
    ?TRY({badfun, {got_exit, '*'}}, nested_maps:take_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_EXIT, Map, Options)),

    ?TRY({badmap, {[k2_1],v1_1}, Map}, nested_maps:take_with([[k1_1], [k2_1], [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:take_with(not_a_list, ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}},
         nested_maps:take_with([[k1_2], [k2_1], [k3_1]], ?FUN_GET_REMOVE_TAKE, not_a_map, Options)),
    ?TRY({badarg, {'Address', [[k1_2], not_a_list, [k3_2]]}},
         nested_maps:take_with([[k1_2], not_a_list, [k3_2]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY({badarg, {'Options', not_strict}},
         nested_maps:take_with([[k1_2], [k2_1]], ?FUN_GET_REMOVE_TAKE, Map, not_strict)),

    ok.

%%% Wildcard

wildcard_strict_take_with_operations(Map) ->
    ok = common_wildcard_take_with_operations(Map, #{strict => true}).

wildcard_non_strict_take_with_operations(Map) ->
    ok = common_wildcard_take_with_operations(Map, #{strict => false}),
    ?TRY({[], Map}, nested_maps:take_with([[k1_2], [ne_key], '*'], ?FUN_GET_REMOVE_TAKE, Map, #{strict => false})),
    ok.

common_wildcard_take_with_operations(Map, Options) ->
    ?TRY({[], Map}, nested_maps:take_with([[k1_2], [k2_1], '*'], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY({[40, 30], #{k1_1 => v1_1,
                      k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                                k2_2 => #{}},
                      k1_3 => #{}}}, nested_maps:take_with([[k1_2], [k2_2], '*'], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ok.

wildcard_strict_take_with_exceptions(Map) ->
    ok = common_wildcard_take_with_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:take_with([[k1_2], '*', [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], '*', []]}, nested_maps:take_with([[k1_2], '*', []], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ?TRY({unreachable_address, [[k1_3], '*', [k3_1]]},
         nested_maps:take_with([[k1_3], '*', [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ok.

wildcard_non_strict_take_with_exceptions(Map) ->
    ok = common_wildcard_take_with_exceptions(Map, #{strict => false}),
    ok.

common_wildcard_take_with_exceptions(Map, Options) ->
    ?TRY({badmap, {'*',v1_1}, Map}, nested_maps:take_with([[k1_1], '*', '*'], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}},
         nested_maps:take_with([[k1_2], '*', [k3_2]], ?FUN_GET_REMOVE_TAKE, not_a_map, Options)),
    ?TRY({badarg, {'Address', [[k1_2], not_a_list, '*']}},
         nested_maps:take_with([[k1_2], not_a_list, '*'], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ok.

%%% Group

group_strict_take_with_operations(Map) ->
    ok = common_group_take_with_operations(Map, #{strict => true}).

group_non_strict_take_with_operations(Map) ->
    ok = common_group_take_with_operations(Map, #{strict => false}),
    ?TRY({[40, 30], #{k1_1 => v1_1,
                      k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                                k2_2 => #{}},
                      k1_3 => #{}}},
         nested_maps:take_with([[k1_2], [ne_key, k2_2], '*'], ?FUN_GET_REMOVE_TAKE, Map, #{strict => false})),
    ok.

common_group_take_with_operations(Map, Options) ->
    ?TRY({[40, 30], #{k1_1 => v1_1,
                      k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                                k2_2 => #{}},
                      k1_3 => #{}}},
        nested_maps:take_with([[k1_2], [k2_1, k2_2], '*'], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ok.

group_strict_take_with_exceptions(Map) ->
    ok = common_group_take_with_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:take_with([[k1_2], [k2_1, k2_2], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ok.

group_non_strict_take_with_exceptions(Map) ->
    ok = common_group_take_with_exceptions(Map, #{strict => false}),
    ok.

common_group_take_with_exceptions(Map, Options) ->
    ?TRY({badmap, {[k2_1,k2_2],v1_1}, Map}, nested_maps:take_with([[k1_1], [k2_1, k2_2], [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}},
         nested_maps:take_with([[k1_2], [k2_1, k2_2], [k3_1]], ?FUN_GET_REMOVE_TAKE, not_a_map, Options)),
    ?TRY({badarg, {'Address', [[k1_2], [k2_1, k2_2], not_a_list]}},
         nested_maps:take_with([[k1_2], [k2_1, k2_2], not_a_list], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reference_map() ->
    #{k1_1 => v1_1,
      k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
      k1_3 => #{}}.

reference_map(integer_values) ->
    #{k1_1 => v1_1,
      k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                k2_2 => #{k3_2 => 30, k3_3 => 40}},
      k1_3 => #{}}.

eq(Expected, Value) ->
    case compare(Expected, Value) of
        true -> true;
        false ->
            ct:fail("~nExpected: ~p~nGot: ~p~n", [Expected, Value])
    end.


eq2(ExpectedList, Value) ->
    case lists:any(fun(Expected) -> compare(Expected, Value) end, ExpectedList) of
        true -> true;
        false ->
            ct:fail("~nExpected one of: ~p~nGot: ~p~n", [ExpectedList, Value])
    end.

compare(List1, List2) when is_list(List1), is_list(List2) ->
    case lists:sort(List1) == lists:sort(List2) of
        true ->
            true;
        _ ->
            false
    end;

compare(Map1, Map2) when is_map(Map1), is_map(Map2) ->
    case Map1 =:= Map2 of
        true ->
            true;
        _ ->
            false
    end;

%% Since it is not possible to pass guard expression with anonymous variable as
%% parameter the wildcard '*' is used as mark where the anonymous variable
%% should be placed.

compare({Term1, {Term2, '*'}} = E1, E2) ->
    case E2 of
        {Term1, {Term2, _}} ->
            true;
        _ ->
            false
    end;

compare({Term1, '*'} = E1, E2) ->
    case E2 of
        {Term1, _} ->
            true;
        _ ->
            false
    end;

compare(E1, E2) ->
    case E2 of
        E1 ->
            true;
        _ ->
            false
    end.


