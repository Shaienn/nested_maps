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
    test_of_combinations/1
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Include Files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("common_test/include/ct.hrl").

-define(TRY(ExpectedResult, CodeBlock),
    [eq(ExpectedResult, Result) ||
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
-define(FUN_GET_REMOVE_TAKE, fun(_Key, Value) when Value > 20 -> true;
                                (_, _) -> false end).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [
        test_of_combinations
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction and Destruction for the Test Suite
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config) ->
    [{map, #{}} | Config].

end_per_suite(_Config) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_of_combinations(_Config) ->

%% @TODO Add test for different types of data
%%    Group1 = [group1, 15, list_to_pid("<0.39.0>"), {group1, tuple}, "Group1", []],
%%    Group2 = [group2, 20, list_to_pid("<0.40.0>"), {group2, tuple}, "Group2"],
%%    Subgroup1 = [subgroup1, 30, list_to_pid("<0.41.0>"), {subgroup1, tuple}, "Subgroup1"],
%%    Subgroup2 = [subgroup2, 35, list_to_pid("<0.42.0>"), {subgroup2, tuple}, "Subgroup2", []],
%%    UnexistedElement = [unexisted_element, 50, list_to_pid("<0.99.0>"), {unexisted_element, tuple}, "UnexistedElement"],
%%    Key1 = [key1, 60, list_to_pid("<0.44.0>"), {key1, tuple}, "Key1", []],
%%    Key2 = [key2, 65, list_to_pid("<0.45.0>"), {key2, tuple}, "Key2"],
%%    Value1 = [value1, "Value1", 50, <<70, 80>>, [1, 2, 3], list_to_pid("<0.46.0>"), {value1, tuple}],
%%    Value2 = [value2, "Value2", 100, <<75, 85>>, [4, 5, 6], list_to_pid("<0.47.0>"), {value2, tuple}],
%%
%%    NumOfRandomCombinations = lists:seq(1, 100),
%%    EL = [Group1, Group2, Subgroup1, Subgroup2, Key1, Key2, Value1, Value2, UnexistedElement],
%%    RandomCombinations = [[lists:nth(rand:uniform(length(E)), E) || E <- EL] || _C <- NumOfRandomCombinations],

    Map = reference_map(),
    IntegerMap = reference_map(integer_values),

    %%% GET

    simple_strict_get_operations(Map),
    simple_non_strict_get_operations(Map),
    simple_strict_get_exceptions(Map),
    simple_non_strict_get_exceptions(Map),

    wildcard_strict_get_operations(Map),
    wildcard_non_strict_get_operations(Map),
    wildcard_strict_get_exceptions(Map),
    wildcard_non_strict_get_exceptions(Map),

    group_strict_get_operations(Map),
    group_strict_get_exceptions(Map),
    group_non_strict_get_operations(Map),
    group_non_strict_get_exceptions(Map),

    %%% GET_WITH

    simple_strict_get_with_operations(IntegerMap),
    simple_non_strict_get_with_operations(IntegerMap),
    simple_strict_get_with_exceptions(IntegerMap),
    simple_non_strict_get_with_exceptions(IntegerMap),

    wildcard_strict_get_with_operations(IntegerMap),
    wildcard_non_strict_get_with_operations(IntegerMap),
    wildcard_strict_get_with_exceptions(IntegerMap),
    wildcard_non_strict_get_with_exceptions(IntegerMap),

    group_strict_get_with_operations(IntegerMap),
    group_non_strict_get_with_operations(IntegerMap),
    group_strict_get_with_exceptions(IntegerMap),
    group_non_strict_get_with_exceptions(IntegerMap),

    %%% PUT

    simple_strict_put_operations(Map),
    simple_non_strict_put_operations(Map),
    simple_strict_put_exceptions(Map),
    simple_non_strict_put_exceptions(Map),

    wildcard_strict_put_operations(Map),
    wildcard_non_strict_put_operations(Map),
    wildcard_strict_put_exceptions(Map),
    wildcard_non_strict_put_exceptions(Map),

    group_strict_put_operations(Map),
    group_strict_put_exceptions(Map),
    group_non_strict_put_operations(Map),
    group_non_strict_put_exceptions(Map),

    %%% PUT_WITH

    simple_strict_put_with_operations(IntegerMap),
    simple_non_strict_put_with_operations(IntegerMap),
    simple_strict_put_with_exceptions(IntegerMap),
    simple_non_strict_put_with_exceptions(IntegerMap),

    wildcard_strict_put_with_operations(IntegerMap),
    wildcard_non_strict_put_with_operations(IntegerMap),
    wildcard_strict_put_with_exceptions(IntegerMap),
    wildcard_non_strict_put_with_exceptions(IntegerMap),

    group_strict_put_with_operations(IntegerMap),
    group_strict_put_with_exceptions(IntegerMap),
    group_non_strict_put_with_operations(IntegerMap),
    group_non_strict_put_with_exceptions(IntegerMap),

    %%% KEYS

    simple_strict_keys_operations(Map),
    simple_non_strict_keys_operations(Map),
    simple_strict_keys_exceptions(Map),
    simple_non_strict_keys_exceptions(Map),

    wildcard_strict_keys_operations(Map),
    wildcard_non_strict_keys_operations(Map),
    wildcard_strict_keys_exceptions(Map),
    wildcard_non_strict_keys_exceptions(Map),

    group_strict_keys_operations(Map),
    group_strict_keys_exceptions(Map),
    group_non_strict_keys_operations(Map),
    group_non_strict_keys_exceptions(Map),

    %%% KEYS_WITH

    simple_strict_keys_with_operations(IntegerMap),
    simple_non_strict_keys_with_operations(IntegerMap),
    simple_strict_keys_with_exceptions(IntegerMap),
    simple_non_strict_keys_with_exceptions(IntegerMap),

    wildcard_strict_keys_with_operations(IntegerMap),
    wildcard_non_strict_keys_with_operations(IntegerMap),
    wildcard_strict_keys_with_exceptions(IntegerMap),
    wildcard_non_strict_keys_with_exceptions(IntegerMap),

    group_strict_keys_with_operations(IntegerMap),
    group_strict_keys_with_exceptions(IntegerMap),
    group_non_strict_keys_with_operations(IntegerMap),
    group_non_strict_keys_with_exceptions(IntegerMap),

    %%% UPDATE
    simple_strict_update_operations(Map),
    simple_non_strict_update_operations(Map),
    simple_strict_update_exceptions(Map),
    simple_non_strict_update_exceptions(Map),

    wildcard_strict_update_operations(Map),
    wildcard_non_strict_update_operations(Map),
    wildcard_strict_update_exceptions(Map),
    wildcard_non_strict_update_exceptions(Map),

    group_strict_update_operations(Map),
    group_strict_update_exceptions(Map),
    group_non_strict_update_operations(Map),
    group_non_strict_update_exceptions(Map),

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

simple_strict_get_operations(Map) ->
    ok = common_simple_get_operations(Map, #{strict => true}).

simple_non_strict_get_operations(Map) ->
    ok = common_simple_get_operations(Map, #{strict => false}),

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

common_simple_get_operations(Map, Options) ->
    ?TRY([v1_1], nested_maps:get([[k1_1]], Map, Options)),
    ?TRY([v3_1_1], nested_maps:get([[k1_2], [k2_1], [k3_1]], Map, Options)),
    ?TRY([v3_2_1], nested_maps:get([[k1_2], [k2_1], [k3_2]], Map, Options)),
    ?TRY([v3_2_2], nested_maps:get([[k1_2], [k2_2], [k3_2]], Map, Options)),
    ?TRY([v3_3_2], nested_maps:get([[k1_2], [k2_2], [k3_3]], Map, Options)),
    ?TRY([#{}], nested_maps:get([[k1_3]], Map, Options)),

    ?TRY([#{k3_1 => v3_1_1, k3_2 => v3_2_1}], nested_maps:get([[k1_2], [k2_1]], Map, Options)),
    ?TRY([#{k3_2 => v3_2_2, k3_3 => v3_3_2}], nested_maps:get([[k1_2], [k2_2]], Map, Options)),
    ?TRY([#{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
            k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}}], nested_maps:get([[k1_2]], Map, Options)),
    ok.

simple_strict_get_exceptions(Map) ->
    ok = common_simple_get_exceptions(Map, #{strict => true}),
    ?TRY({badarg, {'Address', []}}, nested_maps:get([], Map, #{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key], [k2_1], [k3_2]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key], [k2_1], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key], [ne_key], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [ne_key], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [k2_1], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key]], Map, #{strict => true})),

    ?TRY({bad_address, [[]]}, nested_maps:get([[]], Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], []]}, nested_maps:get([[k1_2], []], Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [k3_2]]}, nested_maps:get([[k1_2], [], [k3_2]], Map, #{strict => true})),
    ok.

simple_non_strict_get_exceptions(Map) ->
    ok = common_simple_get_exceptions(Map, #{strict => false}),
    ok.

common_simple_get_exceptions(Map, Options) ->
    ?TRY({badmap, Map}, nested_maps:get([[k1_1], [k2_1]], Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:get([[k1_2], [k2_1], [k3_2]], not_a_map, Options)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:get(not_a_list, Map, Options)),
    ?TRY({badarg, {'Address', [[k1_2], not_a_list, [k3_2]]}},
         nested_maps:get([[k1_2], not_a_list, [k3_2]], Map, Options)),
    ?TRY({badarg, {'Options', not_strict}}, nested_maps:get([[k1_2], [k2_1]], Map, not_strict)),
    ok.

%%% WILDCARD

wildcard_strict_get_operations(Map) ->
    ok = common_wildcard_get_operations(Map, #{strict => true}).

wildcard_non_strict_get_operations(Map) ->
    ok = common_wildcard_get_operations(Map, #{strict => false}),

    ?TRY([], nested_maps:get([[k1_2], [ne_key], '*'], Map, #{strict => false})),
    ?TRY([], nested_maps:get([[k1_2], '*', [ne_key]], Map, #{strict => false})),
    ?TRY([], nested_maps:get([[ne_key], [ne_key], '*'], Map, #{strict => false})),
    ?TRY([], nested_maps:get([[ne_key], [k2_1], '*'], Map, #{strict => false})),


    ?TRY([v3_1_1], nested_maps:get([[k1_2], '*', [k3_1]], Map, #{strict => false})),
    ?TRY([v3_3_2], nested_maps:get([[k1_2], '*', [k3_3]], Map, #{strict => false})),

    ?TRY([], nested_maps:get([[k1_3], '*', [k3_2]], Map, #{strict => false})),
    ok.

common_wildcard_get_operations(Map, Options) ->
    ?TRY([v3_2_2, v3_3_2], nested_maps:get([[k1_2], [k2_2], '*'], Map, Options)),
    ?TRY([v3_1_1, v3_2_1, v3_2_2, v3_3_2], nested_maps:get([[k1_2], '*', '*'], Map, Options)),
    ?TRY([v3_2_1, v3_2_2], nested_maps:get([[k1_2], '*', [k3_2]], Map, Options)),

    ?TRY([#{k3_1 => v3_1_1, k3_2 => v3_2_1}, #{k3_2 => v3_2_2, k3_3 => v3_3_2}],
         nested_maps:get([[k1_2], '*'], Map, Options)),
    ?TRY([v1_1, #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                  k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}}, #{}
         ], nested_maps:get(['*'], Map, Options)),
    ok.

wildcard_strict_get_exceptions(Map) ->
    ok = common_wildcard_get_exceptions(Map, #{strict => true}),
    ?TRY({badkey, k3_1}, nested_maps:get([[k1_2], '*', [k3_1]], Map, #{strict => true})),
    ?TRY({badkey, k3_3}, nested_maps:get([[k1_2], '*', [k3_3]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [ne_key], '*'], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], '*', [ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key], [ne_key], '*'], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key], [k2_1], '*'], Map, #{strict => true})),
    ?TRY({unreachable_address, [[k1_3], '*', [k3_2]]}, nested_maps:get([[k1_3], '*', [k3_2]], Map, #{strict => true})),
    ok.

wildcard_non_strict_get_exceptions(Map) ->
    ok = common_wildcard_get_exceptions(Map, #{strict => false}),
    ok.

common_wildcard_get_exceptions(Map, Options) ->
    ?TRY({badmap, Map}, nested_maps:get([[k1_1], '*'], Map, Options)),
    ?TRY({badmap, Map}, nested_maps:get(['*', '*', '*'], Map, Options)),
    ok.

%%% GROUP

group_strict_get_operations(Map) ->
    ok = common_group_get_operations(Map, #{strict => true}).

group_non_strict_get_operations(Map) ->
    ok = common_group_get_operations(Map, #{strict => false}),

    ?TRY([v3_2_1, v3_2_2, v3_3_2], nested_maps:get([[k1_2], [k2_1, k2_2], [k3_2, k3_3]], Map, #{strict => false})),

    ?TRY([v3_1_1], nested_maps:get([[k1_2], [k2_1], [k3_1, ne_key]], Map, #{strict => false})),
    ?TRY([v3_2_2], nested_maps:get([[k1_2], [k2_2], [ne_key, k3_2]], Map, #{strict => false})),
    ?TRY([v3_3_2], nested_maps:get([[k1_2], [k2_2, ne_key], [k3_3]], Map, #{strict => false})),
    ?TRY([v3_2_1], nested_maps:get([[k1_2, ne_key], [k2_1], [k3_2]], Map, #{strict => false})),

    ?TRY([v3_1_1], nested_maps:get([[k1_2], [k2_1, ne_key], [k3_1, ne_key]], Map, #{strict => false})),
    ?TRY([v3_3_2], nested_maps:get([[k1_2], [ne_key, k2_2], [ne_key, k3_3]], Map, #{strict => false})),

    ?TRY([v3_1_1], nested_maps:get([[k1_2, ne_key], [k2_1, ne_key], [k3_1, ne_key]], Map, #{strict => false})),
    ?TRY([], nested_maps:get([[ne_key, k1_3], [ne_key, k2_2], [ne_key, k3_3]], Map, #{strict => false})),
    ok.

common_group_get_operations(Map, Options) ->
    ?TRY([v3_1_1, v3_2_1], nested_maps:get([[k1_2], [k2_1], [k3_1, k3_2]], Map, Options)),
    ?TRY([v3_2_2, v3_3_2], nested_maps:get([[k1_2], [k2_2], [k3_2, k3_3]], Map, Options)),
    ?TRY([v3_2_1, v3_2_2], nested_maps:get([[k1_2], [k2_1, k2_2], [k3_2]], Map, Options)),
    ok.

group_strict_get_exceptions(Map) ->
    ok = common_group_get_exceptions(Map, #{strict => true}),

    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [k2_1], [k3_1, ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [k2_1, ne_key], [k3_1]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [ne_key, k2_2], [k3_2]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2, ne_key], [k2_1], [k3_1]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key, k1_3], [k2_2], [k3_2]], Map, #{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [k2_1, ne_key], [k3_1, ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2], [ne_key, k2_2], [ne_key, k3_2]], Map, #{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:get([[k1_2, ne_key], [k2_1, ne_key], [k3_1, ne_key]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:get([[ne_key, k1_3], [ne_key, k2_2], [ne_key, k3_2]], Map, #{strict => true})),
    ok.

group_non_strict_get_exceptions(Map) ->
    ok = common_group_get_exceptions(Map, #{strict => false}),
    ok.

common_group_get_exceptions(Map, Options) ->
    ?TRY({badmap, Map}, nested_maps:get([[k1_1], [k2_1, k2_2], [k3_1]], Map, Options)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% GET_WITH Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Simple

simple_strict_get_with_operations(Map) ->
    ok = common_simple_get_with_operations(Map, #{strict => true}).

simple_non_strict_get_with_operations(Map) ->
    ok = common_simple_get_with_operations(Map, #{strict => false}),

    ?TRY([], nested_maps:get_with([[k1_2], [k2_1], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => false})),
    ?TRY([], nested_maps:get_with([], ?FUN_GET_REMOVE_TAKE, Map, #{strict => false})),
    ok.

common_simple_get_with_operations(Map, Options) ->
    ?TRY([], nested_maps:get_with([[k1_2], [k2_1], [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY([30], nested_maps:get_with([[k1_2], [k2_2], [k3_2]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ok.

simple_strict_get_with_exceptions(Map) ->
    ok = common_simple_get_with_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:get_with([[k1_2], [k2_1], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [ne_key]]},
         nested_maps:get_with([[k1_2], [], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),

    ?TRY({badarg, {'Address', []}}, nested_maps:get_with([], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ok.

simple_non_strict_get_with_exceptions(Map) ->
    ok = common_simple_get_with_exceptions(Map, #{strict => false}),
    ok.

common_simple_get_with_exceptions(Map, Options) ->
    Fun1 = fun(V) when V > 20 -> true; (_) -> false end,
    ?TRY({badarg, {'Function', Fun1}}, nested_maps:get_with([[k1_2], [k2_1], [k3_1]], Fun1, Map, Options)),

    Fun2 = fun(_K, V) when V > 20 -> not_true; (_, _) -> not_false end,
    ?TRY({badarg, {'Function', Fun2}}, nested_maps:get_with([[k1_2], [k2_1], [k3_1]], Fun2, Map, Options)),

    Fun3 = fun(_K, V) when V > 20 -> 1 / 0; (_, _) -> false end,
    ?TRY({badfun, {badarith, '*'}}, nested_maps:get_with([[k1_2], [k2_2], [k3_2]], Fun3, Map, Options)),

    ?TRY({badfun, {got_throw, '*'}}, nested_maps:get_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_THROW, Map, Options)),
    ?TRY({badfun, {got_error, '*'}}, nested_maps:get_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_ERROR, Map, Options)),
    ?TRY({badfun, {got_exit, '*'}}, nested_maps:get_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_EXIT, Map, Options)),

    ?TRY({badmap, Map}, nested_maps:get_with([[k1_1], [k2_1], [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:get_with(not_a_list, ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}},
         nested_maps:get_with([[k1_2], [k2_1], [k3_1]], ?FUN_GET_REMOVE_TAKE, not_a_map, Options)),
    ?TRY({badarg, {'Address', [[k1_2], not_a_list, [k3_2]]}},
         nested_maps:get_with([[k1_2], not_a_list, [k3_2]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ok.

%%% Wildcard

wildcard_strict_get_with_operations(Map) ->
    ok = common_wildcard_get_with_operations(Map, #{strict => true}).

wildcard_non_strict_get_with_operations(Map) ->
    ok = common_wildcard_get_with_operations(Map, #{strict => false}),
    ?TRY([], nested_maps:get_with([[k1_2], [ne_key], '*'], ?FUN_GET_REMOVE_TAKE, Map, #{strict => false})),

    ok.

common_wildcard_get_with_operations(Map, Options) ->
    ?TRY([], nested_maps:get_with([[k1_2], [k2_1], '*'], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ?TRY([30, 40], nested_maps:get_with([[k1_2], [k2_2], '*'], ?FUN_GET_REMOVE_TAKE, Map, Options)),
    ok.

wildcard_strict_get_with_exceptions(Map) ->
    ok = common_wildcard_get_with_exceptions(Map, #{strict => true}),

    ?TRY({unreachable_address, [[k1_3], '*', [k3_1]]},
         nested_maps:get_with([[k1_3], '*', [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),

    ok.

wildcard_non_strict_get_with_exceptions(Map) ->
    ok = common_wildcard_get_with_exceptions(Map, #{strict => false}),
    ok.

common_wildcard_get_with_exceptions(_Map, _Options) ->
    ok.

%%% Group

group_strict_get_with_operations(Map) ->
    ok = common_group_get_with_operations(Map, #{strict => true}).

group_non_strict_get_with_operations(Map) ->
    ok = common_group_get_with_operations(Map, #{strict => false}),
    ?TRY([30, 40], nested_maps:get_with([[k1_2], [ne_key, k2_2], '*'], ?FUN_GET_REMOVE_TAKE, Map, #{strict => false})),
    ok.

common_group_get_with_operations(Map, Options) ->
    ?TRY([20, 30, 40], nested_maps:get_with([[k1_2], [k2_1, k2_2], '*'], ?FUN_KEYS, Map, Options)),
    ok.

group_strict_get_with_exceptions(Map) ->
    ok = common_group_get_with_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:get_with([[k1_2], [k2_1, k2_2], [ne_key]], ?FUN_GET_REMOVE_TAKE, Map, #{strict => true})),
    ok.

group_non_strict_get_with_exceptions(Map) ->
    ok = common_group_get_with_exceptions(Map, #{strict => false}),
    ok.

common_group_get_with_exceptions(_Map, _Options) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PUT Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% SIMPLE

simple_strict_put_operations(Map) ->
    ok = common_simple_put_operations(Map, #{strict => true}).

simple_non_strict_put_operations(Map) ->
    ok = common_simple_put_operations(Map, #{strict => false}),

    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{},
           k1_4 => #{k4_1 => #{k3_1 => v3_1}}}, nested_maps:put([[k1_4], [k4_1], [k3_1]], v3_1, Map, #{strict => false})),
    ?TRY(Map, nested_maps:put([[k1_4], [], [k3_1]], v3_1, Map, #{strict => false})),

    ?TRY(Map, nested_maps:put([], v3_1, Map, #{strict => false})),
    ok.

common_simple_put_operations(Map, Options) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1_new, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1], [k3_1]], v3_1_1_new, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2_new}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_2], [k3_3]], v3_3_2_new, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => v2_1,
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1]], v2_1, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => v1_2,
           k1_3 => #{}}, nested_maps:put([[k1_2]], v1_2, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{},
           k1_4 => v1_4}, nested_maps:put([[k1_4]], v1_4, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1, k3_3 => v3_3_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1], [k3_3]], v3_3_1, Map, Options)),
    ok.

simple_strict_put_exceptions(Map) ->
    ok = common_simple_put_exceptions(Map, #{strict => true}),
    ?TRY({badarg, {'Address', []}}, nested_maps:put([], v3_1_1_new, Map, #{strict => true})),
    ?TRY({badkey, k1_4}, nested_maps:put([[k1_4], [k4_1], [k3_1]], v3_1, Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [k3_1]]}, nested_maps:put([[k1_2], [], [k3_1]], v3_1_1_new, Map, #{strict => true})),
    ok.

simple_non_strict_put_exceptions(Map) ->
    ok = common_simple_put_exceptions(Map, #{strict => false}),
    ok.

common_simple_put_exceptions(Map, Options) ->
    ?TRY({badmap, Map}, nested_maps:put([[k1_1], [k4_1], [k3_1]], v3_1, Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:put([[k1_2], [k2_1], [k3_2]], v3_1, not_a_map, Options)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:put(not_a_list, v3_1, Map, Options)),
    ?TRY({badarg, {'Address', [[k1_2], not_a_list, [k3_2]]}},
         nested_maps:put([[k1_2], not_a_list, [k3_2]], v3_1, Map, Options)),
    ok.

%%% WILDCARD

wildcard_strict_put_operations(Map) ->
    ok = common_wildcard_put_operations(Map, #{strict => true}),
    ok.

wildcard_non_strict_put_operations(Map) ->
    ok = common_wildcard_put_operations(Map, #{strict => false}),

    ?TRY(Map, nested_maps:put([[k1_3], '*', [k3_1]], v3_1, Map, #{strict => false})),
    ok.

common_wildcard_put_operations(Map, Options) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_new},
                     k2_2 => #{k3_2 => v3_new, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:put([[k1_2], '*', [k3_2]], v3_new, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_new, k3_2 => v3_new},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1], '*'], v3_new, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_new, k3_2 => v3_new},
                     k2_2 => #{k3_2 => v3_new, k3_3 => v3_new}},
           k1_3 => #{}}, nested_maps:put([[k1_2], '*', '*'], v3_new, Map, Options)),
    ok.

wildcard_strict_put_exceptions(Map) ->
    ok = common_wildcard_put_exceptions(Map, #{strict => true}),
    ?TRY({unreachable_address, [[k1_3], '*', [k3_1]]}, nested_maps:put([[k1_3], '*', [k3_1]], v3_1, Map, #{strict => true})),
    ok.

wildcard_non_strict_put_exceptions(Map) ->
    ok = common_wildcard_put_exceptions(Map, #{strict => false}),
    ok.

common_wildcard_put_exceptions(_Map, _Options) ->
    ok.

%%% GROUP

group_strict_put_operations(Map) ->
    ok = common_group_put_operations(Map, #{strict => true}),
    ok.

group_non_strict_put_operations(Map) ->
    ok = common_group_put_operations(Map, #{strict => false}),

    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_new, k3_2 => v3_2_1},
                     k2_2 => #{k3_1 => v3_new, k3_2 => v3_2_2, k3_3 => v3_3_2},
                     k2_3 => #{k3_1 => v3_new}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1, k2_2, k2_3], [k3_1]], v3_new, Map, #{strict => false})),
    ok.

common_group_put_operations(Map, Options) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_new},
                     k2_2 => #{k3_2 => v3_new, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1, k2_2], [k3_2]], v3_new, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_new, k3_2 => v3_new},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1], [k3_1, k3_2]], v3_new, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_new, k3_2 => v3_new, k3_3 => v3_new},
                     k2_2 => #{k3_1 => v3_new, k3_2 => v3_new, k3_3 => v3_new}},
           k1_3 => #{}}, nested_maps:put([[k1_2], [k2_1, k2_2], [k3_1, k3_2, k3_3]], v3_new, Map, Options)),
    ok.

group_strict_put_exceptions(Map) ->
    ?TRY({badkey, k2_3}, nested_maps:put([[k1_2], [k2_1, k2_2, k2_3], [k3_1]], v3_new, Map, #{strict => true})),
    ok.
group_non_strict_put_exceptions(_Map) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PUT_WITH Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% SIMPLE

simple_strict_put_with_operations(Map) ->
    ok = common_simple_put_with_operations(Map, #{strict => true}).

simple_non_strict_put_with_operations(Map) ->
    ok = common_simple_put_with_operations(Map, #{strict => false}),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                     k2_2 => #{k3_2 => 30, k3_3 => 40}},
           k1_3 => #{},
           k1_4 => #{k4_1 => #{k3_1 => 100}}}, nested_maps:put_with([[k1_4], [k4_1], [k3_1]], ?FUN_PUT, Map, #{strict => false})),
    ?TRY(Map, nested_maps:put_with([[k1_4], [], [k3_1]], ?FUN_PUT, Map, #{strict => false})),
    ?TRY(Map, nested_maps:put_with([], ?FUN_PUT, Map, #{strict => false})),
    ok.

common_simple_put_with_operations(Map, Options) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 20},
                     k2_2 => #{k3_2 => 30, k3_3 => 40}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], [k2_1], [k3_1]], ?FUN_PUT, Map, Options)),


    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                     k2_2 => #{k3_2 => 30, k3_3 => 41}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], [k2_2], [k3_3]], ?FUN_PUT, Map, Options)),


    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 20},
                     k2_2 => #{k3_2 => 30, k3_3 => 40, k3_4 => 100}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], [k2_2], [k3_4]], ?FUN_PUT, Map, Options)),
    ok.

simple_strict_put_with_exceptions(Map) ->
    ok = common_simple_put_with_exceptions(Map, #{strict => true}),

    ?TRY({badkey, k1_4}, nested_maps:put_with([[k1_4], [k4_1], [k3_1]], ?FUN_PUT, Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [k3_1]]}, nested_maps:put_with([[k1_2], [], [k3_1]], ?FUN_PUT, Map, #{strict => true})),
    ?TRY({badarg, {'Address', []}}, nested_maps:put_with([], ?FUN_PUT, Map, #{strict => true})),
    ok.

simple_non_strict_put_with_exceptions(Map) ->
    ok = common_simple_put_with_exceptions(Map, #{strict => false}),
    ok.

common_simple_put_with_exceptions(Map, Options) ->
    Bad_fun = fun(_Key, Value, true) when Value > 10 -> 1 / 0;
                 (_Key, _Value, true) -> 1 / 0;
                 (_Key, _Value, false) -> 1 / 0 end,
    ?TRY({badfun, {badarith, '*'}}, nested_maps:put_with([[k1_2], [k2_1], [k3_2]], Bad_fun, Map, Options)),

    ?TRY({badfun, {got_throw, '*'}}, nested_maps:put_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_THROW3, Map, Options)),
    ?TRY({badfun, {got_error, '*'}}, nested_maps:put_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_ERROR3, Map, Options)),
    ?TRY({badfun, {got_exit, '*'}}, nested_maps:put_with([[k1_2], [k2_2], [k3_2]], ?BAD_FUN_EXIT3, Map, Options)),

    ?TRY({badmap, Map}, nested_maps:put_with([[k1_1], [k4_1], [k3_1]], ?FUN_PUT, Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:put_with([[k1_2], [k2_1], [k3_2]], ?FUN_PUT, not_a_map, Options)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:put_with(not_a_list, ?FUN_PUT, Map, Options)),
    ?TRY({badarg, {'Address', [[k1_2], not_a_list, [k3_2]]}},
         nested_maps:put_with([[k1_2], not_a_list, [k3_2]], ?FUN_PUT, Map, Options)),
    ok.

%%% WILDCARD

wildcard_strict_put_with_operations(Map) ->
    ok = common_wildcard_put_with_operations(Map, #{strict => true}),
    ok.

wildcard_non_strict_put_with_operations(Map) ->
    ok = common_wildcard_put_with_operations(Map, #{strict => false}),
    ?TRY(Map, nested_maps:put_with([[k1_3], '*', [k3_1]], ?FUN_PUT, Map, #{strict => false})),
    ok.

common_wildcard_put_with_operations(Map, Options) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 21},
                     k2_2 => #{k3_2 => 31, k3_3 => 40}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], '*', [k3_2]], ?FUN_PUT, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 21},
                     k2_2 => #{k3_2 => 30, k3_3 => 40}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], [k2_1], '*'], ?FUN_PUT, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 21},
                     k2_2 => #{k3_2 => 31, k3_3 => 41}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], '*', '*'], ?FUN_PUT, Map, Options)),
    ok.

wildcard_strict_put_with_exceptions(Map) ->
    ok = common_wildcard_put_with_exceptions(Map, #{strict => true}),
    ?TRY({unreachable_address, [[k1_3], '*', [k3_1]]},
         nested_maps:put_with([[k1_3], '*', [k3_1]], ?FUN_PUT, Map, #{strict => true})),
    ok.

wildcard_non_strict_put_with_exceptions(Map) ->
    ok = common_wildcard_put_with_exceptions(Map, #{strict => false}),
    ok.

common_wildcard_put_with_exceptions(_Map, _Options) ->
    ok.

%%% GROUP

group_strict_put_with_operations(Map) ->
    ok = common_group_put_with_operations(Map, #{strict => true}),
    ok.

group_non_strict_put_with_operations(Map) ->
    ok = common_group_put_with_operations(Map, #{strict => false}),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 20},
                     k2_2 => #{k3_1 => 100, k3_2 => 30, k3_3 => 40},
                     k2_3 => #{k3_1 => 100}},
           k1_3 => #{}},
         nested_maps:put_with([[k1_2], [k2_1, k2_2, k2_3], [k3_1]], ?FUN_PUT, Map, #{strict => false})),
    ok.

common_group_put_with_operations(Map, Options) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 10, k3_2 => 21},
                     k2_2 => #{k3_2 => 31, k3_3 => 40}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], [k2_1, k2_2], [k3_2]], ?FUN_PUT, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 21},
                     k2_2 => #{k3_2 => 30, k3_3 => 40}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], [k2_1], [k3_1, k3_2]], ?FUN_PUT, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => 9, k3_2 => 21, k3_3 => 100},
                     k2_2 => #{k3_1 => 100, k3_2 => 31, k3_3 => 41}},
           k1_3 => #{}}, nested_maps:put_with([[k1_2], [k2_1, k2_2], [k3_1, k3_2, k3_3]], ?FUN_PUT, Map, Options)),
    ok.

group_strict_put_with_exceptions(Map) ->
    ?TRY({badkey, k2_3}, nested_maps:put_with([[k1_2], [k2_1, k2_2, k2_3], [k3_1]], ?FUN_PUT, Map, #{strict => true})),
    ok.
group_non_strict_put_with_exceptions(_Map) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% KEYS Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% SIMPLE

simple_strict_keys_operations(Map) ->
    ok = common_simple_keys_operations(Map, #{strict => true}).

simple_non_strict_keys_operations(Map) ->
    ok = common_simple_keys_operations(Map, #{strict => false}),

    ?TRY([], nested_maps:keys([], Map, #{strict => false})),
    ?TRY([], nested_maps:keys([[k1_1]], Map, #{strict => false})),
    ?TRY([], nested_maps:keys([[]], Map, #{strict => false})),
    ?TRY([], nested_maps:keys([[k1_2], []], Map, #{strict => false})),

    ?TRY([], nested_maps:keys([[k1_3]], Map, #{strict => false})),

    ?TRY([], nested_maps:keys([[ne_key], [k2_1], [k3_2]], Map, #{strict => false})),
    ?TRY([], nested_maps:keys([[k1_2], [ne_key], [k3_2]], Map, #{strict => false})),
    ?TRY([], nested_maps:keys([[k1_2], [k2_1], [ne_key]], Map, #{strict => false})),
    ok.

simple_strict_keys_exceptions(Map) ->
    ok = common_simple_keys_exceptions(Map, #{strict => true}),
    ?TRY({bad_address, [[]]}, nested_maps:keys([[]], Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], []]}, nested_maps:keys([[k1_2], []], Map, #{strict => true})),
    ?TRY({badarg, {'Address', []}}, nested_maps:keys([], Map, #{strict => true})),
    ?TRY({badkey, k1_3}, nested_maps:keys([[k1_3]], Map, #{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:keys([[ne_key], [k2_1], [k3_2]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:keys([[k1_2], [ne_key], [k3_2]], Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:keys([[k1_2], [k2_1], [ne_key]], Map, #{strict => true})),

    ok.

simple_non_strict_keys_exceptions(Map) ->
    ok = common_simple_keys_exceptions(Map, #{strict => false}).

common_simple_keys_operations(Map, Options) ->
    ?TRY([k2_1, k2_2], nested_maps:keys([[k1_2]], Map, Options)),
    ?TRY([k3_1, k3_2], nested_maps:keys([[k1_2], [k2_1]], Map, Options)),
    ?TRY([k3_2, k3_3], nested_maps:keys([[k1_2], [k2_2]], Map, Options)),
    ok.

common_simple_keys_exceptions(Map, Options) ->
    ?TRY({badmap, Map}, nested_maps:keys([[k1_1], [k2_1]], Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:keys([[k1_2], [k2_1], [k3_2]], not_a_map, Options)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:keys(not_a_list, Map, Options)),
    ?TRY({badarg, {'Options', not_strict}}, nested_maps:keys([[k1_2], [k2_1]], Map, not_strict)),
    ok.

%%% WILDCARD

wildcard_strict_keys_operations(Map) ->
    ok = common_wildcard_keys_operations(Map, #{strict => true}).

wildcard_non_strict_keys_operations(Map) ->
    ok = common_wildcard_keys_operations(Map, #{strict => false}),

    ?TRY([k2_1, k2_2], nested_maps:keys(['*'], Map, #{strict => false})),
    ?TRY([], nested_maps:keys([[k1_3], '*'], Map, #{strict => false})),
    ?TRY([], nested_maps:keys(['*'], #{}, #{strict => false})),
    ok.

wildcard_strict_keys_exceptions(Map) ->
    ok = common_wildcard_keys_exceptions(Map, #{strict => true}),

    ?TRY({unreachable_address, ['*']}, nested_maps:keys(['*'], #{}, #{strict => true})),
    ?TRY({badmap, v1_1}, nested_maps:keys(['*'], Map, #{strict => true})),
    ok.

wildcard_non_strict_keys_exceptions(Map) ->
    ok = common_wildcard_keys_exceptions(Map, #{strict => false}).

common_wildcard_keys_operations(Map, Options) ->
    ?TRY([k3_1, k3_2, k3_2, k3_3], nested_maps:keys([[k1_2], '*'], Map, Options)),
    ok.

common_wildcard_keys_exceptions(_Map, _Options) ->
    ok.

%%% GROUP

group_strict_keys_operations(Map) ->
    ok = common_group_keys_operations(Map, #{strict => true}).
group_non_strict_keys_operations(Map) ->
    ok = common_group_keys_operations(Map, #{strict => false}),
    ?TRY([k3_1, k3_2], nested_maps:keys([[k1_2], [k2_1, ne_key]], Map, #{strict => false})),
    ok.

group_strict_keys_exceptions(Map) ->
    ok = common_group_keys_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:keys([[k1_2], [k2_1, ne_key]], Map, #{strict => true})),
    ok.

group_non_strict_keys_exceptions(Map) ->
    ok = common_group_keys_exceptions(Map, #{strict => false}).

common_group_keys_operations(Map, Options) ->
    ?TRY([k3_1, k3_2, k3_2, k3_3], nested_maps:keys([[k1_2], [k2_1, k2_2]], Map, Options)),
    ok.

common_group_keys_exceptions(_Map, _Options) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% KEYS_WITH Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% SIMPLE

simple_strict_keys_with_operations(Map) ->
    ok = common_simple_keys_with_operations(Map, #{strict => true}).

simple_non_strict_keys_with_operations(Map) ->
    ok = common_simple_keys_with_operations(Map, #{strict => false}),

    ?TRY([], nested_maps:keys_with([[k1_1]], ?FUN_KEYS, Map, #{strict => false})),
    ?TRY([], nested_maps:keys_with([[]], ?FUN_KEYS, Map, #{strict => false})),
    ?TRY([], nested_maps:keys_with([[k1_2], []], ?FUN_KEYS, Map, #{strict => false})),

    ?TRY([], nested_maps:keys_with([], ?FUN_KEYS, Map, #{strict => false})),
    ?TRY([], nested_maps:keys_with([[k1_3]], ?FUN_KEYS, Map, #{strict => false})),

    ?TRY([], nested_maps:keys_with([[ne_key], [k2_1], [k3_2]], ?FUN_KEYS, Map, #{strict => false})),
    ?TRY([], nested_maps:keys_with([[k1_2], [ne_key], [k3_2]], ?FUN_KEYS, Map, #{strict => false})),
    ?TRY([], nested_maps:keys_with([[k1_2], [k2_1], [ne_key]], ?FUN_KEYS, Map, #{strict => false})),
    ok.

simple_strict_keys_with_exceptions(Map) ->
    ok = common_simple_keys_with_exceptions(Map, #{strict => true}),

    ?TRY({bad_address, [[]]}, nested_maps:keys_with([[]], ?FUN_KEYS, Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], []]}, nested_maps:keys_with([[k1_2], []], ?FUN_KEYS, Map, #{strict => true})),
    ?TRY({badmap, v1_1}, nested_maps:keys_with([[k1_1]], ?FUN_KEYS, Map, #{strict => true})),
    ?TRY({badarg, {'Address', []}}, nested_maps:keys_with([], ?FUN_KEYS, Map, #{strict => true})),

    ?TRY({badkey, k1_3}, nested_maps:keys_with([[k1_3]], ?FUN_KEYS, Map, #{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:keys_with([[ne_key], [k2_1], [k3_2]], ?FUN_KEYS, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:keys_with([[k1_2], [ne_key], [k3_2]], ?FUN_KEYS, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:keys_with([[k1_2], [k2_1], [ne_key]], ?FUN_KEYS, Map, #{strict => true})),


    ?TRY({badarg, {'Address', []}}, nested_maps:keys_with([], ?FUN_KEYS, Map, #{strict => true})),
    ok.

simple_non_strict_keys_with_exceptions(Map) ->
    ok = common_simple_keys_with_exceptions(Map, #{strict => false}).

common_simple_keys_with_operations(Map, Options) ->
    ?TRY([], nested_maps:keys_with([[k1_2]], ?FUN_KEYS, Map, Options)),
    ?TRY([k3_2], nested_maps:keys_with([[k1_2], [k2_1]], ?FUN_KEYS, Map, Options)),
    ?TRY([k3_2, k3_3], nested_maps:keys_with([[k1_2], [k2_2]], ?FUN_KEYS, Map, Options)),
    ok.

common_simple_keys_with_exceptions(Map, Options) ->
    ?TRY({badmap, Map}, nested_maps:keys_with([[k1_1], [k2_1]], ?FUN_KEYS, Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:keys_with([[k1_2], [k2_1], [k3_2]], ?FUN_KEYS, not_a_map, Options)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:keys_with(not_a_list, ?FUN_KEYS, Map, Options)),
    ?TRY({badarg, {'Options', not_strict}}, nested_maps:keys_with([[k1_2], [k2_1]], ?FUN_KEYS, Map, not_strict)),

    Bad_fun1 = fun(_K, V) when V > 20 -> not_true; (_, _) -> not_false end,
    ?TRY({badarg, {'Function', Bad_fun1}}, nested_maps:keys_with([[k1_2], [k2_2]], Bad_fun1, Map, Options)),

    Bad_fun2 = fun(_K, V) when V > 20 -> 1 / 0; (_, _) -> false end,
    ?TRY({badfun, {badarith, '*'}}, nested_maps:keys_with([[k1_2], [k2_2]], Bad_fun2, Map, Options)),

    ?TRY({badfun, {got_throw, '*'}}, nested_maps:keys_with([[k1_2], [k2_2]], ?BAD_FUN_THROW, Map, Options)),
    ?TRY({badfun, {got_error, '*'}}, nested_maps:keys_with([[k1_2], [k2_2]], ?BAD_FUN_ERROR, Map, Options)),
    ?TRY({badfun, {got_exit, '*'}}, nested_maps:keys_with([[k1_2], [k2_2]], ?BAD_FUN_EXIT, Map, Options)),

    Bad_fun3 = fun(V) when is_integer(V), V > 10 -> true; (_) -> false end,
    ?TRY({badarg, {'Function', Bad_fun3}}, nested_maps:keys_with([[k1_2], [k2_2]], Bad_fun3, Map, Options)),

    ok.

%%% WILDCARD

wildcard_strict_keys_with_operations(Map) ->
    ok = common_wildcard_keys_with_operations(Map, #{strict => true}).

wildcard_non_strict_keys_with_operations(Map) ->
    ok = common_wildcard_keys_with_operations(Map, #{strict => false}),

    ?TRY([], nested_maps:keys_with(['*'], ?FUN_KEYS, Map, #{strict => false})),
    ?TRY([], nested_maps:keys_with([[k1_3], '*'], ?FUN_KEYS, Map, #{strict => false})),
    ?TRY([], nested_maps:keys_with(['*'], ?FUN_KEYS, #{}, #{strict => false})),
    ok.

wildcard_strict_keys_with_exceptions(Map) ->
    ok = common_wildcard_keys_with_exceptions(Map, #{strict => true}),

    ?TRY({unreachable_address, ['*']}, nested_maps:keys_with(['*'], ?FUN_KEYS, #{}, #{strict => true})),
    ?TRY({badmap, v1_1}, nested_maps:keys_with(['*'], ?FUN_KEYS, Map, #{strict => true})),
    ok.

wildcard_non_strict_keys_with_exceptions(Map) ->
    ok = common_wildcard_keys_with_exceptions(Map, #{strict => false}).

common_wildcard_keys_with_operations(Map, Options) ->
    ?TRY([k3_3, k3_2, k3_2], nested_maps:keys_with([[k1_2], '*'], ?FUN_KEYS, Map, Options)),
    ok.

common_wildcard_keys_with_exceptions(_Map, _Options) ->
    ok.

%%% GROUP

group_strict_keys_with_operations(Map) ->
    ok = common_group_keys_with_operations(Map, #{strict => true}).
group_non_strict_keys_with_operations(Map) ->
    ok = common_group_keys_with_operations(Map, #{strict => false}),
    ?TRY([k3_2], nested_maps:keys_with([[k1_2], [k2_1, ne_key]], ?FUN_KEYS, Map, #{strict => false})),
    ok.

group_strict_keys_with_exceptions(Map) ->
    ok = common_group_keys_with_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:keys_with([[k1_2], [k2_1, ne_key]], ?FUN_KEYS, Map, #{strict => true})),
    ok.

group_non_strict_keys_with_exceptions(Map) ->
    ok = common_group_keys_with_exceptions(Map, #{strict => false}).

common_group_keys_with_operations(Map, Options) ->
    ?TRY([k3_3, k3_2, k3_2], nested_maps:keys_with([[k1_2], [k2_1, k2_2]], ?FUN_KEYS, Map, Options)),
    ok.

common_group_keys_with_exceptions(_Map, _Options) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% UPDATE Case Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Simple

simple_strict_update_operations(Map) ->
    ok = common_simple_update_operations(Map, #{strict => true}).

simple_non_strict_update_operations(Map) ->
    ok = common_simple_update_operations(Map, #{strict => false}),

    ?TRY(Map, nested_maps:update([[new_key], [k2_1], [k3_1]], new_value, Map, #{strict => false})),
    ?TRY(Map, nested_maps:update([[new_key], [k2_1], [new_key]], new_value, Map, #{strict => false})),
    ?TRY(Map, nested_maps:update([[new_key], [new_key], [new_key]], new_value, Map, #{strict => false})),
    ?TRY(Map, nested_maps:update([[k1_2], [new_key], [new_key]], new_value, Map, #{strict => false})),
    ?TRY(Map, nested_maps:update([[k1_2], [k2_1], [new_key]], new_value, Map, #{strict => false})),
    ?TRY(Map, nested_maps:update([[k1_2], [new_key]], new_value, Map, #{strict => false})),
    ?TRY(Map, nested_maps:update([[new_key]], new_value, Map, #{strict => false})),

    ?TRY(Map, nested_maps:update([[]], new_value, Map, #{strict => false})),
    ?TRY(Map, nested_maps:update([[k1_2], []], new_value, Map, #{strict => false})),
    ?TRY(Map, nested_maps:update([[k1_2], [], [k3_1]], new_value, Map, #{strict => false})),

    ?TRY(Map, nested_maps:update([], new_value, Map, #{strict => false})),
    ok.

common_simple_update_operations(Map, Options) ->
    ?TRY(#{k1_1 => new_value,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_1]], new_value, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => new_value, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_1], [k3_1]], new_value, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => new_value},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_1], [k3_2]], new_value, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => new_value, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_2], [k3_2]], new_value, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => new_value}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_2], [k3_3]], new_value, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{k2_3 => new_value}}, nested_maps:update([[k1_3]], #{k2_3 => new_value}, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => new_value,
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_1]], new_value, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => new_value},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_2]], new_value, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{},
           k1_3 => #{}}, nested_maps:update([[k1_2]], #{}, Map, Options)),
    ok.
simple_strict_update_exceptions(Map) ->
    % Input parameters
    ok = common_simple_update_exceptions(Map, #{strict => true}),

    % Undefined keys
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key], [k2_1], [k3_2]], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key], [k2_1], [ne_key]], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key], [ne_key], [ne_key]], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [ne_key], [ne_key]], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [k2_1], [ne_key]], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [ne_key]], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key]], new_value, Map, #{strict => true})),

% Missing keys
    ?TRY({badarg, {'Address', []}}, nested_maps:update([], new_value, Map, #{strict => true})),
    ?TRY({bad_address, [[]]}, nested_maps:update([[]], new_value, Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], []]}, nested_maps:update([[k1_2], []], new_value, Map, #{strict => true})),
    ?TRY({bad_address, [[k1_2], [], [k3_2]]}, nested_maps:update([[k1_2], [], [k3_2]], new_value, Map, #{strict => true})),
    ok.

simple_non_strict_update_exceptions(Map) ->
    ok = common_simple_update_exceptions(Map, #{strict => false}),
    ok.

common_simple_update_exceptions(Map, Options) ->
    ?TRY({badmap, Map}, nested_maps:update([[k1_1], [k2_1]], new_value, Map, Options)),
    ?TRY({badarg, {'Map', not_a_map}}, nested_maps:update([[k1_2], [k2_1], [k3_2]], new_value, not_a_map, Options)),
    ?TRY({badarg, {'Address', not_a_list}}, nested_maps:update(not_a_list, new_value, Map, Options)),
    ?TRY({badarg, {'Options', not_strict}}, nested_maps:update([[k1_2], [k2_1]], new_value, Map, not_strict)),
    ok.

%%% Wildcard

wildcard_strict_update_operations(Map) ->
    ok = common_wildcard_update_operations(Map, #{strict => true}).

wildcard_non_strict_update_operations(Map) ->
    ok = common_wildcard_update_operations(Map, #{strict => false}),

    ?TRY(Map, nested_maps:update([[k1_2], [ne_key], '*'], new_value, Map, #{strict => false})),
    ?TRY(Map, nested_maps:update([[k1_2], '*', [ne_key]], new_value, Map, #{strict => false})),
    ?TRY(Map, nested_maps:update([[ne_key], [ne_key], '*'], new_value, Map, #{strict => false})),
    ?TRY(Map, nested_maps:update([[ne_key], [k2_1], '*'], new_value, Map, #{strict => false})),


    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => new_value, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], '*', [k3_1]], new_value, Map, #{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => new_value}},
           k1_3 => #{}}, nested_maps:update([[k1_2], '*', [k3_3]], new_value, Map, #{strict => false})),

    ?TRY(Map, nested_maps:update([[k1_3], '*', [k3_2]], new_value, Map, #{strict => false})),
    ok.

common_wildcard_update_operations(Map, Options) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => new_value, k3_3 => new_value}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_2], '*'], new_value, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => new_value, k3_2 => new_value},
                     k2_2 => #{k3_2 => new_value, k3_3 => new_value}},
           k1_3 => #{}}, nested_maps:update([[k1_2], '*', '*'], new_value, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => new_value},
                     k2_2 => #{k3_2 => new_value, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], '*', [k3_2]], new_value, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => new_value,
                     k2_2 => new_value},
           k1_3 => #{}}, nested_maps:update([[k1_2], '*'], new_value, Map, Options)),
    ?TRY(#{k1_1 => new_value,
           k1_2 => new_value,
           k1_3 => new_value}, nested_maps:update(['*'], new_value, Map, Options)),
    ok.
wildcard_strict_update_exceptions(Map) ->
    ok = common_wildcard_update_exceptions(Map, #{strict => true}),
    ?TRY({badkey, k3_1}, nested_maps:update([[k1_2], '*', [k3_1]], new_value, Map, #{strict => true})),
    ?TRY({badkey, k3_3}, nested_maps:update([[k1_2], '*', [k3_3]], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [ne_key], '*'], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], '*', [ne_key]], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key], [ne_key], '*'], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key], [k2_1], '*'], new_value, Map, #{strict => true})),
    ?TRY({unreachable_address, [[k1_3], '*', [k3_2]]},
         nested_maps:update([[k1_3], '*', [k3_2]], new_value, Map, #{strict => true})),
    ok.

wildcard_non_strict_update_exceptions(Map) ->
    ok = common_wildcard_update_exceptions(Map, #{strict => false}),
    ok.

common_wildcard_update_exceptions(Map, Options) ->
    ?TRY({badmap, Map}, nested_maps:update([[k1_1], '*'], new_value, Map, Options)),
    ?TRY({badmap, Map}, nested_maps:update(['*', '*', '*'], new_value, Map, Options)),
    ok.

%%% Group

group_strict_update_operations(Map) ->
    ok = common_group_update_operations(Map, #{strict => true}).

group_non_strict_update_operations(Map) ->
    ok = common_group_update_operations(Map, #{strict => false}),

    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => new_value, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_1], [k3_1, ne_key]], new_value, Map, #{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => new_value, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_2], [ne_key, k3_2]], new_value, Map, #{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => new_value}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_2, ne_key], [k3_3]], new_value, Map, #{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => new_value},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2, ne_key], [k2_1], [k3_2]], new_value, Map, #{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => new_value, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_1, ne_key], [k3_1, ne_key]], new_value, Map, #{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => new_value}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [ne_key, k2_2], [ne_key, k3_3]], new_value, Map, #{strict => false})),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => new_value, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2, ne_key], [k2_1, ne_key], [k3_1, ne_key]], new_value, Map, #{strict => false})),
    ?TRY(Map, nested_maps:update([[ne_key, k1_3], [ne_key, k2_2], [ne_key, k3_3]], new_value, Map, #{strict => false})),
    ok.

common_group_update_operations(Map, Options) ->
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => new_value, k3_2 => new_value},
                     k2_2 => #{k3_2 => v3_2_2, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_1], [k3_1, k3_2]], new_value, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => v3_2_1},
                     k2_2 => #{k3_2 => new_value, k3_3 => new_value}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_2], [k3_2, k3_3]], new_value, Map, Options)),
    ?TRY(#{k1_1 => v1_1,
           k1_2 => #{k2_1 => #{k3_1 => v3_1_1, k3_2 => new_value},
                     k2_2 => #{k3_2 => new_value, k3_3 => v3_3_2}},
           k1_3 => #{}}, nested_maps:update([[k1_2], [k2_1, k2_2], [k3_2]], new_value, Map, Options)),
    ok.

group_strict_update_exceptions(Map) ->
    ok = common_group_update_exceptions(Map, #{strict => true}),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [k2_1], [k3_1, ne_key]], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [k2_1, ne_key], [k3_1]], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [ne_key, k2_2], [k3_2]], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2, ne_key], [k2_1], [k3_1]], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key, k1_3], [k2_2], [k3_2]], new_value, Map, #{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [k2_1, ne_key], [k3_1, ne_key]], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2], [ne_key, k2_2], [ne_key, k3_2]], new_value, Map, #{strict => true})),

    ?TRY({badkey, ne_key}, nested_maps:update([[k1_2, ne_key], [k2_1, ne_key], [k3_1, ne_key]], new_value, Map, #{strict => true})),
    ?TRY({badkey, ne_key}, nested_maps:update([[ne_key, k1_3], [ne_key, k2_2], [ne_key, k3_2]], new_value, Map, #{strict => true})),
    ok.

group_non_strict_update_exceptions(Map) ->
    ok = common_group_update_exceptions(Map, #{strict => false}),
    ok.

common_group_update_exceptions(Map, Options) ->
    ?TRY({badmap, Map}, nested_maps:update([[k1_1], [k2_1, k2_2], [k3_1]], new_value, Map, Options)),
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

    ?TRY({badmap, Map}, nested_maps:update_with([[k1_1], [k4_1], [k3_1]], ?FUN_UPDATE, Map, Options)),
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
    ?TRY({badmap, Map}, nested_maps:update_with([[k1_1], '*', '*'], ?FUN_UPDATE, Map, Options)),
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
    ?TRY({badmap, Map}, nested_maps:update_with([[k1_1], [k2_1, k2_2], [k3_1]], ?FUN_UPDATE, Map, Options)),
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
    ?TRY({badmap, Map}, nested_maps:remove([[k1_1], [k2_1]], Map, Options)),
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
    ?TRY({badmap, Map}, nested_maps:remove([[k1_1], '*'], Map, Options)),
    ?TRY({badmap, Map}, nested_maps:remove(['*', '*', '*'], Map, Options)),
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
    ?TRY({badmap, Map}, nested_maps:remove([[k1_1], [k2_1, k2_2], [k3_1]], Map, Options)),
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

    ?TRY({badmap, Map}, nested_maps:remove_with([[k1_1], [k2_1], [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
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
    ?TRY({badmap, Map}, nested_maps:remove_with([[k1_1], '*', '*'], ?FUN_GET_REMOVE_TAKE, Map, Options)),
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
    ?TRY({badmap, Map}, nested_maps:remove_with([[k1_1], [k2_1, k2_2], [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
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
    ?TRY({badmap, Map}, nested_maps:take([[k1_1], [k2_1]], Map, Options)),
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
    ?TRY({badmap, Map}, nested_maps:take([[k1_1], '*'], Map, Options)),
    ?TRY({badmap, Map}, nested_maps:take(['*', '*', '*'], Map, Options)),
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
    ?TRY({badmap, Map}, nested_maps:take([[k1_1], [k2_1, k2_2], [k3_1]], Map, Options)),
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

    ?TRY({badmap, Map}, nested_maps:take_with([[k1_1], [k2_1], [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
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
    ?TRY({badmap, Map}, nested_maps:take_with([[k1_1], '*', '*'], ?FUN_GET_REMOVE_TAKE, Map, Options)),
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
    ?TRY({badmap, Map}, nested_maps:take_with([[k1_1], [k2_1, k2_2], [k3_1]], ?FUN_GET_REMOVE_TAKE, Map, Options)),
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

eq({List1, Map1}, {List2, Map2}) when is_list(List1), is_list(List2), is_map(Map1), is_map(Map2) ->
    case {eq(List1, List2), eq(Map1, Map2)} of
        {true, true} ->
            true;
        _ ->
            ct:fail("~nExpected: ~p~nGot: ~p~n", [List1, List2])
    end;

eq(List1, List2) when is_list(List1), is_list(List2) ->
    case lists:sort(List1) == lists:sort(List2) of
        true ->
            true;
        _ ->
            ct:fail("~nExpected: ~p~nGot: ~p~n", [List1, List2])
    end;

eq(Map1, Map2) when is_map(Map1), is_map(Map2) ->
    case Map1 =:= Map2 of
        true ->
            true;
        _ ->
            ct:fail("~nExpected: ~p~nGot: ~p~n", [Map1, Map2])
    end;

%% Since it is not possible to pass guard expression with anonymous variable as
%% parameter the wildcard '*' is used as mark where the anonymous variable
%% should be placed.

eq({Term1, {Term2, '*'}} = E1, E2) ->
    case E2 of
        {Term1, {Term2, _}} ->
            true;
        _ ->
            ct:fail("~nExpected: ~p~nGot: ~p~n", [E1, E2])
    end;

eq({Term1, '*'} = E1, E2) ->
    case E2 of
        {Term1, _} ->
            true;
        _ ->
            ct:fail("~nExpected: ~p~nGot: ~p~n", [E1, E2])
    end;

eq(E1, E2) ->
    case E2 of
        E1 ->
            true;
        _ ->
            ct:fail("~nExpected: ~p~nGot: ~p~n", [E1, E2])
    end.