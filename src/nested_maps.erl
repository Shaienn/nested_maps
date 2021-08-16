%%%----------------------------------------------------------------------
%%% File    : nested_maps.erl
%%% Purpose : Maps manipulation
%%%
%%% Authors:
%%%	    Shevchenko Vitaly <shaienn@mail.ru>
%%%     Kholodov Sergey
%%%     Kolobyanina Svetlana	
%%%----------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% @doc LTS map library.
%%%
%%% The library is aimed to ease access to complex data structures.
%%%
%%% Suppose there is a complex record `Rec :: #rec1{}' containing nested records:
%%%
%%% ```
%%% -record(rec3, {
%%%     f3_1 :: any()
%%%     f3_2 :: any()
%%% }).
%%%
%%% -record(rec2, {
%%%     f2_1 :: #rec3{}
%%% }).
%%%
%%% -record(rec1, {
%%%     f1_1 :: #rec2{},
%%%     f1_2 :: list(#rec3{}),
%%% }).
%%%
%%% Rec = #rec1{
%%%     f1_1 = #rec2{
%%%         f2_1 = #rec3{
%%%             f3_1 = v3_1_1,
%%%             f3_2 = v3_2_1
%%%         }
%%%     },
%%%
%%%     f1_2 = [
%%%         #rec3{
%%%             f3_1 = v3_1_2,
%%%             f3_2 = v3_2_2
%%%         },
%%%
%%%         #rec3{
%%%             f3_1 = v3_1_3,
%%%             f3_2 = v3_2_3
%%%         }
%%%     ]
%%% }.
%%% '''
%%%
%%% If one needs to get the lowest level (level 3) values (`v3_'*) they will need to
%%% use different approaches and there will not be a simple common solution for this.
%%%
%%% But such a complex record can be represented as a multilevel map (a map consisting of
%%% nested maps). And such map can be parsed using a common approach:
%%%
%%% ```
%%% Map = #{
%%%     f1_1 => #{
%%%         f2_1 => #{
%%%             f3_1 => v3_1_1,
%%%             f3_2 => v3_2_1
%%%         }
%%%     },
%%%
%%%     f1_2 => #{
%%%         1 => #{
%%%             f3_1 => v3_1_2,
%%%             f3_2 => v3_2_2
%%%         },
%%%
%%%         2 => #{
%%%             f3_1 => v3_1_3,
%%%             f3_2 => v3_2_3
%%%         }
%%%     }
%%% }.
%%% '''
%%%
%%% Any value here can be retrieved by recursively processing nested maps.
%%%
%%% To do so a special multilevel map address should be specified:
%%%
%%% ```
%%% Address =
%%%     [
%%%         Level1Keys =
%%%             [Level1Key1, ..., Level1KeyN],
%%%         ...,
%%%         LevelNKeys =
%%%             [LevelNKey1, ..., LevelNKeyN]
%%%     ]
%%% '''
%%%
%%% Each element of `Address' represents a set of keys of a specific level in a multilevel
%%% map. The element position represents the depth level. The `Address' thus defines a branched
%%% system of paths to the end keys.
%%%
%%% For convenience instead of list of keys of a particular level a special wildcard atom `` '*' '' can be specified,
%%% which means all the keys which can be found on this key level.
%%%
%%% E.g. address `[[f1_2], [1, 2], [f3_2]]' represents the following key paths:
%%% ```
%%% f1_2 -> 1 -> f3_2
%%% f1_2 -> 2 -> f3_2
%%% '''
%%%
%%% And a `get' operation with this address on `Map' will result in list `[v3_2_2, v3_2_3]'.
%%%
%%% The same result can be achieved by using a wildcard in Address: ``[[f1_2], '*', [f3_2]]''.
%%%
%%% There may be a usual (non-strict) address ({@link address()}) and a strict address ({@link strict_address()}):
%%%
%%% - In a strict address all level keys must be specified (there must not be an empty key list),
%%%   and all specified non-end keys must be present in a map (and for some operations all keys
%%%   must be present in the map).
%%%
%%% - In a non-strict address some level keys may be unspecified (empty key list) which effectively means
%%%   that nothing should be processed; and some or all the specified keys of a particular level
%%%   may be absent in a map.
%%% @end
%%%-----------------------------------------------------------------------------

-module(nested_maps).
-compile({inline, [handle_procedure_wrapper/3, get_operation_type/1]}).

%%%API
-export([
    get/3,
    get_with/4,
    put/4,
    put_with/4,
    update/4,
    update_with/4,
    remove/3,
    remove_with/4,
    keys/3,
    keys_with/4,
    take/3,
    take_with/4
]).

-type address() :: list(list(LevelNKeyN :: any()) | '*').

-type strict_address() :: nonempty_list(nonempty_list(LevelNKeyN :: any()) | '*').

-type filter_fun() :: fun((EndKey :: any(), EndValue :: any()) -> boolean()).

-type modify_fun2() :: fun((EndKey :: any(), EndValue :: any()) -> any()).

-type modify_fun3() :: fun((EndKey :: any(), EndValue :: any(), IsValueExists :: boolean()) -> any()).

-record(params, {
    function = undefined :: filter_fun() | modify_fun2() | modify_fun3() | undefined,
    value = undefined :: any(),
    op = undefined :: get | get_with | put | put_with | update | update_with | remove | remove_with | keys | keys_with | take | take_with,
    op_type = undefined :: collect | modify | multitask,
    strict = undefined :: boolean(),
    address = undefined :: strict_address() | address(),
    parallel = false :: boolean()
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%-----------------------------------------------------------------------------
%%% @doc
%%% ``get(Address, Map, Strict) -> Values''
%%%
%%% Types:
%%% ```
%%%  Address = strict_address() | address()
%%%  Map = map()
%%%  Strict = boolean()
%%%  Values = list(Value)
%%%  Value = any()
%%% '''
%%%
%%% == When Strict =:= 'true': ==
%%%
%%%   `Address' must be of type {@link strict_address()}.
%%%
%%%   Returns `Values' associated with end keys from `Address' if `Map' contains all keys specified in `Address'.
%%%
%%%   The call fails with a `{badkey, Key}' exception if any key specified in `Address' is absent in `Map'.
%%%
%%%   The call fails with a ``{badarg, {'Address', Address}}'' exception if `Address' is not {@link strict_address()}.
%%%
%%%   The call fails with an ``{unreachable_address, Address}'' exception if a wildcard key list (`` '*' '')
%%%   is specified in `Address', and it expands to an empty list in `Map' (i.e. there is an empty map).
%%%
%%% == When Options =:= 'false': ==
%%%
%%%   `Address' must be of type {@link address()}.
%%%
%%%   Returns `Values' associated with end keys from `Address' which could be found in `Map'.
%%%
%%%   If no end keys could be found in `Map' or if an empty key list (`[]') is specified in `Address',
%%%   the function returns an empty list (`[]').
%%%
%%%   The call fails with a ``{badarg, {'Address', Address}}'' exception if `Address' is not {@link address()}.
%%%
%%%  == In any case the call fails with: ==
%%%
%%%  ``{badarg, {'Address', Address}}'' exception if any element of `Address' is not `list()' or wildcard (`` '*' '').
%%%
%%%  ``{badarg, {'Map', Map}}'' exception if `Map' is not `map()'.
%%%
%%%  ``{badarg, {'Strict', Strict}}'' exception if `Strict' is not `boolean()'.
%%%
%%%  ``{badmap, Map}'' exception if a non-end key from `Address' is not associated with a map in `Map'.
%%% @end
%%%-----------------------------------------------------------------------------
-spec get(Address :: strict_address(),
          Map :: map(),
          #{strict => false}) -> nonempty_list(Value :: any());
         (Address :: address(),
          Map :: map(),
          #{strict => true}) -> list(Value :: any()).

get([_H | _T] = Address, Map, Options)
    when is_list(Address), is_map(Map), is_map(Options) ->
    handle_procedure_wrapper(Address, Map, #params{
        address = Address,
        op      = ?FUNCTION_NAME,
        op_type = get_operation_type(?FUNCTION_NAME),
        strict  = maps:get(strict, Options, false)
        %parallel = maps:get(parallel, Options, false)
    });

get(Address, Map, Options) ->
    bad_argument_handler(?FUNCTION_NAME, Address, Map, Options).

%%%-----------------------------------------------------------------------------
%%% @doc
%%% ``get_with(Address, Function, Map, Strict) -> Values''
%%%
%%% Types:
%%% ```
%%% Address = strict_address() | address()
%%% Map = map()
%%% Function = fun((EndKey, EndValue) -> ShouldGet)
%%% EndKey = EndValue = any()
%%% ShouldGet = Strict = boolean()
%%% Values = list(Value)
%%% Value = any()
%%% '''
%%%
%%% Same as {@link get/3} but returns only such end values for which ``Function(EndKey, EndValue) =:= 'true' ''.
%%%
%%% The call fails with a ``{badarg, {'Function', Function}}'' exception if `Function' is not `fun()' of arity 2 or
%%% if a call to `Function' returns not `boolean()'.
%%%
%%% The call fails with a ``{badfun, {Reason, Stack}}'' exception if a call to `Function' fails with `Reason',
%%% where `Stack' is a stack back-trace.
%%% @end
%%%-----------------------------------------------------------------------------
-spec get_with(Address :: strict_address(),
               Function :: filter_fun(),
               Map :: map(),
               #{strict => true}) -> list(Value :: any());
              (Address :: address(),
               Function :: filter_fun(),
               Map :: map(),
               #{strict => false}) -> list(Value :: any()).
get_with([_H | _T] = Address, Function, Map, Options)
    when is_list(Address), is_function(Function, 2), is_map(Map), is_map(Options) ->
    handle_procedure_wrapper(Address, Map, #params{
        address  = Address,
        function = Function,
        op       = ?FUNCTION_NAME,
        op_type  = get_operation_type(?FUNCTION_NAME),
        strict   = maps:get(strict, Options, false)
        %parallel = maps:get(parallel, Options, false)
    });

get_with(Address, Function, Map, Options) ->
    bad_argument_handler(?FUNCTION_NAME, Address, Function, 2, Map, Options).


%%%-----------------------------------------------------------------------------
%%% @doc
%%% ``keys(Address, Map, Strict) -> KeyList''
%%%
%%% Types:
%%% ```
%%% Address = strict_address() | address()
%%% Map = map()
%%% Strict = boolean()
%%% KeyList = list(Key)
%%% Key = any()
%%% '''
%%%
%%% For each end key from `Address' retrieves the associated map in `Map' and gets its keys, and
%%% returns the total list of keys.
%%%
%%% == When Strict =:= 'true': ==
%%%
%%%   `Address' must be of type {@link strict_address()}.
%%%
%%%   The call fails with a ``{badkey, Key}'' exception if any key specified in `Address' is absent in `Map'.
%%%
%%%   The call fails with a ``{badarg, {'Address', Address}}'' exception if `Address' is not {@link strict_address()}.
%%%
%%%   The call fails with an ``{unreachable_address, Address}'' exception if a wildcard key list (`` '*' '')
%%%   is specified in `Address', and it expands to an empty list in `Map' (i.e. there is an empty map).
%%%
%%% == When Strict =:= 'false': ==
%%%
%%%   `Address' must be of type {@link address()}.
%%%
%%%   Any keys from `Address' not found in `Map' are ignored and not processed further.
%%%
%%%   If an empty key list (`[]') is specified in `Address' the function returns an empty list (`[]').
%%%
%%%   The call fails with a ``{badarg, {'Address', Address}}'' exception if `Address' is not {@link address()}.
%%%
%%% == In any case the call fails with: ==
%%%
%%%  ``{badarg, {'Address', Address}}'' exception if any element of `Address' is not `list()' or wildcard (`` '*' '').
%%%
%%%  ``{badarg, {'Map', Map}}'' exception if `Map' is not `map()'.
%%%
%%%  ``{badarg, {'Strict', Strict}}'' exception if `Strict' is not `boolean()'.
%%%
%%%  ``{badmap, Map}'' exception if a non-end key from `Address' is not associated with a map in `Map'.
%%% @end
%%%-----------------------------------------------------------------------------
-spec keys(Address :: strict_address(),
           Map :: map(),
           #{strict => true}) -> nonempty_list(Value :: any());
          (Address :: address(),
           Map :: map(),
           #{strict => true}) -> list(Value :: any()).
keys([_H | _T] = Address, Map, Options)
    when is_list(Address), is_map(Map), is_map(Options) ->
    handle_procedure_wrapper(Address, Map, #params{
        address = Address,
        op      = ?FUNCTION_NAME,
        op_type = get_operation_type(?FUNCTION_NAME),
        strict   = maps:get(strict, Options, false)
        %parallel = maps:get(parallel, Options, false)
    });

keys(Address, Map, Options) ->
    bad_argument_handler(?FUNCTION_NAME, Address, Map, Options).

%%%-----------------------------------------------------------------------------
%%% @doc
%%% ``keys_with(Address, Function, Map, Strict) -> KeyList''
%%%
%%% Types:
%%% ```
%%% Address = strict_address() | address()
%%% Function = fun((EndMapKey, EndMapValue) -> ShouldGetKey)
%%% EndMapKey = EndMapValue = any()
%%% ShouldGetKey = Strict = boolean()
%%% Map = map()
%%% KeyList = list(Key)
%%% Key = any()
%%% '''
%%%
%%% Same as {@link keys/3} but:
%%%
%%% For each end key from `Address' retrieves the associated map in `Map' and gets its keys (`EndMapKey')
%%% and values (`EndMapValue'), and returns the total list of keys `KeyList' for which
%%% ``Function(EndMapKey, EndMapValue) =:= 'true' ''.
%%%
%%% The call fails with a ``{badarg, {'Function', Function}}'' exception if `Function' is not `fun()' of arity 2 or
%%% if a call to `Function' returns not `boolean()'.
%%%
%%% The call fails with a ``{badfun, {Reason, Stack}}'' exception if a call to `Function' fails with `Reason',
%%% where `Stack' is a stack back-trace.
%%% @end
%%%-----------------------------------------------------------------------------
-spec keys_with(Address :: strict_address(),
                Function :: filter_fun(),
                Map1 :: map(),
                #{strict => true}) -> list(Value :: any());
               (Address :: address(),
                Function :: filter_fun(),
                Map1 :: map(),
                #{strict => false}) -> list(Value :: any()).
keys_with([_H | _T] = Address, Function, Map, Options)
    when is_list(Address), is_function(Function, 2), is_map(Map), is_map(Options) ->
    handle_procedure_wrapper(Address, Map, #params{
        address  = Address,
        function = Function,
        op       = ?FUNCTION_NAME,
        op_type  = get_operation_type(?FUNCTION_NAME),
      strict   = maps:get(strict, Options, false)
      %parallel = maps:get(parallel, Options, false)
    });

keys_with(Address, Function, Map, Options) ->
    bad_argument_handler(?FUNCTION_NAME, Address, Function, 2, Map, Options).

%%%-----------------------------------------------------------------------------
%%% @doc
%%% ``put(Address, Value, Map1, Strict) -> Map2''
%%%
%%% Types:
%%% ```
%%% Address = strict_address() | address()
%%% Value = any()
%%% Map1 = Map2 = map()
%%% Strict = boolean()
%%% '''
%%%
%%% Associates end keys from `Address' with value `Value' and inserts the associations into map `Map2'. If an end
%%% key already exists in map `Map1', the old associated value is replaced by value `Value'. The function
%%% returns a new map `Map2' containing the new associations and the old associations in `Map1'.
%%%
%%% == When Strict =:= 'true': ==
%%%
%%%   `Address' must be of type {@link strict_address()}.
%%%
%%%   The call fails with a ``{badkey, Key}'' exception if a non-end key from `Address' is absent in `Map1'.
%%%
%%%   The call fails with a ``{badarg, {'Address', Address}}'' exception if `Address' is not {@link strict_address()}.
%%%
%%%   The call fails with an ``{unreachable_address, Address}'' exception if a wildcard key list (`` '*' '')
%%%   is specified in `Address', and it expands to an empty list in `Map1' (i.e. there is an empty map).
%%%
%%% == When Strict =:= 'false': ==
%%%
%%%   `Address' must be of type {@link address()}.
%%%
%%%   All non-end keys from `Address' which are absent in `Map1' are created in `Map2'.
%%%
%%%   If an empty key list (`[]') is specified in `Address' the function returns the original map `Map1'.
%%%
%%%   The call fails with a ``{badarg, {'Address', Address}}'' exception if `Address' is not {@link address()}.
%%%
%%% == In any case the call fails with: ==
%%%
%%%  ``{badarg, {'Address', Address}}'' exception if any element of `Address' is not `list()' or wildcard (`` '*' '').
%%%
%%%  ``{badarg, {'Map1', Map1}}'' exception if `Map1' is not `map()'.
%%%
%%%  ``{badarg, {'Strict', Strict}}'' exception if `Strict' is not `boolean()'.
%%%
%%%  ``{badmap, Map1}'' exception if a non-end key from `Address' is not associated with a map in `Map1'.
%%% @end
%%%-----------------------------------------------------------------------------
-spec put(Address :: strict_address(),
          Value :: any(),
          Map1 :: map(),
          #{strict => true}) -> Map2 :: map();
         (Address :: address(),
          Value :: any(),
          Map1 :: map(),
          #{strict => false}) -> Map2 :: map().
put([_H | _T] = Address, Value, Map, Options)
    when is_list(Address), is_map(Map), is_map(Options) ->
    handle_procedure_wrapper(Address, Map, #params{
        address = Address,
        value   = Value,
        op      = ?FUNCTION_NAME,
        op_type = get_operation_type(?FUNCTION_NAME),
      strict   = maps:get(strict, Options, false)
      %parallel = maps:get(parallel, Options, false)
    });

put(Address, _Value, Map, Options) ->
    bad_argument_handler(?FUNCTION_NAME, Address, Map, Options).

%%%-----------------------------------------------------------------------------
%%% @doc
%%% ``put_with(Address, Function, Map1, Strict) -> Map2''
%%%
%%% Types:
%%% ```
%%% Address = strict_address() | address()
%%% Function = fun((EndKey, EndValue, IsValueExists) -> Value)
%%% EndKey = EndValue = Value = any()
%%% IsValueExists = Strict = boolean()
%%% Map1 = Map2 = map()
%%% '''
%%%
%%% Same as {@link put/4} but:
%%%
%%% For each end key `EndKey' from `Address' not existing in `Map1' a new association is created in `Map2'
%%% using a result (`Value') of calling ``Function(EndKey, undefined, false)''.
%%%
%%% For each end key `EndKey' from `Address' existing in `Map1' the association is replaced in `Map2'
%%% with a result (`Value') of calling ``Function(EndKey, EndValue, true)''.
%%%
%%% The call fails with a ``{badarg, {'Function', Function}}'' exception if `Function' is not `fun()' of arity 3.
%%%
%%% The call fails with a ``{badfun, {Reason, Stack}}'' exception if a call to `Function' fails with `Reason',
%%% where `Stack' is a stack back-trace.
%%% @end
%%%-----------------------------------------------------------------------------
-spec put_with(Address :: strict_address(),
               Function :: modify_fun3(),
               Map1 :: map(),
               #{strict => true}) -> Map2 :: map();
              (Address :: address(),
               Function :: modify_fun3(),
               Map1 :: map(),
               #{strict => false}) -> Map2 :: map().
put_with([_H | _T] = Address, Function, Map, Options)
    when is_list(Address), is_function(Function, 3), is_map(Map), is_map(Options) ->
    handle_procedure_wrapper(Address, Map, #params{
        address  = Address,
        function = Function,
        op       = ?FUNCTION_NAME,
        op_type  = get_operation_type(?FUNCTION_NAME),
      strict   = maps:get(strict, Options, false)
      %parallel = maps:get(parallel, Options, false)
    });

put_with(Address, Function, Map, Options) ->
    bad_argument_handler(?FUNCTION_NAME, Address, Function, 3, Map, Options).

%%%-----------------------------------------------------------------------------
%%% @doc
%%% ``update(Address, Value, Map1, Strict) -> Map2''
%%%
%%% Types:
%%% ```
%%% Address = strict_address() | address()
%%% Value = any()
%%% Map1 = Map2 = map()
%%% Strict = boolean()
%%% '''
%%%
%%% For end keys from `Address' existing in `Map1', the associated values are replaced by value `Value'.
%%% The function returns a new map `Map2' containing the new associated values and the old associations in `Map1'.
%%%
%%% == When Strict =:= 'true': ==
%%%
%%%   `Address' must be of type {@link strict_address()}.
%%%
%%%   The call fails with a ``{badkey, Key}'' exception if any key specified in `Address' is absent in `Map1'.
%%%
%%%   The call fails with a ``{badarg, {'Address', Address}}'' exception if `Address' is not {@link strict_address()}.
%%%
%%%   The call fails with an ``{unreachable_address, Address}'' exception if a wildcard key list (`` '*' '')
%%%   is specified in `Address', and it expands to an empty list in `Map' (i.e. there is an empty map).
%%%
%%% == When Strict =:= 'false': ==
%%%
%%%   `Address' must be of type {@link address()}.
%%%
%%%   Any keys from `Address' not found in `Map1' are ignored and not processed further.
%%%
%%%   If an empty key list (`[]') is specified in `Address' the function returns the original map `Map1'.
%%%
%%%   The call fails with a ``{badarg, {'Address', Address}}'' exception if `Address' is not {@link address()}.
%%%
%%% == In any case the call fails with: ==
%%%
%%%  ``{badarg, {'Address', Address}}'' exception if any element of `Address' is not `list()' or wildcard (`` '*' '').
%%%
%%%  ``{badarg, {'Map1', Map1}}'' exception if `Map1' is not `map()'.
%%%
%%%  ``{badarg, {'Strict', Strict}}'' exception if `Strict' is not `boolean()'.
%%%
%%%  ``{badmap, Map1}'' exception if a non-end key from `Address' is not associated with a map in `Map1'.
%%% @end
%%%-----------------------------------------------------------------------------
-spec update(Address :: strict_address(),
             Value :: any(),
             Map1 :: map(),
             #{strict => true}) -> Map2 :: map();
            (Address :: address(),
             Value :: any(),
             Map1 :: map(),
             #{strict => false}) -> Map2 :: map().
update([_H | _T] = Address, Value, Map, Options)
    when is_list(Address), is_map(Map), is_map(Options) ->
    handle_procedure_wrapper(Address, Map, #params{
        address = Address,
        value   = Value,
        op      = ?FUNCTION_NAME,
        op_type = get_operation_type(?FUNCTION_NAME),
      strict   = maps:get(strict, Options, false)
      %parallel = maps:get(parallel, Options, false)
    });

update(Address, _Value, Map, Options) ->
    bad_argument_handler(?FUNCTION_NAME, Address, Map, Options).

%%%-----------------------------------------------------------------------------
%%% @doc
%%% ``update_with(Address, Function, Map1, Strict) -> Map2''
%%%
%%% Types:
%%% ```
%%% Address = strict_address() | address()
%%% Function = fun((EndKey, EndValue) -> Value)
%%% EndKey = EndValue = Value = any()
%%% Map1 = Map2 = map()
%%% Strict = boolean()
%%% '''
%%%
%%% Same as {@link update/4} but:
%%%
%%% For each end key `EndKey' from `Address' existing in `Map1', the associated value `EndValue'
%%% is replaced in `Map2' with the result (`Value') of calling `Function(EndKey, EndValue)'.
%%%
%%% The call fails with a ``{badarg, {'Function', Function}}'' exception if `Function' is not `fun()' of arity 2.
%%%
%%% The call fails with a ``{badfun, {Reason, Stack}}'' exception if a call to `Function' fails with `Reason',
%%% where `Stack' is a stack back-trace.
%%% @end
%%%-----------------------------------------------------------------------------
-spec update_with(Address :: strict_address(),
                  Function :: modify_fun2(),
                  Map1 :: map(),
                  #{strict => true}) -> Map2 :: map();
                 (Address :: address(),
                  Function :: modify_fun2(),
                  Map1 :: map(),
                  #{strict => false}) -> Map2 :: map().
update_with([_H | _T] = Address, Function, Map, Options)
    when is_list(Address), is_function(Function, 2), is_map(Map), is_map(Options) ->
    handle_procedure_wrapper(Address, Map, #params{
        address  = Address,
        function = Function,
        op       = ?FUNCTION_NAME,
        op_type  = get_operation_type(?FUNCTION_NAME),
      strict   = maps:get(strict, Options, false)
      %parallel = maps:get(parallel, Options, false)
    });

update_with(Address, Function, Map, Options) ->
    bad_argument_handler(?FUNCTION_NAME, Address, Function, 2, Map, Options).

%%%-----------------------------------------------------------------------------
%%% @doc
%%% ``remove(Address, Map1, Strict) -> Map2''
%%%
%%% Types:
%%% ```
%%% Address = strict_address() | address()
%%% Map1 = Map2 = map()
%%% Strict = boolean()
%%% '''
%%%
%%% For all end keys from `Address' found in `Map1' removes the keys and associated values, and returns a new `Map2'
%%% without that keys.
%%%
%%% == When Strict =:= 'true': ==
%%%
%%%   `Address' must be of type {@link strict_address()}
%%%
%%%   The call fails with a ``{badkey, Key}'' exception if a non-end key from `Address' is absent in `Map1'.
%%%
%%%   The call fails with a ``{badarg, {'Address', Address}}'' exception if `Address' is not {@link strict_address()}.
%%%
%%%   The call fails with an ``{unreachable_address, Address}'' exception if a non-end key list is specified as
%%%   a wildcard (`` '*' '') in `Address', and it expands to an empty list in `Map' (i.e. there is an empty map).
%%%
%%% == When Strict =:= 'false': ==
%%%
%%%   `Address' must be of type {@link address()}.
%%%
%%%   Any keys from `Address' not found in `Map1' are ignored and not processed further.
%%%
%%%   If an empty key list (`[]') is specified in `Address' the function returns the original map `Map1'.
%%%
%%%   The call fails with a ``{badarg, {'Address', Address}}'' exception if `Address' is not {@link address()}.
%%%
%%% == In any case the call fails with: ==
%%%
%%%  ``{badarg, {'Address', Address}}'' exception if any element of `Address' is not `list()' or wildcard (`` '*' '').
%%%
%%%  ``{badarg, {'Map1', Map1}}'' exception if `Map1' is not `map()'.
%%%
%%%  ``{badarg, {'Strict', Strict}}'' exception if `Strict' is not `boolean()'.
%%%
%%%  ``{badmap, Map1}'' exception if a non-end key from `Address' is not associated with a map in `Map1'.
%%% @end
%%%-----------------------------------------------------------------------------
-spec remove(Address :: strict_address(),
             Map1 :: map(),
             #{strict => true}) -> Map2 :: map();
            (Address :: address(),
             Map1 :: map(),
             #{strict => false}) -> Map2 :: map().
remove([_H | _T] = Address, Map, Options)
    when is_list(Address), is_map(Map), is_map(Options) ->
    handle_procedure_wrapper(Address, Map, #params{
        address = Address,
        op      = ?FUNCTION_NAME,
        op_type = get_operation_type(?FUNCTION_NAME),
      strict   = maps:get(strict, Options, false)
      %parallel = maps:get(parallel, Options, false)
    });

remove(Address, Map, Options) ->
    bad_argument_handler(?FUNCTION_NAME, Address, Map, Options).

%%%-----------------------------------------------------------------------------
%%% @doc
%%% ``remove_with(Address, Function, Map1, Strict) -> Map2''
%%%
%%% Types:
%%% ```
%%% Address = strict_address() | address()
%%% Function = fun((EndKey, EndValue) -> ShouldRemove)
%%% EndKey = EndValue = any()
%%% ShouldRemove = Strict = boolean()
%%% Map1 = Map2 = map()
%%% '''
%%%
%%% Same as {@link remove/3} but:
%%%
%%% For each end key `EndKey' from `Address' found in `Map1' removes the key and the associated value `EndValue'
%%% if ``Function(EndKey, EndValue) =:= 'true' '', and returns a new `Map2' without that keys.
%%%
%%% The call fails with a ``{badarg, {'Function', Function}}'' exception if `Function' is not `fun()' of arity 2 or
%%% if a call to `Function' returns not `boolean()'.
%%%
%%% The call fails with a ``{badfun, {Reason, Stack}}'' exception if a call to `Function' fails with `Reason',
%%% where `Stack' is a stack back-trace.
%%% @end
%%%-----------------------------------------------------------------------------
-spec remove_with(Address :: strict_address(),

                  Function :: modify_fun2(),
                  Map1 :: map(),
                  #{strict => true}) -> Map2 :: map();
                 (Address :: address(),
                  Function :: modify_fun2(),
                  Map1 :: map(),
                  #{strict => false}) -> Map2 :: map().
remove_with([_H | _T] = Address, Function, Map, Options)
    when is_list(Address), is_function(Function, 2), is_map(Map), is_map(Options) ->
    handle_procedure_wrapper(Address, Map, #params{
        address  = Address,
        function = Function,
        op       = ?FUNCTION_NAME,
        op_type  = get_operation_type(?FUNCTION_NAME),
      strict   = maps:get(strict, Options, false)
      %parallel = maps:get(parallel, Options, false)
    });

remove_with(Address, Function, Map, Options) ->
    bad_argument_handler(?FUNCTION_NAME, Address, Function, 2, Map, Options).

%%%-----------------------------------------------------------------------------
%%% @doc
%%% ``take(Address, Map1, Strict) -> {Values, Map2}''
%%%
%%% Types:
%%% ```
%%%  Address = strict_address() | address()
%%%  Map1 = Map2 = map()
%%%  Strict = boolean()
%%%  Values = list(Value)
%%%  Value = any()
%%% '''
%%%
%%% == When Strict =:= 'true': ==
%%%
%%%   `Address' must be of type {@link strict_address()}.
%%%
%%%   For all end keys from `Address' found in `Map1' removes the keys and associated values and returns a tuple
%%%   with the removed `Values' and the `Map2' without removed keys.
%%%
%%%   The call fails with a `{badkey, Key}' exception if any key specified in `Address' is absent in `Map1'.
%%%
%%%   The call fails with a ``{badarg, {'Address', Address}}'' exception if `Address' is not {@link strict_address()}.
%%%
%%%   The call fails with an ``{unreachable_address, Address}'' exception if a wildcard key list (`` '*' '')
%%%   is specified in `Address', and it expands to an empty list in `Map1' (i.e. there is an empty map).
%%%
%%% == When Strict =:= 'false': ==
%%%
%%%   `Address' must be of type {@link address()}.
%%%
%%%   Returns a tuple with `Values' associated with end keys from `Address' which could be found in `Map1' and the
%%%  `Map2' without these keys.
%%%
%%%   If no end keys could be found in `Map1' or if an empty key list (`[]') is specified in `Address',
%%%   the function returns a tuple with empty list (`[]') and `Map2' which is equal to `Map1'.
%%%
%%%   The call fails with a ``{badarg, {'Address', Address}}'' exception if `Address' is not {@link address()}.
%%%
%%%  == In any case the call fails with: ==
%%%
%%%  ``{badarg, {'Address', Address}}'' exception if any element of `Address' is not `list()' or wildcard (`` '*' '').
%%%
%%%  ``{badarg, {'Map', Map}}'' exception if `Map1' is not `map()'.
%%%
%%%  ``{badarg, {'Strict', Strict}}'' exception if `Strict' is not `boolean()'.
%%%
%%%  ``{badmap, Map}'' exception if a non-end key from `Address' is not associated with a map in `Map1'.
%%% @end
%%%-----------------------------------------------------------------------------
-spec take(Address :: strict_address(),
           Map1 :: map(),
           #{strict => true}) -> {nonempty_list(Value :: any()), Map2 :: map()};
          (Address :: address(),
           Map1 :: map(),
           #{strict => false}) -> {list(Value :: any()), Map2 :: map()}.

take([_H | _T] = Address, Map, Options)
    when is_list(Address), is_map(Map), is_map(Options) ->
    handle_procedure_wrapper(Address, Map, #params{
        address = Address,
        op      = ?FUNCTION_NAME,
        op_type = get_operation_type(?FUNCTION_NAME),
      strict   = maps:get(strict, Options, false)
      %parallel = maps:get(parallel, Options, false)
    });

take(Address, Map, Options) ->
    bad_argument_handler(?FUNCTION_NAME, Address, Map, Options).

%%%-----------------------------------------------------------------------------
%%% @doc
%%% ``take_with(Address, Function, Map1, Strict) -> {Values, Map2}''
%%%
%%% Types:
%%% ```
%%%  Address = strict_address() | address()
%%%  Function = fun((EndKey, EndValue) -> ShouldRemove)
%%%  EndKey = EndValue = any()
%%%  ShouldRemove = Strict = boolean()
%%%  Map1 = Map2 = map()
%%%  Values = list(Value)
%%%  Value = any()
%%% '''
%%%
%%% Same as {@link take/3} but:
%%%
%%% For each end key `EndKey' from `Address' found in `Map1' removes the key and the associated value `EndValue'
%%% if ``Function(EndKey, EndValue) =:= 'true' '', and returns a tuple with the removed `Values' and the
%%% `Map2' without removed keys.
%%%
%%% The call fails with a ``{badarg, {'Function', Function}}'' exception if `Function' is not `fun()' of arity 2 or
%%% if a call to `Function' returns not `boolean()'.
%%%
%%% The call fails with a ``{badfun, {Reason, Stack}}'' exception if a call to `Function' fails with `Reason',
%%% where `Stack' is a stack back-trace.
%%% @end
%%%-----------------------------------------------------------------------------
-spec take_with(Address :: strict_address(),
                Function :: filter_fun(),
                Map1 :: map(),
                #{strict => true}) -> {list(Value :: any()), Map2 :: map()};
               (Address :: address(),
                Function :: filter_fun(),
                Map1 :: map(),
                #{strict => false}) -> {list(Value :: any()), Map2 :: map()}.
take_with([_H | _T] = Address, Function, Map, Options)
    when is_list(Address), is_function(Function, 2), is_map(Map), is_map(Options) ->
    handle_procedure_wrapper(Address, Map, #params{
        address  = Address,
        function = Function,
        op       = ?FUNCTION_NAME,
        op_type  = get_operation_type(?FUNCTION_NAME),
      strict   = maps:get(strict, Options, false)
      %parallel = maps:get(parallel, Options, false)
    });

take_with(Address, Function, Map, Options) ->
    bad_argument_handler(?FUNCTION_NAME, Address, Function, 2, Map, Options).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% handle element
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_element(Key :: any(),
                     IsValueExists :: boolean(),
                     ExistedValue :: any(),
                     Acc :: list(any()),
                     Map1 :: map(),
                     Parameters :: #params{
                     op :: get_with | put_with| update_with | remove_with | keys_with | take_with,
                     function :: filter_fun() | modify_fun2() | modify_fun3()
                     }) -> {list(Value :: any()), Map2 :: map()};
                    (Key :: any(),
                     IsValueExists :: boolean(),
                     ExistedValue :: any(),
                     Acc :: list(any()),
                     Map1 :: map(),
                     Parameters :: #params{
                     op :: get | put | update| remove | keys | take,
                     function :: undefined
                     }) -> {list(Value :: any()), Map2 :: map()}.
handle_element(_Key, _IsValueExists, ExistedValue, Acc, Map, #params{op = get} = _Parameters) ->
    {[ExistedValue | Acc], Map};

handle_element(Key, _IsValueExists, ExistedValue, Acc, Map, #params{op       = get_with,
                                                                    function = Function} = _Parameters) ->
    try Function(Key, ExistedValue) of
        true ->
            {[ExistedValue | Acc], Map};
        false ->
            {Acc, Map};
        _ ->
            erlang:error({badarg, {'Function', Function}})
    catch
        _:Error:StackTrace ->
            erlang:error({badfun, {Error, StackTrace}})
    end;

handle_element(Key, _IsValueExists, _ExistedValue, _Acc, Map,
               #params{op = put, value = Value}) ->
    {[], maps:put(Key, Value, Map)};

handle_element(Key, IsValueExists, ExistedValue, Acc, Map, #params{op       = put_with,
                                                                   function = Function}) ->
    NewValue = try
                   Function(Key, ExistedValue, IsValueExists)
               catch
                   _:Error:StackTrace ->
                       erlang:error({badfun, {Error, StackTrace}})
               end,
    case NewValue of
        ExistedValue -> {Acc, Map};
        NewValue ->  {Acc, maps:put(Key, NewValue, Map)}
    end;

handle_element(Key, _IsValueExists, _ExistedValue, Acc, Map,
               #params{op = update, value = Value}) ->
    {Acc, maps:put(Key, Value, Map)};

handle_element(Key, _IsValueExists, ExistedValue, Acc, Map, #params{op       = update_with,
                                                                    function = Function}) ->
    NewValue = try
                   Function(Key, ExistedValue)
               catch
                   _:Error:StackTrace ->
                       erlang:error({badfun, {Error, StackTrace}})
               end,
    case NewValue of
        ExistedValue -> {Acc, Map};
        NewValue ->  {Acc, maps:put(Key, NewValue, Map)}
    end;

handle_element(Key, _IsValueExists, _ExistedValue, Acc, Map, #params{op = remove}) ->
    {Acc, maps:remove(Key, Map)};

handle_element(Key, _IsValueExists, ExistedValue, Acc, Map, #params{op       = remove_with,
                                                                    function = Function}) ->
    try Function(Key, ExistedValue) of
        true ->
            {Acc, maps:remove(Key, Map)};
        false ->
            {Acc, Map};
        _ ->
            erlang:error({badarg, {'Function', Function}})
    catch
        _:Error:StackTrace ->
            erlang:error({badfun, {Error, StackTrace}})
    end;

handle_element(Key, _IsValueExists, ExistedValue, Acc, Map, #params{op     = keys,
                                                                    strict = Strict} = _Parameters) when is_map(
    ExistedValue) ->
    Keys = maps:keys(ExistedValue),
    case {Strict, Keys} of
        {true, []} ->
            erlang:error({badkey, Key});
        {false, []} ->
            {Acc, Map};
        _ ->
            {maps:keys(ExistedValue) ++ Acc, Map}
    end;

handle_element(_Key, _IsValueExists, ExistedValue, _Acc, _Map, #params{op     = keys,
                                                                       strict = true} = _Parameters) ->
    erlang:error({badmap, ExistedValue});

handle_element(_Key, _IsValueExists, _ExistedValue, Acc, Map,
               #params{op = keys, strict = false} = _Parameters) ->
    {Acc, Map};

handle_element(Key, _IsValueExists, ExistedValue, Acc, Map, #params{op       = keys_with,
                                                                    function = Function,
                                                                    strict   = Strict} = _Parameters) when is_map(
    ExistedValue) ->
    Keys = maps:keys(ExistedValue),
    case {Strict, Keys} of
        {true, []} ->
            erlang:error({badkey, Key});
        {false, []} ->
            {Acc, Map};
        _ ->
            NewAcc = lists:foldl(fun(InnerKey, InnerAcc) ->
                Value = maps:get(InnerKey, ExistedValue),
                try Function(InnerKey, Value) of
                    true ->
                        [InnerKey | InnerAcc];
                    false ->
                        InnerAcc;
                    _ ->
                        erlang:error({badarg, {'Function', Function}})
                catch
                    _:Error:StackTrace ->
                        erlang:error({badfun, {Error, StackTrace}})
                end
                                 end,
                                 Acc, Keys),
            {NewAcc, Map}
    end;

handle_element(_Key, _IsValueExists, ExistedValue, _Acc, _Map, #params{op     = keys_with,
                                                                       strict = true} = _Parameters) ->
    erlang:error({badmap, ExistedValue});

handle_element(_Key, _IsValueExists, _ExistedValue, Acc, Map, #params{op     = keys_with,
                                                                      strict = false}) ->
    {Acc, Map};

handle_element(Key, _IsValueExists, ExistedValue, Acc, Map, #params{op = take} = _Parameters) ->
    {[ExistedValue | Acc], maps:remove(Key, Map)};

handle_element(Key, _IsValueExists, ExistedValue, Acc, Map, #params{op       = take_with,
                                                                    function = Function} = _Parameters) ->
    try Function(Key, ExistedValue) of
        true ->
            {[ExistedValue | Acc], maps:remove(Key, Map)};
        false ->
            {Acc, Map};
        _ ->
            erlang:error({badarg, {'Function', Function}})
    catch
        _:Error:StackTrace ->
            erlang:error({badfun, {Error, StackTrace}})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handle procedure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec key_handler(Key :: any(),
    Keys :: list(any()),
    Acc :: list(),
    Map1 :: map(),
    Parameters :: #params{}) -> {list(any()) | nothing, any() | nothing}.

key_handler(Key, Keys, Acc, Map, #params{strict = Strict} = Parameters) ->
    case maps:find(Key, Map) of
        error ->
            case {is_alowed_to_create_new_element(Parameters), Strict, Keys} of
                {false, false, _} ->
                    {nothing, nothing};
                {true, _, []} ->
                    {element, false, undefined};
                {_, true, _} ->
                    erlang:error({badkey, Key});
                {true, false, Keys} ->
                    handle_keys(Keys, Acc, #{}, Parameters)
            end;
        {ok, ExistedValue} ->
            case Keys of
                [] ->
                    {element, true, ExistedValue};
                Keys ->
                    handle_keys(Keys, Acc, ExistedValue, Parameters)
            end
    end.

-spec handle_keys(Address :: strict_address(),
                  Acc :: list(),
                  Map1 :: map(),
                  Parameters :: #params{strict :: true}) -> Map2 :: map().
handle_keys(_Address, _Acc, Map, _Parameters) when not is_map(Map) ->
    throw(error_badmap);

handle_keys([[] = _Head | _Tail] = _Address, _Acc, _Map, #params{strict = true} = Parameters) ->
    erlang:error({bad_address, Parameters#params.address});

handle_keys([[] = _Head | _Tail] = _Address, _Acc, _Map, #params{strict = false} = _Parameters) ->
    throw(cancel_action);

handle_keys(['*' | Tail] = Address, Acc, Map, #params{strict = Strict} = Parameters) ->
    AllKeys = maps:keys(Map),
    case {Strict, AllKeys} of
        {true, []} ->
            erlang:error({unreachable_address, Parameters#params.address});
        {false, []} when Address == Parameters#params.address->
            {Acc, Map};
        {false, []} ->
            {nothing, nothing};
        _ ->
            handle_keys([AllKeys] ++ Tail, Acc, Map, Parameters)
    end;

handle_keys([Keys | Tail] = _Address, Acc, Map, #params{parallel = false} = Parameters) when is_list(Keys) ->
    lists:foldl(fun(Key, {InnerAcc, InnerMap}) ->
        case key_handler(Key, Tail, Acc, Map, Parameters) of
            {element, IsValueExists, ExistedValue} ->
                handle_element(Key, IsValueExists, ExistedValue, InnerAcc, InnerMap, Parameters);
            {NewAccValue, NewMapValue} ->
                OuterAcc = case NewAccValue of
                               nothing ->
                                   InnerAcc;
                               NewAccValue when is_list(NewAccValue) ->
                                   InnerAcc ++ NewAccValue
                           end,
                OuterMap = case NewMapValue of
                               nothing ->
                                   InnerMap;
                               NewMapValue ->
                                   maps:put(Key, NewMapValue, InnerMap)
                           end,
                {OuterAcc, OuterMap}
        end
                end, {Acc, Map}, Keys);

%%handle_keys([Keys | Tail] = _Address, Acc, Map, #params{parallel = true} = Parameters) when is_list(Keys) ->
%%
%%    KeyHandlerParallelFun = fun(From, Key) ->
%%        Result = key_handler(Key, Tail, Acc, Map, Parameters),
%%        From ! {Key, Result}
%%                            end,
%%
%%    Self = self(),
%%    KeyHandlerPids = [spawn_link(fun() -> KeyHandlerParallelFun(Self, Key) end) || Key <- Keys],
%%    Results = [begin
%%                   receive
%%                       {_Key, {_NewAcc, _NewMap}} = Msg -> Msg;
%%                       {_Key, {element, _IsValueExists, _ExistedValue}} = Msg -> Msg
%%                   after 5000 ->
%%                        error(timeout)
%%                   end
%%               end || _Num <- lists:seq(1, length(KeyHandlerPids))],
%%    lists:foldl(fun({Key, {element, IsValueExists, ExistedValue}},  {InnerAcc, InnerMap}) ->
%%                        handle_element(Key, IsValueExists, ExistedValue, InnerAcc, InnerMap, Parameters);
%%                   ({Key, {NewAcc, NewValue}}, {InnerAcc, InnerMap}) ->
%%        OuterAcc = case NewAcc of
%%                       nothing ->
%%                           InnerAcc;
%%                       NewAcc when is_list(NewAcc) ->
%%                           InnerAcc ++ NewAcc
%%                   end,
%%        OuterMap = case NewValue of
%%                       nothing ->
%%                           InnerMap;
%%                       NewValue ->
%%                           maps:put(Key, NewValue, InnerMap)
%%                   end,
%%        {OuterAcc, OuterMap}
%%                end,
%%        {Acc, Map}, Results);

handle_keys([Head | _Tail] = _Address, _Acc, _Map, Parameters) when not is_list(Head) ->
    erlang:error({badarg, {'Address', Parameters#params.address}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_procedure_wrapper(Address, Map, #params{op_type = Type} = Parameters) ->
    handle_return_value(try
                            handle_keys(Address, [], Map, Parameters)
                        catch
                            throw:cancel_action -> {[], Map};
                            throw:error_badmap ->
                                erlang:error({badmap, Map})
                        end, Type).

is_alowed_to_create_new_element(#params{op = put})      -> true;
is_alowed_to_create_new_element(#params{op = put_with}) -> true;
is_alowed_to_create_new_element(_)                      -> false.

get_operation_type(get)       -> collect;
get_operation_type(get_with)  -> collect;
get_operation_type(keys)      -> collect;
get_operation_type(keys_with) -> collect;
get_operation_type(take)      -> multitask;
get_operation_type(take_with) -> multitask;
get_operation_type(_)         -> modify.

handle_return_value({Acc, _}, collect) -> Acc;
handle_return_value({_, Map}, modify)  -> Map;
handle_return_value(Value, multitask)  -> Value.

bad_argument_handler(Operation, Address, Function, FunctionArity, Map, Options) ->
    case is_function(Function, FunctionArity) of
        false ->
            erlang:error({badarg, {'Function', Function}});
        true ->
            bad_argument_handler(Operation, Address, Map, Options)
    end.

bad_argument_handler(Operation, Address, Map, Options) ->
    case {is_list(Address), is_map(Map), is_map(Options)} of
        {_, _, false} ->
            erlang:error({badarg, {'Options', Options}});
        {_, false, _} ->
            erlang:error({badarg, {'Map', Map}});
        {false, _, _} ->
            erlang:error({badarg, {'Address', Address}});
        {true, _, _} when Address == [] ->
            Strict = maps:get(strict, Options, false),
            case {Strict, handle_return_value({[], Map}, get_operation_type(Operation))} of
                {true, _} ->
                    erlang:error({badarg, {'Address', Address}});
                {false, Value} ->
                    Value
            end
    end.