-module(jason_tests).

-include_lib("eunit/include/eunit.hrl").

long_key()-> string:copies("a", 260).


jason_decode_options_test() ->
     ?assertEqual([{<<"ab">>,<<"cd">>}], jason:decode('{"ab": "cd"}'))
     ,?assertEqual({ok,[{<<"ab">>,<<"cd">>}]}, jason:decode('{"ab": "cd"}', [{return, tuple}]))
     ,?assertMatch({1, _}, catch jason:decode('{"ab": "cd"'))
     ,?assertMatch({error,{1,_}}, jason:decode('{"ab": "cd"', [{return, tuple}]))
     ,?assertEqual(enoent, catch jason:decode_file(""))
     ,?assertEqual({error, enoent}, jason:decode_file("", [{return, tuple}]))
     .

jason_decode_types_test() ->
%% Atoms
% null                 -> null
?assertEqual(null, jason:decode("null"))
% true                 -> true
,?assertEqual(true, jason:decode("true"))
% false                -> false
,?assertEqual(false, jason:decode("false"))
% "any"                -> <<"any">>
,?assertEqual(<<"any">>, jason:decode('"any"'))

%% Integer
% 123                  -> 123
,?assertEqual(123, jason:decode("123"))

%% Float (Automatic precision)
% 123.456789           -> 123.456789
,?assertEqual(123.456789, jason:decode("123.456789"))
% 2.3                  -> 2.3
,?assertEqual(2.3, jason:decode("2.3"))
% 2300.0               -> 2.3e3
,?assertEqual(2.3e3, jason:decode("2300.0"))
% 0.0023               -> 0.0023
,?assertEqual(0.0023, jason:decode("0.0023"))

%% List
% [1,2,3]              -> [1,2,3]
,?assertEqual([1,2,3], jason:decode("[1,2,3]"))
% ["a","b","c"]        -> [<<"a">>,<<"b">>,<<"c">>]
,?assertEqual([<<"a">>,<<"b">>,<<"c">>], jason:decode('["a","b","c"]'))

%% Dates
%% {{1970,1,1},{0,0,0}}    -> "1970-01-01T00:00:00Z"
,?assertEqual({{1970,1,1},{0,0,0}}, jason:decode("\"1970-01-01T00:00:00Z\""))
%% {{1970,1,1},{0,0,0}}    -> "1970-01-01T00:00:00.000Z"
,?assertEqual({{1970,1,1},{0,0,0.0}}, jason:decode("\"1970-01-01T00:00:00.000Z\""))


%% Binary (key/value) mode=struct (default)
% "abc"                -> <<"abc">>
,?assertEqual(<<"abc">>, jason:decode('"abc"'))

%% Struct
%  mode=struct (default)
% {"abc": "def"}       -> [{<<"abc">>,<<"def">>}]
,?assertEqual([{<<"abc">>,<<"def">>}], jason:decode('{"abc": "def"}'))
,?assertEqual([{<<"abc">>,<<"def">>}], jason:decode('{"abc": "def"}', [{mode, struct}]))
,?assertEqual([{<<"abc">>,<<"def">>}], jason:decode('{"abc": "def"}', [{mode, whatever}]))
%  mode=proplist
% {"abc": "def"}       -> [{abc,"def"}]
,?assertEqual([{abc,"def"}], jason:decode('{"abc": "def"}', [{mode, proplist}]))
%  mode=map
% {"abc": "def"}       -> #{abc => "def"}
,?assertEqual(#{abc => "def"}, jason:decode('{"abc": "def"}', [{mode, map}]))
%  mode=record
% {"abc": "def"}       -> {'111259705',"def"}
,?assertEqual({'111259705',"def"}, jason:decode('{"abc": "def"}', [{mode, record}]))


%% Record -
% {"k1": 1,"k2": "ab"} -> [{<<"k1">>,1}, {<<"k2">>,<<"ab">>}]
,?assertEqual([{<<"k1">>,1}, {<<"k2">>,<<"ab">>}], jason:decode('{"k1": 1,"k2": "ab"}'))
,?assertEqual([{<<"k1">>,1}, {<<"k2">>,<<"ab">>}], jason:decode('{"k1": 1,"k2": "ab"}', [{mode, struct}]))
,?assertEqual([{<<"k1">>,1}, {<<"k2">>,<<"ab">>}], jason:decode('{"k1": 1,"k2": "ab"}', [{mode, whatever}]))
%  mode=proplist
% {"k1": 1,"k2": "ab"} -> [{k1,1},{k2,"ab"}]
,?assertEqual([{k1,1},{k2,"ab"}], jason:decode('{"k1": 1,"k2": "ab"}', [{mode, proplist}]))
%  mode=map
% {"k1": 1,"k2": "ab"} -> #{k1 => 1,k2 => "ab"}
,?assertEqual(#{k1 => 1,k2 => "ab"}, jason:decode('{"k1": 1,"k2": "ab"}', [{mode, map}]))
%  mode=record
% {"k1": 1,"k2": "ab"} -> {'8056669',1,"ab"}
,?assertEqual({'8056669',1,"ab"}, jason:decode('{"k1": 1,"k2": "ab"}', [{mode, record}]))

%%% Special cases
% Atom > 255 characters kept binary
%  mode=proplist
,?assertEqual([{list_to_binary(long_key()),"x"}], jason:decode("{\"" ++ long_key() ++ "\": \"x\"}", [{mode, proplist}]))
%  mode=map
,?assertEqual(#{list_to_binary(long_key()) => "x"}, jason:decode("{\"" ++ long_key() ++ "\": \"x\"}", [{mode, map}]))
%  mode=record
,?assertEqual(system_limit, catch jason:decode("{\"" ++ long_key() ++ "\": \"x\"}", [{mode, record}]))

% Atom with UTF-8 should be well handled

.

jason_decode_literals_test() ->
     %% Literals
     ?assertEqual({ok, false}, jason:decode(<<"false">>, [{return, tuple}])),
     ?assertEqual({ok, true}, jason:decode(<<"true">>, [{return, tuple}])),
     ?assertEqual({ok, null} , jason:decode(<<"null">>, [{return, tuple}])).

jason_decode_numbers_test() ->
     %% Numbers: Integer
     % positive integer
      ?assertEqual({ok, 1}, jason:decode(<<"1">>, [{return, tuple}])),
     % zero
      ?assertEqual({ok, 0}, jason:decode(<<"0">>, [{return, tuple}])),
     % negative integer
      ?assertEqual({ok, -1}, jason:decode(<<"-1">>, [{return, tuple}])),
     % large integer (no limit on size)
      ?assertEqual({ok, 111111111111111111111111111111111111111111111111111111111111111111111111111111},
        jason:decode(<<"111111111111111111111111111111111111111111111111111111111111111111111111111111">>, [{return, tuple}])),
     % integer with leading zero
      ?assertEqual({ok, 0} , jason:decode(<<"00">>, [{return, tuple}])),
      ?assertEqual({ok, 1}, jason:decode(<<"01">>, [{return, tuple}])),
      ?assertEqual({ok, -1}, jason:decode(<<"-01">>, [{return, tuple}])),
     % integer can't begin with an explicit plus sign",
      ?assertMatch({error, _}, jason:decode(<<"+1">>, [{return, tuple}])),

     %% Numbers: Floats
     % float: decimal notation",
      ?assertEqual({ok, 1.23}, jason:decode(<<"1.23">>, [{return, tuple}])),
      ?assertEqual({ok, 1.23456789}, jason:decode(<<"1.23456789">>, [{return, tuple}])),
     % float: exponential notation",
      ?assertEqual({ok, 12.345}, jason:decode(<<"12345e-3">>, [{return, tuple}])), % lower case 'e'
      ?assertEqual({ok, 12.345}, jason:decode(<<"12345E-3">>, [{return, tuple}])), % upper case 'E'
      ?assertEqual({ok, 12.345}, jason:decode(<<"12345.0e-3">>, [{return, tuple}])),
      ?assertEqual({ok, 12.345}, jason:decode(<<"0.12345E2">>, [{return, tuple}])),
      ?assertEqual({ok, 12.345}, jason:decode(<<"0.12345e+2">>, [{return, tuple}])), % exponent part can begin with plus sign
      ?assertEqual({ok, 12.345}, jason:decode(<<"0.12345E+2">>, [{return, tuple}])),
      ?assertEqual({ok, -12.345}, jason:decode(<<"-0.012345e3">>, [{return, tuple}])),
     % float: invalid format",
      ?assertMatch({error, _}, jason:decode(<<".123">>, [{return, tuple}])),  % omitted integer part
      ?assertMatch({error, _}, jason:decode(<<"0.">>, [{return, tuple}])),    % omitted fraction part: EOS
      ?assertMatch({error, _}, jason:decode(<<"0.e+3">>, [{return, tuple}])), % omitted fraction part: with exponent part
      ?assertMatch({error, _}, jason:decode(<<"0.1e">>, [{return, tuple}])),    % imcomplete fraction part
      ?assertMatch({error, _}, jason:decode(<<"0.1e-">>, [{return, tuple}])),   % imcomplete fraction part
      ?assertMatch({error, _}, jason:decode(<<"0.1ee-1">>, [{return, tuple}])), % duplicated 'e'
      ?assertMatch({error, _}, jason:decode(<<"0.1e--1">>, [{return, tuple}])), % duplicated sign
      ?assertMatch({error, _}, jason:decode(<<"0.1.2">>, [{return, tuple}])),   % duplicated '.': interpreted as individual tokens
      ok.

jason_decode_strings_test() ->
     %% Strings
     % simple string
     ?assertEqual({ok, <<"abc def">>}, jason:decode(<<"\"abc def\"">>, [{return, tuple}])),
     ?assertEqual({ok, <<"abc def">>}, jason:decode("\"abc def\"", [{return, tuple}])),
     ?assertEqual({ok, <<" abc def ">>}, jason:decode('" abc def "', [{return, tuple}])),
     % string: escaped characters",
     Input    = list_to_binary([$", [[$\\, C] || C <- [$", $/, $\\, $b, $f, $n, $r, $t]], $"]),
     Expected = {ok, <<"\"\/\\\b\f\n\r\t">>},
     ?assertEqual(Expected, jason:decode(Input, [{return, tuple}])),
     % string: escaped Unicode characters",
     %% japanese
     Input1    = <<"\"\\u3042\\u3044\\u3046\\u3048\\u304A\"">>,
     Expected1 = [12354,12356,12358,12360,12362],  %  <<"あいうえお">> UTF-8
     {ok, I1} = jason:decode(Input1, [{return, tuple}]),
     ?assertEqual(Expected1, unicode:characters_to_list(I1)),

     %% ascii
     Input2    = <<"\"\\u0061\\u0062\\u0063\"">>,
     Expected2 = {ok, <<"abc">>},
     ?assertEqual(Expected2, jason:decode(Input2, [{return, tuple}])),

     %% other multi-byte characters
     Input3    = <<"\"\\u06DD\\u06DE\\u10AE\\u10AF\"">>,
     Expected3 = [1757,1758,4270,4271], % <<"۝۞ႮႯ">>
     {ok, I3} = jason:decode(Input3, [{return, tuple}]),
     ?assertEqual(Expected3, unicode:characters_to_list(I3)),

     %% mixture of ascii and japanese characters
     Input4    = <<"\"a\\u30421\\u3044bb\\u304622\\u3048ccc\\u304A333\"">>,
     Expected4 = [97,12354,49,12356,98,98,12358,50,50,12360,99,99,99,12362,
 51,51,51],  % <<"aあ1いbbう22えcccお333">> UTF-8
     {ok, I4} = jason:decode(Input4, [{return, tuple}]),
     ?assertEqual(Expected4, unicode:characters_to_list(I4)),
     % string: surrogate pairs
     Input5    = <<"\"\\ud848\\udc49\\ud848\\udc9a\\ud848\\udcfc\"">>,
     %Expected5 = "𢁉𢂚𢃼", % TODO handle UTF16
     %?assertEqual(Expected5, jason:decode(Input5, [{return, tuple}])) ,
     ?assertMatch({error, _} , catch jason:decode(Input5, [{return, tuple}])) ,
     ok.

jason_decode_arrays_test() ->
     %% Arrays
     % simple array",
     Input6    = <<"[1,2,\"abc def\",null]">>,
     Expected6 = {ok, [1, 2, <<"abc def">>, null]},
     ?assertEqual(Expected6, jason:decode(Input6, [{return, tuple}])),
     % array: contains whitespaces
     Input7    = <<"[  1,\t2, \n \"abc def\",\r null]">>,
     Expected7 = {ok, [1, 2, <<"abc def">>, null]},
     ?assertEqual(Expected7, jason:decode(Input7, [{return, tuple}])),
     % empty array
     ?assertEqual({ok, []}, jason:decode(<<"[]">>, [{return, tuple}])),
     ?assertEqual({ok, []}, jason:decode(<<"[ \t\r\n]">>, [{return, tuple}])),
     % array: trailing comma is disallowed
     Input8 = <<"[1, 2, \"abc def\", null, ]">>,
     ?assertMatch({error, _}, jason:decode(Input8, [{return, tuple}])),
     % array: missing comma
     Input9 = <<"[1 2, \"abc def\", null]">>, % a missing comma between '1' and '2'
     ?assertMatch({error, _}, jason:decode(Input9, [{return, tuple}])),
     % array: missing closing bracket
     Input10 = <<"[1, 2, \"abc def\", null">>,
     ?assertMatch({error, _}, jason:decode(Input10, [{return, tuple}])),
     ok.

jason_decode_objects_test() ->
     %% Objects
     % simple object -> Record TODO

     % simple object -> Struct
     Input11   = <<"{\"1\":2,\"key\":\"value\"}">>,
     Expected11 = {ok, [{<<"1">>,2},{<<"key">>,<<"value">>}]},
     ?assertEqual(Expected11, jason:decode(Input11, [{return, tuple}])),
     % empty object
     ?assertEqual({ok, []}, jason:decode(<<"{}">>, [{return, tuple}])),
     % simple object -> map
     % object: trailing comma is disallowed
     Input12 = <<"{\"1\":2, \"key\":\"value\", }">>,
     ?assertMatch({error, _}, jason:decode(Input12, [{return, tuple}])),
     % object: missing comma
     Input13 = <<"{\"1\":2 \"key\":\"value\"}">>,
     ?assertMatch( {error, _}, jason:decode(Input13, [{return, tuple}])),
     % object: missing field key
     Input14 = <<"{:2, \"key\":\"value\"}">>,
     ?assertMatch({error, _} , jason:decode(Input14, [{return, tuple}])),
     % object: non string key
     Input15 = <<"{1:2, \"key\":\"value\"}">>,
     ?assertMatch({error, _} , jason:decode(Input15, [{return, tuple}])),
     % object: missing field value
     Input16 = <<"{\"1\", \"key\":\"value\"}">>,
     ?assertMatch({error, _} , jason:decode(Input16, [{return, tuple}])),
     % object: missing closing brace
     Input17 = <<"{\"1\":2 \"key\":\"value\"">>,
     ?assertMatch({error, _} , jason:decode(Input17, [{return, tuple}])),
     %% BINARY
     Input18 = <<"{\"1\":2,\"key\":\"value\"}">>,
     %% PROPLIST
     % object: proplist / binary k
     ?assertMatch( {ok,[{<<"1">>,2},{<<"key">>,"value"}]}, jason:decode(Input18, [{return, tuple}, {binary, k}, {mode, proplist}])),
     % object: proplist / binary v
     ?assertMatch( {ok,[{'1',2},{key,<<"value">>}]} , jason:decode(Input18, [{return, tuple}, {binary, v}, {mode, proplist}])),
     % object: proplist / binary kv
     ?assertMatch( {ok,[{<<"1">>,2},{<<"key">>,<<"value">>}]} , jason:decode(Input18, [{return, tuple}, {binary, kv}, {mode, proplist}])),
     %% MAP
     % object: map / binary k
     ?assertMatch({ok,#{<<"1">> := 2,<<"key">> := "value"}} , jason:decode(Input18, [{return, tuple}, {binary, k}, {mode, map}])),
     % object: map / binary v
     ?assertMatch({ok,#{'1' := 2,key := <<"value">>}} , jason:decode(Input18, [{return, tuple}, {binary, v}, {mode, map}])),
     % object: map / binary kv
     ?assertMatch({ok,#{<<"1">> := 2,<<"key">> := <<"value">>}} , jason:decode(Input18, [{return, tuple}, {binary, kv}, {mode, map}])),
     %% RECORD
     % object: record / binary k (does not change)
     ?assertMatch({ok,{'132007953',2,"value"}} , jason:decode(Input18, [{return, tuple}, {binary, k}, {mode, record}])),
     % object: record / binary v
     ?assertMatch( {ok,{'83030046',2,<<"value">>}} , jason:decode(Input18, [{return, tuple}, {binary, v}, {mode, record}])),
     % object: record / binary kv
     ?assertMatch({ok,{'83030046',2,<<"value">>}} , jason:decode(Input18, [{return, tuple}, {binary, kv}, {mode, record}])),
     ok.


