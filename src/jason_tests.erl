-module(jason_tests).

-include_lib("eunit/include/eunit.hrl").


jason_decode_test() -> 

    
     %% Literals
     ?assertEqual(false, jason:decode(<<"false">>)),
     ?assertEqual(true , jason:decode(<<"true">>)),
     ?assertEqual(null , jason:decode(<<"null">>)),

     %% Numbers: Integer
     % positive integer
      ?assertEqual(1, jason:decode(<<"1">>)),
     % zero 
      ?assertEqual(0, jason:decode(<<"0">>)),
     % negative integer
      ?assertEqual(-1, jason:decode(<<"-1">>)),
     % large integer (no limit on size)
      ?assertEqual(111111111111111111111111111111111111111111111111111111111111111111111111111111,
        jason:decode(<<"111111111111111111111111111111111111111111111111111111111111111111111111111111">>)),
     % integer with leading zero
      ?assertEqual(0 , jason:decode(<<"00">>)),
      ?assertEqual(1 , jason:decode(<<"01">>)),
      ?assertEqual(-1, jason:decode(<<"-01">>)),
     % integer can't begin with an explicit plus sign",
     % TODO ?assertMatch({error, {badarg, _}}, jason:decode(<<"+1">>)),

     %% Numbers: Floats
     % float: decimal notation",
     ?assertEqual(1.23, jason:decode(<<"1.23">>)),
     % float: exponential notation",
     ?assertEqual(12.345, jason:decode(<<"12345e-3">>)), % lower case 'e'
     ?assertEqual(12.345, jason:decode(<<"12345E-3">>)), % upper case 'E'
     ?assertEqual(12.345, jason:decode(<<"12345.0e-3">>)),
     ?assertEqual(12.345, jason:decode(<<"0.12345E2">>)),
     ?assertEqual(12.345, jason:decode(<<"0.12345e+2">>)), % exponent part can begin with plus sign
     ?assertEqual(12.345, jason:decode(<<"0.12345E+2">>)),
     ?assertEqual(-12.345, jason:decode(<<"-0.012345e3">>)),
     % float: invalid format",
     %?assertMatch({error, {badarg, _}}, jason:decode(<<".123">>)),  % omitted integer part
     %?assertMatch({error, {badarg, _}}, jason:decode(<<"0.">>)),    % omitted fraction part: EOS
     %?assertMatch({error, {badarg, _}}, jason:decode(<<"0.e+3">>)), % omitted fraction part: with exponent part
     %?assertMatch({error, {badarg, _}}, jason:decode(<<"0.1e">>)),    % imcomplete fraction part
     %?assertMatch({error, {badarg, _}}, jason:decode(<<"0.1e-">>)),   % imcomplete fraction part
     %?assertMatch({error, {badarg, _}}, jason:decode(<<"0.1ee-1">>)), % duplicated 'e'
     %?assertMatch({error, {badarg, _}}, jason:decode(<<"0.1e--1">>)), % duplicated sign
     %?assertEqual(0.1, <<".2">>}, jason:decode(<<"0.1.2">>)),     % duplicated '.': interpreted as individual tokens

     %% Strings
     % simple string
     ?assertEqual("abc", jason:decode(<<"\"abc\"">>)),
     ?assertEqual("abc", jason:decode("\"abc\"")),
     ?assertEqual("abc", jason:decode('"abc"')),
     % string: escaped characters",
     Input    = list_to_binary([$", [[$\\, C] || C <- [$", $/, $\\, $b, $f, $n, $r, $t]], $"]),
     Expected = "\"\/\\\b\f\n\r\t",
     ?assertEqual(Expected, jason:decode(Input)),
     % string: escaped Unicode characters",
     %% japanese
     Input1    = <<"\"\\u3042\\u3044\\u3046\\u3048\\u304A\"">>,
     Expected1 = "あいうえお",  % assumed that the encoding of this file is UTF-8
     ?assertEqual(Expected1, jason:decode(Input1)),

     %% ascii
     Input2    = <<"\"\\u0061\\u0062\\u0063\"">>,
     Expected2 = "abc",
     ?assertEqual(Expected2, jason:decode(Input2)),

     %% other multi-byte characters
     Input3    = <<"\"\\u06DD\\u06DE\\u10AE\\u10AF\"">>,
     Expected3 = "۝۞ႮႯ",
     ?assertEqual(Expected3, jason:decode(Input3)),

     %% mixture of ascii and japanese characters
     Input4    = <<"\"a\\u30421\\u3044bb\\u304622\\u3048ccc\\u304A333\"">>,
     Expected4 = "aあ1いbbう22えcccお333",  % assumed that the encoding of this file is UTF-8
     ?assertEqual(Expected4, jason:decode(Input4)),
     % string: surrogate pairs
     %Input5    = <<"\"\\ud848\\udc49\\ud848\\udc9a\\ud848\\udcfc\"">>,
     %Expected5 = "𢁉𢂚𢃼",
     %?assertEqual(Expected5, jason:decode(Input5)) ,

      % string: invalid escape characters",


     %% Arrays
     % simple array",
     Input6    = <<"[1,2,\"abc\",null]">>,
     Expected6 = [1, 2, "abc", null],
     ?assertEqual(Expected6, jason:decode(Input6)),
     % array: contains whitespaces
     Input7    = <<"[  1,\t2, \n \"abc\",\r null]">>,
     Expected7 = [1, 2, "abc", null],
     ?assertEqual(Expected7, jason:decode(Input7)),
     % empty array
     ?assertEqual([], jason:decode(<<"[]">>)),
     ?assertEqual([], jason:decode(<<"[ \t\r\n]">>)),
     % array: trailing comma is disallowed
     Input8 = <<"[1, 2, \"abc\", null, ]">>,
     ?assertMatch({error, _, _}, jason:decode(Input8)),
     % array: missing comma
     Input9 = <<"[1 2, \"abc\", null]">>, % a missing comma between '1' and '2'
     ?assertMatch({error, _, _}, jason:decode(Input9)),
     % array: missing closing bracket
     Input10 = <<"[1, 2, \"abc\", null">>,
     ?assertMatch({error, _, _}, jason:decode(Input10)),

     %% Objects
     % simple object
     %Input    = <<"{\"1\":2,\"key\":\"value\"}">>,
     %Expected = ?OBJ2(<<"1">>, 2, <<"key">>, <<"value">>),
     %?assertEqual(Expected, jason:decode(Input)), % `map' is the default format
     %?assertEqual(Expected, jason:decode(Input, [])),
     % simple object: tuple or proplist
     Input11   = <<"{\"1\":2,\"key\":\"value\"}">>,
     Expected11 = {struct,[{"1",2},{"key","value"}]},
     ?assertEqual(Expected11, jason:decode(Input11, [])),
     % object: contains whitespaces
     %Input    = <<"{  \"1\" :\t 2,\n\r\"key\" :   \n  \"value\"}">>,
     %Expected = ?OBJ2(<<"1">>, 2, <<"key">>, <<"value">>),
     %?assertEqual(Expected, jason:decode(Input)),
     % empty object
     %?assertEqual(?OBJ0, jason:decode(<<"{}">>)),
     %?assertEqual(?OBJ0, jason:decode(<<"{ \t\r\n}">>)),
     ?assertEqual({struct,[]}, jason:decode(<<"{}">>, [])),
     ?assertEqual({struct,[]}, jason:decode(<<"{}">>, [])),
     % empty object: map
     %?assertEqual(?OBJ0, jason:decode(<<"{}">>, [])),
     % duplicated members: map
     %Input    = <<"{\"1\":\"first\",\"1\":\"second\"}">>,
     %case ?MAP_OBJECT_TYPE of
     %    map   ->
     %        Expected = ?OBJ1(<<"1">>, <<"first">>), % the first (leftmost) value is used
     %        ?assertEqual(Expected, jason:decode(Input, []));
     %    tuple ->
     %        Expected = ?OBJ2(<<"1">>, <<"first">>, <<"1">>, <<"second">>),
     %        ?assertEqual(Expected, jason:decode(Input, []))
     %end,
     % object: trailing comma is disallowed
     Input12 = <<"{\"1\":2, \"key\":\"value\", }">>,
     ?assertMatch({error, _, _}, jason:decode(Input12, [])),
     % object: missing comma
     Input13 = <<"{\"1\":2 \"key\":\"value\"}">>,
     ?assertMatch( {error, _, _}, jason:decode(Input13)),
     % object: missing field key
     Input14 = <<"{:2, \"key\":\"value\"}">>,
     ?assertMatch({error, _, _} , jason:decode(Input14)),
     % object: non string key
     Input15 = <<"{1:2, \"key\":\"value\"}">>,
     ?assertMatch({error, _, _} , jason:decode(Input15)),
     % object: missing field value
     Input16 = <<"{\"1\", \"key\":\"value\"}">>,
     ?assertMatch({error, _, _} , jason:decode(Input16)),
     % object: missing closing brace
     Input17 = <<"{\"1\":2 \"key\":\"value\"">>,
     ?assertMatch({error, _, _} , jason:decode(Input17))

     %% Others
     % compound data
     %Input    = <<"  [true, {\"1\" : 2, \"array\":[[[[1]]], {\"ab\":\"cd\"}, false]}, null]   ">>,
     %Expected = [true, ?OBJ2(<<"1">>, 2, <<"array">>, [[[[1]]], ?OBJ1(<<"ab">>, <<"cd">>), false]), null],
     %?assertEqual(Expected, <<"   ">>}, jason:decode(Input))   
     .

