-module(jason_tests).

-include_lib("eunit/include/eunit.hrl").




jason_decode_literals_test() -> 
     %% Literals
     ?assertEqual(false, jason:decode(<<"false">>)),
     ?assertEqual(true , jason:decode(<<"true">>)),
     ?assertEqual(null , jason:decode(<<"null">>)).

jason_decode_numbers_test() -> 
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
     % TODO ?assertMatch({error, {badarg, _}}, jason:decode(<<"+1">>))

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
      ok.

jason_decode_strings_test() -> 
     %% Strings
     % simple string
     ?assertEqual(<<"abc def">>, jason:decode(<<"\"abc def\"">>)),
     ?assertEqual(<<"abc def">>, jason:decode("\"abc def\"")),
     ?assertEqual(<<" abc def ">>, jason:decode('" abc def "')),
     % string: escaped characters",
     Input    = list_to_binary([$", [[$\\, C] || C <- [$", $/, $\\, $b, $f, $n, $r, $t]], $"]),
     Expected = <<"\"\/\\\b\f\n\r\t">>,
     ?assertEqual(Expected, jason:decode(Input)),
     % string: escaped Unicode characters",
     %% japanese
     Input1    = <<"\"\\u3042\\u3044\\u3046\\u3048\\u304A\"">>,
     Expected1 = "あいうえお",  % assumed that the encoding of this file is UTF-8
     ?assertEqual(Expected1, unicode:characters_to_list(jason:decode(Input1))),

     %% ascii
     Input2    = <<"\"\\u0061\\u0062\\u0063\"">>,
     Expected2 = <<"abc">>,
     ?assertEqual(Expected2, jason:decode(Input2)),

     %% other multi-byte characters
     Input3    = <<"\"\\u06DD\\u06DE\\u10AE\\u10AF\"">>,
     Expected3 = "۝۞ႮႯ",
     ?assertEqual(Expected3, unicode:characters_to_list(jason:decode(Input3))),

     %% mixture of ascii and japanese characters
     Input4    = <<"\"a\\u30421\\u3044bb\\u304622\\u3048ccc\\u304A333\"">>,
     Expected4 = "aあ1いbbう22えcccお333",  % assumed that the encoding of this file is UTF-8
     ?assertEqual(Expected4, unicode:characters_to_list(jason:decode(Input4))),
     % string: surrogate pairs
     %Input5    = <<"\"\\ud848\\udc49\\ud848\\udc9a\\ud848\\udcfc\"">>,
     %Expected5 = "𢁉𢂚𢃼",
     %?assertEqual(Expected5, jason:decode(Input5)) ,

      % string: invalid escape characters",
     ok.

jason_decode_arrays_test() -> 
     %% Arrays
     % simple array",
     Input6    = <<"[1,2,\"abc def\",null]">>,
     Expected6 = [1, 2, <<"abc def">>, null],
     ?assertEqual(Expected6, jason:decode(Input6)),
     % array: contains whitespaces
     Input7    = <<"[  1,\t2, \n \"abc def\",\r null]">>,
     Expected7 = [1, 2, <<"abc def">>, null],
     ?assertEqual(Expected7, jason:decode(Input7)),
     % empty array
     ?assertEqual([], jason:decode(<<"[]">>)),
     ?assertEqual([], jason:decode(<<"[ \t\r\n]">>)),
     % array: trailing comma is disallowed
     Input8 = <<"[1, 2, \"abc def\", null, ]">>,
     ?assertMatch({error, _, _}, jason:decode(Input8)),
     % array: missing comma
     Input9 = <<"[1 2, \"abc def\", null]">>, % a missing comma between '1' and '2'
     ?assertMatch({error, _, _}, jason:decode(Input9)),
     % array: missing closing bracket
     Input10 = <<"[1, 2, \"abc def\", null">>,
     ?assertMatch({error, _, _}, jason:decode(Input10)),
     ok.

jason_decode_objects_test() -> 
     %% Objects
     % simple object -> Record TODO

     % simple object -> Struct
     Input11   = <<"{\"1\":2,\"key\":\"value\"}">>,
     Expected11 = {struct,[{<<"1">>,2},{<<"key">>,<<"value">>}]},
     ?assertEqual(Expected11, jason:decode(Input11, [])),
     % empty object
     ?assertEqual({struct,[]}, jason:decode(<<"{}">>, [])),
     ?assertEqual({struct,[]}, jason:decode(<<"{}">>, [])),
     % simple object -> map
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
     ?assertMatch({error, _, _} , jason:decode(Input17)),
     ok.
     

