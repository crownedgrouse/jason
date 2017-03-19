-module(jason_tests).

-include_lib("eunit/include/eunit.hrl").




jason_decode_literals_test() -> 
     %% Literals
     ?assertEqual({ok, false}, jason:decode(<<"false">>)),
     ?assertEqual({ok, true}, jason:decode(<<"true">>)),
     ?assertEqual({ok, null} , jason:decode(<<"null">>)).

jason_decode_numbers_test() -> 
     %% Numbers: Integer
     % positive integer
      ?assertEqual({ok, 1}, jason:decode(<<"1">>)),
     % zero 
      ?assertEqual({ok, 0}, jason:decode(<<"0">>)),
     % negative integer
      ?assertEqual({ok, -1}, jason:decode(<<"-1">>)),
     % large integer (no limit on size)
      ?assertEqual({ok, 111111111111111111111111111111111111111111111111111111111111111111111111111111},
        jason:decode(<<"111111111111111111111111111111111111111111111111111111111111111111111111111111">>)),
     % integer with leading zero
      ?assertEqual({ok, 0} , jason:decode(<<"00">>)),
      ?assertEqual({ok, 1}, jason:decode(<<"01">>)),
      ?assertEqual({ok, -1}, jason:decode(<<"-01">>)),
     % integer can't begin with an explicit plus sign",
     % TODO ?assertMatch({error, {badarg, _}}, jason:decode(<<"+1">>))

     %% Numbers: Floats
     % float: decimal notation",
     ?assertEqual({ok, 1.23}, jason:decode(<<"1.23">>)),
     ?assertEqual({ok, 1.23456789}, jason:decode(<<"1.23456789">>)),
     % float: exponential notation",
     ?assertEqual({ok, 12.345}, jason:decode(<<"12345e-3">>)), % lower case 'e'
     ?assertEqual({ok, 12.345}, jason:decode(<<"12345E-3">>)), % upper case 'E'
     ?assertEqual({ok, 12.345}, jason:decode(<<"12345.0e-3">>)),
     ?assertEqual({ok, 12.345}, jason:decode(<<"0.12345E2">>)),
     ?assertEqual({ok, 12.345}, jason:decode(<<"0.12345e+2">>)), % exponent part can begin with plus sign
     ?assertEqual({ok, 12.345}, jason:decode(<<"0.12345E+2">>)),
     ?assertEqual({ok, -12.345}, jason:decode(<<"-0.012345e3">>)),
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
     ?assertEqual({ok, <<"abc def">>}, jason:decode(<<"\"abc def\"">>)),
     ?assertEqual({ok, <<"abc def">>}, jason:decode("\"abc def\"")),
     ?assertEqual({ok, <<" abc def ">>}, jason:decode('" abc def "')),
     % string: escaped characters",
     Input    = list_to_binary([$", [[$\\, C] || C <- [$", $/, $\\, $b, $f, $n, $r, $t]], $"]),
     Expected = {ok, <<"\"\/\\\b\f\n\r\t">>},
     ?assertEqual(Expected, jason:decode(Input)),
     % string: escaped Unicode characters",
     %% japanese
     Input1    = <<"\"\\u3042\\u3044\\u3046\\u3048\\u304A\"">>,
     Expected1 = [12354,12356,12358,12360,12362],  %  <<"あいうえお">> UTF-8
     {ok, I1} = jason:decode(Input1),
     ?assertEqual(Expected1, unicode:characters_to_list(I1)),

     %% ascii
     Input2    = <<"\"\\u0061\\u0062\\u0063\"">>,
     Expected2 = {ok, <<"abc">>},
     ?assertEqual(Expected2, jason:decode(Input2)),

     %% other multi-byte characters
     Input3    = <<"\"\\u06DD\\u06DE\\u10AE\\u10AF\"">>,
     Expected3 = [1757,1758,4270,4271], % <<"۝۞ႮႯ">>
     {ok, I3} = jason:decode(Input3),
     ?assertEqual(Expected3, unicode:characters_to_list(I3)),

     %% mixture of ascii and japanese characters
     Input4    = <<"\"a\\u30421\\u3044bb\\u304622\\u3048ccc\\u304A333\"">>,
     Expected4 = [97,12354,49,12356,98,98,12358,50,50,12360,99,99,99,12362,
 51,51,51],  % <<"aあ1いbbう22えcccお333">> UTF-8
     {ok, I4} = jason:decode(Input4),
     ?assertEqual(Expected4, unicode:characters_to_list(I4)),
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
     Expected6 = {ok, [1, 2, <<"abc def">>, null]},
     ?assertEqual(Expected6, jason:decode(Input6)),
     % array: contains whitespaces
     Input7    = <<"[  1,\t2, \n \"abc def\",\r null]">>,
     Expected7 = {ok, [1, 2, <<"abc def">>, null]},
     ?assertEqual(Expected7, jason:decode(Input7)),
     % empty array
     ?assertEqual({ok, []}, jason:decode(<<"[]">>)),
     ?assertEqual({ok, []}, jason:decode(<<"[ \t\r\n]">>)),
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
     Expected11 = {ok, [{<<"1">>,2},{<<"key">>,<<"value">>}]},
     ?assertEqual(Expected11, jason:decode(Input11, [])),
     % empty object
     ?assertEqual({ok, []}, jason:decode(<<"{}">>, [])),
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
     

