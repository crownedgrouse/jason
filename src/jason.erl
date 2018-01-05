%%%-----------------------------------------------------------------------------
%%% File:      jason.erl
%%% @author    Eric Pailleau <jason@crownedgrouse.com>
%%% @copyright 2017 crownedgrouse.com
%%% @doc
%%% JSON encode/decode Erlang library
%%% @end
%%%
%%% Permission to use, copy, modify, and/or distribute this software
%%% for any purpose with or without fee is hereby granted, provided
%%% that the above copyright notice and this permission notice appear
%%% in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
%%% WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
%%% AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
%%% CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
%%% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
%%% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% Created : 2017-01-08
%%%-----------------------------------------------------------------------------
-module(jason).

-export([encode/1, decode/1]).
-export([encode/2, decode/2]).
-export([decode_file/1, decode_file/2]).
-export([encode_file/2, encode_file/3]).
-export([pp/1, pp/2, types/0]).

-record(opt, {nl      = []     :: list()
             ,indent  = false
             ,records = []     :: list()
             ,mode    = struct :: atom()
             }).

%%==============================================================================
%% @doc Pretty print JSON data
%% @end
-spec pp(list()) -> list().

pp(C) -> jason_pp:indent(C).

%%==============================================================================
%% @doc Pretty print JSON data
%% @end
-spec pp(_, maybe_improper_list() | {'pp',_,_,_,_,_}) -> [any()].

pp(C, Style) -> jason_pp:indent(C, Style).

%%==============================================================================
%% @doc Encode Erlang data to JSON file
%% @end
-spec encode_file(term(), list()) -> 'ok'.

encode_file(Term, Target) when is_list(Target) -> encode_file(Term, Target, []).

%%==============================================================================
%% @doc Encode Erlang data to JSON file with options
%% @end
-spec encode_file(any, list(), list()) -> atom().

encode_file(Term, Target, Opt)
   when is_list(Target) ->
            file:write_file(Target, encode(Term, Opt)).

%%==============================================================================
%% @doc Encode Erlang data to JSON
%% @end
-spec encode(any()) -> list().

encode(Term) -> encode(Term, []).

%%==============================================================================
%% @doc Encode Erlang data to JSON with options
%% @end
-spec encode(any(), list()) -> list().

encode(Term, O) ->
   Opt = options(O),
   Compact = lists:flatten(encode(Term, Opt, left, 0)),
   case Opt#opt.indent of
      ""     -> Compact ;
      I      -> pp(Compact, I) % TODO
   end.
% MAP
encode(Term, Opt, Side, Depth)
      when is_map(Term) ->  encode(maps:to_list(Term), Opt#opt{mode = map}, Side, Depth) ;
% TUPLE left
encode({L, R}, Opt, left, Depth)
           when is_atom(L),
(Opt#opt.mode =/= 'record')
         -> "{" ++ encode(L, Opt, left, Depth) ++ ": " ++ encode(R, Opt, right, (Depth + 1)) ++ "}";
% TUPLE right
encode({L, R}, Opt, right, Depth)
           when is_atom(L),
(Opt#opt.mode =/= 'record')
         -> "{" ++ encode(L, Opt, left,  Depth) ++ ": " ++ encode(R, Opt, right, Depth) ++ "}";
% TUPLE
encode(Term, Opt, Side, Depth)
   when is_tuple(Term),
         (Opt#opt.mode =:= 'record')  ->
      % Record ?
      Name = element(1, Term),
      % Check if a definition was given in option
      case check_rec_def(Name, Opt#opt.records) of
            false -> % If 2 elements tuple, encode anyway as proplist
                     case Term of
                        {_, _} -> encode(Term, Opt#opt{mode=proplist}, Side, Depth);
                        _      -> throw({'unable_to_encode', Name, Depth})
                     end;
            Def   -> X = lists:foldl( fun({A,B}, Acc) ->
                                       Acc ++ [encode(A, Opt, left, Depth) ++ ": " ++ encode(B, Opt,right, (Depth + 1))]
                                       end, [], record2object(Term, Def)),
                     "{" ++ string:join(X, ",") ++ "}"
      end;
% LIST
encode([{_, _}| _] = Term, Opt, _, Depth)
   when is_list(Term),
   ((Opt#opt.mode =:= proplist) or
   (Opt#opt.mode =:= map) or
   (Opt#opt.mode =:= struct)) ->
            X = lists:foldl( fun({A,B}, Acc) ->
                              Acc ++ [encode(A, Opt, left, Depth) ++ ": " ++ encode(B, Opt,right, (Depth + 1))]
                              end, [], Term),
            "{" ++ string:join(X, ",") ++ "}"
         ;
encode(Term, Opt, Side, Depth)
   when is_tuple(Term),
   (tuple_size(Term) > 1) ->
         case element(1, Term) of
               X when is_atom(X) -> encode(Term, Opt#opt{mode='record'}, Side, Depth);
               _ -> encode([Term], Opt, Side, Depth)
         end;
encode({L}, Opt, Side, Depth)
   when is_list(L) -> encode(L, Opt, Side, Depth) ;
encode({}, _Opt, _Side, _Depth) -> "{}" ;
encode(Term, _Opt, _Side, Depth)
   when is_tuple(Term)     -> throw({'invalid_term', Term, Depth}) ;

% INTEGER
encode(Term, _Opt, _, _Depth)
   when is_integer(Term) -> integer_to_list(Term) ;
% FLOAT
encode(Term, _Opt, _, _Depth)
   when is_float(Term) ->  Precision = get_precision(Term),
                           [float_to_list(Term, [{decimals, Precision}, compact])] ;
% BINARY
encode(Term, _Opt, _, _Depth)
   when is_binary(Term) -> "\"" ++ binary_to_list(Term) ++ "\"";
% ATOMS
encode(null, _Opt, _, _Depth)      -> io_lib:format("null", []) ;
encode(undefined, _Opt, _, _Depth) -> io_lib:format("null", []) ;
encode(true, _Opt, _, _Depth)      -> io_lib:format("true", []) ;
encode(false, _Opt, _, _Depth)     -> io_lib:format("false", []) ;
encode(Term, _Opt, _, _Depth)
   when is_atom(Term) -> "\"" ++ atom_to_list(Term) ++ "\"";
% LIST
encode(Term, Opt, _, Depth)
   when is_list(Term) ->
      case io_lib:printable_unicode_list(Term) of
               false -> A = lists:foldl(fun(X, Acc) ->
                                       Acc ++ [encode(X, Opt, right, (Depth + 1))]
                                       end, [], Term),
                        "[" ++ string:join(A, ",") ++ "]" ;
               true  -> "\"" ++ Term ++ "\""
      end.


%%==============================================================================
%% @doc Decode JSON data
%% @end
-spec decode(any()) -> tuple().

decode(Json) -> decode(Json, []).

%%==============================================================================
%% @doc Decode JSON data with options
%% @end
-spec decode(any(), list()) -> tuple().

decode(Json, Opt) when is_atom(Json)   -> decode(atom_to_list(Json), Opt);
decode(Json, Opt) when is_binary(Json) -> decode(binary_to_list(Json), Opt);
decode(Json, Opt) when is_list(Json)   ->
   try
      O = options(Opt),
      put(jason_records, O#opt.records),
      {ok, X, _} = jason_lex:string(Json),
      Mode = proplists:get_value(mode, Opt),
      put(jason_mode, Mode),
      To = proplists:get_value(to, Opt),
      case valid_to_file(To) of
            skip        -> ok ;
            true        -> put(jason_to, To) ;
            false       -> throw({error, "Invalid 'to' record definition dump file : cannot create"});
            notempty    -> throw({error, "Invalid 'to' record definition dump file : not empty"})
      end,
      {ok, R}  = jason_yec:parse(X),
      case proplists:get_value(return, Opt) of
         tuple -> {ok, R};
         _     -> R
      end
   catch
      throw:Term   ->  Term ;
      error:Reason ->  Err = case Reason of
                                 {badmatch,{error,{Line,_,["syntax error before: ", []]}}} ->
                                       {Line, lists:flatten(io_lib:format("syntax error before end", []))} ;
                                 {badmatch,{error,{Line,_,["syntax error before: ", [What]]}}} ->
                                       {Line, lists:flatten(io_lib:format("syntax error before: ~ts", [What]))} ;
                                 {badmatch,{error,{Line,jason_lex,{user,What}}, _}} ->
                                       {Line, lists:flatten(io_lib:format("~ts", [What]))};
                                 _ ->  Reason
                              end,
                        case proplists:get_value(return, Opt) of
                           tuple -> {error, Err};
                           _     -> throw(Err)
                        end
   after
      erase(jason_mode),
      case get(jason_to) of
         undefined -> ok ;
         _         -> erase(jason_adhoc)
      end,
      erase(jason_to)
   end.

%%==============================================================================
%% @doc Decode JSON file
%% @end
-spec decode_file(list()) -> tuple().

decode_file(F) when is_list(F) -> decode_file(F, []).

%%==============================================================================
%% @doc Decode JSON file with options
%% @end
-spec decode_file(list(), list()) -> tuple().

decode_file(F, Opt) when is_list(F) ->
   try
      % Use raw reading, and use real error from read_file otherwise
      B = case erl_prim_loader:get_file(F) of
               error -> {ok, BA} = file:read_file(F), BA;
               {ok, BB, _} -> BB
          end,
      jason:decode(B, Opt)
   catch
      throw:Term   -> Term ;
      error:Reason -> Err = case Reason of
                                 {badmatch,{error,X}} -> X;
                                 _ -> Reason
                            end,
                      case proplists:get_value(return, Opt) of
                        tuple -> {error, Err};
                        _     -> throw(Err)
                      end
   end.

%%==============================================================================
%% @doc
%% @end
%decode_stream(F) when is_pid(F) -> decode_stream(F, [{chunks, 2048]).

%%==============================================================================
%% @doc
%% @end
%decode_stream(F, Opt) when is_pid(F) ->

%%==============================================================================
%% @doc Check if record definition was given in options
%% @end
-spec check_rec_def(atom(), list()) -> atom() | list().

check_rec_def(Name, Opt) ->
   case Opt of
      []    -> false ;
      Recs  -> case proplists:get_value(Name, Recs) of
                     undefined -> false ;
                     Res       -> Res
               end
   end.

%%==============================================================================
%% @doc Translate a record to JSON object
%% @end
-spec record2object(tuple(), list()) -> list().

record2object(Term, Def)
   when is_tuple(Term) -> [_ | T] = erlang:tuple_to_list(Term),
                          lists:zip(Def, T).

%%==============================================================================
%% @doc Check if target file is valid (no exist or empty), upper dir. exists
%% @end
-spec valid_to_file(list()) -> atom().

valid_to_file(To) when is_list(To) ->
   case filelib:is_file(To) of
      false -> case filelib:is_dir(filename:dirname(To)) of
                  true  -> true ;
                  false -> false
               end;
      true  -> case filelib:is_regular(To) of
                  true -> case filelib:file_size(To) of
                              0 -> true ;
                              _ -> notempty
                           end
               end
   end;

valid_to_file(_) -> skip.


%%==============================================================================
%% @doc Analyze options
%% @end
-spec options(list()) -> tuple().

options(O) ->
   I = case proplists:get_value(indent, O) of
            undefined -> "" ;
            false -> "" ;
            true  -> "   "
       end,
   N = case proplists:get_value(indent, O) of
            undefined -> "" ;
            false -> "" ;
            true  -> "\n"
       end,
   R = case proplists:get_value(records, O) of
            undefined -> [] ;
            X  when is_atom(X)   -> get_records([X]);
            X  when is_tuple(X) -> get_records([X]);
            X  when is_list(X)  -> get_records(X);
            _  -> throw({0, {invalid, records}})
       end,
   M = case proplists:get_value(mode, O) of
            map      -> map ;
            proplist -> proplist ;
            'record' -> 'record' ;
            _        -> struct
       end,
   #opt{nl = N
      ,indent = I
      ,records = lists:flatten(R)
      ,mode = M
      }.
%%==============================================================================
%% @doc Get records info
%% @end
get_records(X) when is_list(X)
                    -> lists:flatmap(fun(E) -> [get_records(E)] end, X) ;

get_records(X) when is_tuple(X),
                    is_atom(element(1, X)),
                    is_list(element(2, X))
                    -> % Check all entries are atoms
                       case lists:all(fun(A) -> case is_atom(A) of true -> true ; _ -> false end end, element(2, X)) of
                           true  -> X ;
                           false -> throw({0, {invalid, records}}),
                                    []
                       end;

get_records(X) when is_atom(X)
                    -> % Extract record info from pid
                       Beam = case code:which(X) of
                                 cover_compiled -> "";
                                 preloaded      -> "";
                                 non_existing   -> "";
                                 B              -> B
                              end,
                       case beam_lib:chunks( Beam,[abstract_code]) of
                           {ok,{_, [{abstract_code, {_, Abs }}]}}
                               -> extract_records_ac(Abs);
                           _   -> throw( {0, {invalid, abstract_code}})

                       end;

get_records(_) -> throw( {0, {invalid, records}}).



%   [{attribute,1,file,{"src/jason_pp.erl",1}},
%   {attribute,26,module,jason_pp},
%   {attribute,28,export,[{indent,1},{indent,2}]},
%   {attribute,30,record,
%         {pp,[{record_field,30,{atom,30,style},{atom,30,ratliff}},
%            {record_field,31,{atom,31,depth},{integer,31,0}},
%            {record_field,32,{atom,32,nl},{string,32,[...]}},
%            {record_field,33,{atom,33,tab},{string,33,...}},
%            {record_field,34,{atom,34,...},{atom,...}}]}},
%   {function,38,indent,1,

extract_records_ac(Abs) ->
   Raw = lists:filter(fun(E) ->  case (is_tuple(E) and (size(E) > 2)) of
                                     true -> case element(3, E) of
                                                  record -> true ;
                                                  _ -> false
                                             end;
                                      false -> false
                                 end
                                 end, Abs),
   lists:flatmap(fun({attribute, _, record, {N, FF}}) ->
                     F = lists:flatmap(fun(R) -> case element(1, R) of
                                                      record_field -> {atom, _, RA} = element(3, R),
                                                                      [RA];
                                                      typed_record_field -> {typed_record_field, W, _} = R,
                                                                            {atom, _, RT} = element(3, W),
                                                                             [RT]
                                                 end
                                       end, FF),
                     [{N, F}]
                  end, Raw).

%%==============================================================================
%% @doc Get precision of float
%% @end
-spec get_precision(float()) -> integer().

get_precision(F) when is_float(F) ->
   case F < 0 of
      false -> get_precision(F, lfloor(F), 1);
      true  -> get_precision(-F, lfloor(-F), 1)
   end.

get_precision(F, I, P) when is_float(F) ->
   case F == I of
      true  -> P ;
      false -> get_precision(F * 10, lfloor(F * 10), P+1)
   end.

%%==============================================================================
%% @doc floor as local function since erlang:floor/1 not available in older release
%% @end
lfloor(X) ->
    T = trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.
%%==============================================================================
%% @doc Display types transformation
%% @end
-spec types() -> ok.

types() ->
io:format("Erlang                       JSON                      Erlang~n"),
io:format("================================================================================~n"),
% Null
Null = null,
NUll = jason:encode(Null),
{ok, NULl} = jason:decode(NUll, [{return, tuple}]),
io:format("~n%% Atoms~n~p\t\t\t-> ~s \t\t-> ~p~n",[Null, NUll, NULl]),
% Undefined
Undef = undefined,
UNdef = jason:encode(Undef),
{ok, UNDef} = jason:decode(UNdef, [{return, tuple}]),
io:format("~p\t\t-> ~s \t\t-> ~p~n",[Undef, UNdef, UNDef]),
% True
True = true,
TRue = jason:encode(True),
{ok, TRUe} = jason:decode(TRue, [{return, tuple}]),
io:format("~p\t\t\t-> ~s \t\t-> ~p~n",[True, TRue, TRUe]),
% False
False = false,
FAlse = jason:encode(False),
{ok, FALse} = jason:decode(FAlse, [{return, tuple}]),
io:format("~p\t\t\t-> ~s \t\t-> ~p~n",[False, FAlse, FALse]),
% Other atom
Any = any,
ANy = jason:encode(Any),
{ok, ANY} = jason:decode(ANy, [{return, tuple}]),
io:format("~p\t\t\t-> ~s \t\t-> ~p~n",[Any, ANy, ANY]),

% Integer
Int = 123,
INt = jason:encode(Int),
{ok, INT} = jason:decode(INt, [{return, tuple}]),
io:format("~n%% Integer~n~p\t\t\t-> ~s \t\t\t-> ~p~n",[Int, INt, INT]),
% Float
Float = 123.456789,
FLoat = jason:encode(Float),
{ok, FLOat} = jason:decode(FLoat, [{return, tuple}]),
io:format("~n%% Float (Automatic precision)~n~p\t\t-> ~s \t\t-> ~p~n",[Float, FLoat, FLOat]),

Float2 = 2.30e+0,
FLoat2 = jason:encode(Float2),
{ok, FLOat2} = jason:decode(FLoat2, [{return, tuple}]),
io:format("2.30e+0\t\t\t-> ~s \t\t\t-> ~p~n",[FLoat2, FLOat2]),

Float3 = 2.30e+3,
FLoat3 = jason:encode(Float3),
{ok, FLOat3} = jason:decode(FLoat3, [{return, tuple}]),
io:format("2.30e+3\t\t\t-> ~s \t\t-> ~p~n",[FLoat3, FLOat3]),

Float4 = 2.30e-3,
FLoat4 = jason:encode(Float4),
{ok, FLOat4} = jason:decode(FLoat4, [{return, tuple}]),
io:format("2.30e-3\t\t\t-> ~s \t\t-> ~p~n",[FLoat4, FLOat4]),

% List
List = [1,2,3],
LIst = jason:encode(List),
{ok, LISt} = jason:decode(LIst, [{return, tuple}]),
io:format("~n%% List~n~p\t\t\t-> ~s \t\t-> ~p ~n",[List, LIst, LISt]),

List2 = ['a',"b",<<"c">>],
LIst2 = jason:encode(List2),
{ok, LISt2} = jason:decode(LIst2, [{return, tuple}]),
io:format("~p\t\t-> ~s \t-> ~p ~n",[List2, LIst2, LISt2]),
% Binary
Bin1 = <<"abc">>,
BIn1 = jason:encode(Bin1),
{ok, [{_, BIN1}]} = jason:decode("{ \"key\":"++  BIn1 ++ "}", [{return, tuple}]),
io:format("~n%% Binary (key/value) mode=struct (default)~n~p\t\t-> ~s \t\t-> ~p~n",[Bin1, BIn1, BIN1]),

% STRUCT
Bin2s = {<<"abc">>,<<"def">>},
BIn2s = jason:encode(Bin2s),
{ok, BIN2s} = jason:decode(BIn2s, [{mode, struct}, {return, tuple}]),
io:format("~n%% Struct~n%  mode=struct (default)~n~p\t-> ~s \t-> ~p~n",[Bin2s, BIn2s, BIN2s]),

Bin3s = {<<"abc">>,<<"def">>},
BIn3s = jason:encode(Bin3s),
{ok, BIN3s} = jason:decode(BIn3s, [{mode, proplist}, {return, tuple}]),
io:format("%  mode=proplist~n~p\t-> ~s \t-> ~p~n",[Bin3s, BIn3s, BIN3s]),


Bin4s = {<<"abc">>,<<"def">>},
BIn4s = jason:encode(Bin4s),
{ok, BIN4s} = jason:decode(BIn4s, [{mode, map}, {return, tuple}]),
io:format("%  mode=map~n~p\t-> ~s \t-> ~p~n",[Bin4s, BIn4s, BIN4s]),

Bin5s = {<<"abc">>,<<"def">>},
BIn5s = jason:encode(Bin5s),
{ok, {Rs, BIN5s}} = jason:decode(BIn5s, [{mode, 'record'}, {return, tuple}]),
io:format("%  mode=record~n~p\t-> ~s \t-> ~p ~n",[Bin5s, BIn5s, {Rs, BIN5s}]),
io:format("\t\t\t\t\t\twith ~s~n",[Rs:def()]),

% PROPLIST
Bin2p = [{abc,<<"def">>}],
BIn2p = jason:encode(Bin2p),
{ok, BIN2p} = jason:decode(BIn2p, [{mode, struct}, {return, tuple}]),
io:format("~n%% Proplist~n%  mode=struct (default)~n~p\t-> ~s \t-> ~p~n",[Bin2p, BIn2p, BIN2p]),

Bin3p = [{abc,<<"def">>}],
BIn3p = jason:encode(Bin3p),
{ok, BIN3p} = jason:decode(BIn3p, [{mode, proplist}, {return, tuple}]),
io:format("%  mode=proplist~n~p\t-> ~s \t-> ~p~n",[Bin3p, BIn3p, BIN3p]),


Bin4p = [{abc,<<"def">>}],
BIn4p = jason:encode(Bin4p),
{ok, BIN4p} = jason:decode(BIn4p, [{mode, map}, {return, tuple}]),
io:format("%  mode=map~n~p\t-> ~s \t-> ~p~n",[Bin4p, BIn4p, BIN4p]),

Bin5p = [{abc,<<"def">>}],
BIn5p = jason:encode(Bin5p),
{ok, {Rp, BIN5p}} = jason:decode(BIn5p, [{mode, 'record'}, {return, tuple}]),
io:format("%  mode=record~n~p\t-> ~s \t-> ~p ~n",[Bin5p, BIn5p, {Rp, BIN5p}]),
io:format("\t\t\t\t\t\twith ~s~n",[Rp:def()]),

% MAP
Bin2m = #{"abc" => <<"def">>},
BIn2m = jason:encode(Bin2m),
{ok, BIN2m} = jason:decode(BIn2m, [{mode, struct}, {return, tuple}]),
io:format("~n%% Map~n%  mode=struct (default)~n~p\t-> ~s \t-> ~p~n",[Bin2m, BIn2m, BIN2m]),

Bin3m = #{"abc" => <<"def">>},
BIn3m = jason:encode(Bin3m),
{ok, BIN3m} = jason:decode(BIn3m, [{mode, proplist}, {return, tuple}]),
io:format("%  mode=proplist~n~p\t-> ~s \t-> ~p~n",[Bin3m, BIn3m, BIN3m]),


Bin4m = #{"abc" => <<"def">>},
BIn4m = jason:encode(Bin4m),
{ok, BIN4m} = jason:decode(BIn4m, [{mode, map}, {return, tuple}]),
io:format("%  mode=map~n~p\t-> ~s \t-> ~p~n",[Bin4m, BIn4m, BIN4m]),

Bin5m = #{"abc" => <<"def">>},
BIn5m = jason:encode(Bin5m),
{ok, {Rm, BIN5m}} = jason:decode(BIn5m, [{mode, 'record'}, {return, tuple}]),
io:format("%  mode=record~n~p\t-> ~s \t-> ~p ~n",[Bin5m, BIn5m, {Rm, BIN5m}]),
io:format("\t\t\t\t\t\twith ~s~n",[Rm:def()]),

% RECORD
Bin2r = {'r', 1, <<"ab">>},
BIn2r = jason:encode(Bin2r, [{records, [{r, [k1,k2]}]}]),
{ok, BIN2r} = jason:decode(BIn2r, [{mode, struct}, {return, tuple}]),
io:format("~n%% Record - encoding using option [{records, [{r, record_info(fields, r)}]}] or [{records, [{r, [k1,k2]}]}]~n%  mode=struct (default)~n~p\t\t-> ~s -> ~p~n",[Bin2r, BIn2r, BIN2r]),

Bin3r = {'r', 1, <<"ab">>},
BIn3r = jason:encode(Bin3r, [{records, [{r, [k1,k2]}]}]),
{ok, BIN3r} = jason:decode(BIn3r, [{mode, proplist}, {return, tuple}]),
io:format("%  mode=proplist~n~p\t\t-> ~s -> ~p~n",[Bin3r, BIn3r, BIN3r]),


Bin4r = {'r', 1, <<"ab">>},
BIn4r = jason:encode(Bin4r, [{records, [{r, [k1,k2]}]}]),
{ok, BIN4r} = jason:decode(BIn4r, [{mode, map}, {return, tuple}]),
io:format("%  mode=map~n~p\t\t-> ~s -> ~p~n",[Bin4r, BIn4r, BIN4r]),

Bin5r = {'r', 1, <<"ab">>},
BIn5r = jason:encode(Bin5r, [{records, [{r, [k1,k2]}]}]),
{ok, BIN5r} = jason:decode(BIn5r, [{mode, 'record'}, {return, tuple}]),
R5 = element(1, BIN5r),
io:format("%  mode=record~n~p\t\t-> ~s -> ~p ~n",[Bin5r, BIn5r, BIN5r]),
io:format("\t\t\t\t\t\twith ~s~n",[R5:def()]),

% TODO transcode record name
{ok, BIN6r} = jason:decode(BIn5r, [{mode, 'record'}, {records, [{r, [k1,k2]}]}, {return, tuple}]),
R6 = element(1, BIN6r),
io:format("~n%  mode=record - decoding using option [{records, [{r, [k1,k2]}]}]~n\t\t\t\t\t\t-> ~p ~n",[BIN6r]),
io:format("\t\t\t\t\t\twith ~s~n",[R6:def()])
.

