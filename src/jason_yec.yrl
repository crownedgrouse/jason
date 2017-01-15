Nonterminals json array values object members member number integer int float frac exp value literal string chars.

Terminals 'b-a' 'e-a' 'v-s' 'b-o' 'e-o'
          'n-s' digits minus plus 'd-p' 'chr' exponent
          true false null .

Rootsymbol json.

json -> array   : '$1' .
json -> object  : '$1' .
json -> number  : '$1' .
json -> string  : '$1' .
json -> literal : '$1' .

array  -> 'b-a' 'e-a' : [].
array  -> 'b-a' values 'e-a' : '$2'.

values -> value : ['$1'].
values -> value 'v-s' values : ['$1'] ++ '$3' .

object -> 'b-o' 'e-o' : [].
object -> 'b-o' members 'e-o' : case get(jason_mode) of
                                    record   -> recordify('$2');
                                    map      -> mapify('$2');
                                    proplist -> proplistify('$2');
                                    _        -> '$2'
                                end.

members -> member : ['$1'].
members -> member 'v-s' members : ['$1'] ++ '$3'.

member -> string 'n-s' value : {'$1', '$3'}.

number -> integer exp : list_to_float('$1' ++ ".0" ++ '$2').
number -> float exp : list_to_float('$1' ++ '$2').
number -> integer : list_to_integer('$1').
number -> float : list_to_float('$1').

integer -> int : '$1'.
integer -> minus int : "-" ++ '$2' .

int -> digits : {_, _, C} = '$1', C.

float -> minus int frac : "-" ++ '$2' ++ '$3'.
float -> int frac : '$1' ++ '$2'.

frac -> 'd-p' digits : {_, _, C} = '$2', "." ++ C.

exp -> exponent minus digits : {_, _, C} = '$3', "e-" ++ C.
exp -> exponent plus  digits : {_, _, C} = '$3', "e" ++ C.
exp -> exponent digits : {_, _, C} = '$2', "e" ++ C.

string ->  chars  : '$1'.

chars -> 'chr' : {_, _, C} = '$1', C.
chars -> 'chr' chars : {_, _, C} = '$1', C ++ ['$2'].

value -> literal : '$1'.
value -> object  : '$1'.
value -> array   : '$1'.
value -> number  : '$1'.
value -> string  : '$1'.

literal -> true  : true.
literal -> false : false.
literal -> null  : null.

Erlang code.

%% MAPS %%
mapify(Obj) when is_list(Obj) -> 
          {_, M} = lists:mapfoldl(fun(X, Acc) -> case X of
                                                   {K, V} -> {{K, V}, Acc#{list_to_atom(binary_to_list(K)) => mapify(V)}};
                                                   Z      -> {Z, Acc}
                                                  end
                                  end , #{}, Obj),
          M;
mapify({K, V}) when is_list(V) -> #{list_to_atom(binary_to_list(K)) => mapify(V)};
mapify({K, V}) -> #{list_to_atom(binary_to_list(K)) => cast(V)};
mapify(X) -> cast(X).

%% RECORDS %%
recordify(Obj) -> % Replace binary keys by atom key, and detect values types
                  R = lists:flatmap(fun({K, V}) -> [{list_to_atom(binary_to_list(K)), cast(V)}] end, Obj),
                  T = lists:flatmap(fun({K, V}) -> [{K, detect_type(V)}] end, R),
                  % Hash Erlang term for ad hoc record name
                  H = list_to_atom(integer_to_list(erlang:phash2(T))),
                  % Create module for this record handling if not existing
                  case get(jason_adhoc) of
                       undefined -> put(jason_adhoc, []);
                       _         -> ok
                  end,
                  case lists:any(fun(X) -> case X of H -> true; _ -> false end end, get(jason_adhoc)) of
                      true  -> ok ;
                      false -> create_module(H, T)
                  end,
                  % Create record
                  V = lists:flatmap(fun({_, Z}) -> [Z] end, R),
                  erlang:list_to_tuple([H] ++ V).

detect_type(V) when is_atom(V)    -> literal ;
detect_type(V) when is_float(V)   -> float ;
detect_type(V) when is_integer(V) -> integer ;
detect_type(V) when is_list(V)    -> list;
detect_type(V) when is_tuple(V)   -> {record, element(1, V)}.


create_module(H, T) -> 
      % Module declaration
      M1 = parse_forms(io_lib:format("-module(~p).", [H])),
      {Ks, _Ts} = lists:unzip(T),

      % Functions export
      M2 = parse_forms(io_lib:format("-export([new/0, ~ts]).", 
                        [string:join(lists:flatmap(fun(K) -> [io_lib:format("~p/1,~p/2", [K,K])] end, Ks), ", ")])),

      % Json types definition
      M3 = parse_forms(io_lib:format("-type literal() :: null | true | false .",[])),

      % Record definition
      M40 = parse_forms(io_lib:format("-record(~p, {~ts}).", 
                        [H,
                         string:join(lists:flatmap(fun({K, V}) -> 
                           Def  =    case V of
                                          {record, _} -> "" ;
                                          integer -> " = 0 " ;
                                          float   -> " = 0.0 " ;
                                          list    -> " = [] " ;
                                          literal -> " = null "
                                     end,
                           Type =    case V of
                                          {record, A} -> "'" ++  atom_to_list(A) ++ "':'" ++ atom_to_list(A) ++ "'" ;
                                          V when is_atom(V) -> atom_to_list(V)
                                     end,
                           [io_lib:format("~p ~s :: ~s()", [K, Def, Type])] 
                                                   end, T), ", ")])),

      M41 = parse_forms(io_lib:format("-opaque ~p() :: #~p{}.", [H, H])),
      M42 = parse_forms(io_lib:format("-export_type([~p/0]).", [H])),

      % Function definitions
      M50 = parse_forms(io_lib:format("new() -> #~p{}.", [H])),

      M51 = lists:flatmap(fun({K, _}) -> 
                           [parse_forms(io_lib:format("~p(#~p{~p = X}) -> X.", [K, H, K])),
                            parse_forms(io_lib:format("~p(R, V) when is_record(R, ~p) -> R#~p{~p = V}.",
                                                      [K, H, H, K]))] end, T),
      % Compile forms
      {ok, _, Binary} = compile:forms(lists:flatten([M1,M2,M3,M40,M41,M42,M50,M51])),

      % Load module
      case code:load_binary(H, atom_to_list(H), Binary) of
         {module, _}    -> put(jason_adhoc, lists:flatten(get(jason_adhoc) ++ [H])) ;
         {error, _What} -> ok
      end.


parse_forms(C) -> 
         Code = lists:flatten(C),
         case erl_scan:string(lists:flatten(Code)) of
              {ok, S, _} ->  case erl_parse:parse_form(S) of
                                 {ok, PF}    -> PF ;
                                 {error, Ei} -> erlang:display({parse_error, Ei, io_lib:format("~ts",[Code])}), false
                             end;
              {error, EI, EL} -> erlang:display({scan_error, EI, EL, io_lib:format("~ts",[Code])}), false 
         end.

%% PROPLIST %%
proplistify(R) when is_list(R) -> lists:flatmap(fun({K, V}) -> [{list_to_atom(binary_to_list(K)), cast(V)}] end, R).
          
%% General %%
cast(V) when is_binary(V) -> erlang:binary_to_list(V) ;
cast(V)                   -> V . 

 

