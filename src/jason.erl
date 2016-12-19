-module(jason).


-export([encode/1, decode/1]).
-export([encode/2, decode/2]).
-export([decode_file/1]).


encode(_Term) -> ok.
encode(_Term, _) -> ok.

decode(Json) -> decode(Json, []).

decode(Json, _Opt) when is_atom(Json)   -> decode(atom_to_list(Json));
decode(Json, _Opt) when is_binary(Json) -> decode(binary_to_list(Json));
decode(Json, _Opt) when is_list(Json)   -> 
      try
         {ok, X, _} = jason_lex:string(Json), %io:format("~p~n",[X]),
         {ok, Res}  = jason_yec:parse(X),
         Res
      catch 
         throw:Term -> Term ;
         error:Reason -> %io:format("~p~n",[Reason]),
                         case Reason of
                              {badmatch,{error,{Line,_,["syntax error before: ", []]}}} -> 
                                    {error, Line, lists:flatten(io_lib:format("syntax error before end", []))} ;
                              {badmatch,{error,{Line,_,["syntax error before: ", [What]]}}} -> 
                                    {error, Line, lists:flatten(io_lib:format("syntax error before: ~ts", [What]))} ;
                              _ ->  {error, Reason }
                         end
      end.


decode_file(F) when is_list(F) -> 
      try
         {ok, B} = file:read_file(F),
         jason:decode(B)
      catch
         throw:Term -> Term ;
         error:Reason -> case Reason of
                              {badmatch,{error,enoent}} -> {error, enoent};
                              _ -> {error, Reason} 
                         end
      end.

