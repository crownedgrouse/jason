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

%%==============================================================================
%% @doc 
%% @end
encode(Term) -> encode(Term, []).

encode({L, R}, Opt) -> ok = io:format("~ts: ~ts", [encode(L, Opt, left), encode(R, Opt, right)]);
encode(Term, Opt) -> ok = io:format("~ts", [encode(Term, Opt, left)]).

encode({L, R}, Opt, left)  -> io_lib:format("~ts: ~ts", [encode(L, Opt, left), encode(R, Opt, right)]) ;
encode({L, R}, Opt, right) -> io_lib:format("~ts: ~ts", [encode(L, Opt, left), encode(R, Opt, right)]) ;
encode(Term, Opt, left) 
		when is_tuple(Term)  -> % Record ?
										Name = element(1, Term),
										% Check if a definition was given in option
										case check_rec_def(Name, Opt) of
											  false -> throw({'unable_to_encode', Name}) ;
											_  -> ok
											  %Def   -> lists:foldl( fun() -> end, record2object(Term, Def))
										end;
encode(Term, _Opt, right)
		when is_float(Term) -> io_lib:format("~ts", [float_to_list(Term, [{decimals, 4}, compact])]) ;
encode(Term, _Opt, _)
		when is_binary(Term) -> io_lib:format("\"~ts\"", [binary_to_list(Term)]) ;
encode(Term, _Opt, _)
		when is_atom(Term) -> io_lib:format("\"~ts\"", [atom_to_list(Term)]) ;
encode(Term, Opt, left) 
      when is_list(Term) -> case io_lib:printable_unicode_list(Term) of
											 false -> io:format("[~n~ts~n]", [encode(Term, Opt, right)]) ;
											 true  -> io:format("\"~ts\"", [Term]) 
									  end;
encode(Term, Opt, right) 
		when is_list(Term) -> case io_lib:printable_unicode_list(Term) of
											 false -> A = lists:foldl(fun(X, Acc) -> Acc ++ [encode(X, Opt, right)] end, [], Term),
														 io_lib:format("{~n~ts~n}~n", [string:join(A, ",\n")]) ;
											 true  -> io_lib:format("\"~ts\"", [Term]) 
									  end.

%%==============================================================================
%% @doc 
%% @end
decode(Json) -> decode(Json, []).

%%==============================================================================
%% @doc 
%% @end
decode(Json, Opt) when is_atom(Json)   -> decode(atom_to_list(Json), Opt);
decode(Json, Opt) when is_binary(Json) -> decode(binary_to_list(Json), Opt);
decode(Json, Opt) when is_list(Json)   -> 
      try
         {ok, X, _} = jason_lex:string(Json), 
         Mode = proplists:get_value(mode, Opt),
         put(jason_mode, Mode),
			To = proplists:get_value(to, Opt),
			case valid_to_file(To) of
				 true        -> put(jason_to, To) ;
				 false       -> throw({error, "Invalid 'to' record definition dump file : cannot create"});
				 notempty    -> throw({error, "Invalid 'to' record definition dump file : not empty"}) 
			end,
         {ok, R}  = jason_yec:parse(X), 
         R
      catch 
         throw:Term -> Term ;
         error:Reason -> case Reason of
                              {badmatch,{error,{Line,_,["syntax error before: ", []]}}} -> 
                                    {error, Line, lists:flatten(io_lib:format("syntax error before end", []))} ;
                              {badmatch,{error,{Line,_,["syntax error before: ", [What]]}}} -> 
                                    {error, Line, lists:flatten(io_lib:format("syntax error before: ~ts", [What]))} ;
                              _ ->  {error, Reason }
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
%% @doc 
%% @end
decode_file(F) when is_list(F) -> decode_file(F, []).

%%==============================================================================
%% @doc 
%% @end
decode_file(F, Opt) when is_list(F) -> 
      try
         {ok, B} = file:read_file(F),
         jason:decode(B, Opt)
      catch
         throw:Term -> Term ;
         error:Reason -> case Reason of
                              {badmatch,{error,enoent}} -> {error, enoent};
                              _ -> {error, Reason} 
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

check_rec_def(Name, Opt) -> case proplists:get_value(records, Opt) of
											undefined -> false ;
											Recs      -> Def  = proplists:get_value(Name, Recs),
									 						 Def
									 end.


%record2object(Term, Def) 
%	when is_tuple(Term) -> [_ | T] = erlang:tuple_to_list(Term),
%								  lists:zip(Def, T).

valid_to_file(To) -> case filelib:is_file(To) of
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
						   end .
 

