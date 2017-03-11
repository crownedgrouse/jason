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

-record(opt, {nl      = []     :: list()
				 ,indent  = []     :: list()
             ,compact = false  :: boolean()
             ,records = []     :: list()
             ,mode    = struct :: atom()
				 }).

%%==============================================================================
%% @doc 
%% @end
encode_file(Term, Target) -> encode_file(Term, Target, []).

encode_file(Term, Target, Opt) 
	when is_list(Target) -> 
				file:write_file(Target, encode(Term, Opt)).

%%==============================================================================
%% @doc 
%% @end
encode(Term) -> encode(Term, []).

encode(Term, O) -> Opt = options(O),
						 lists:flatten(encode(Term, Opt, left, 0)).

encode(Term, Opt, Side, Depth)
		when is_map(Term) ->  encode(maps:to_list(Term), Opt, Side, Depth) ;
encode({L, R}, Opt, left, Depth) 
			 when is_atom(L) ->
			 case check_rec_def(L, Opt#opt.records) of
				  		false -> io_lib:format("~ts~ts: ~ts", 
													 [indent(Opt#opt.indent, Depth), 
								                 encode(L, Opt, left, Depth), 
								                 encode(R, Opt, right, (Depth + 1))]);
						_  -> io_lib:format("~ts~ts", 
								             [indent(Opt#opt.indent, Depth), 
								              encode(R, Opt, right, (Depth + 1))])
			 end;
encode({L, R}, Opt, right, Depth) 
			 when is_atom(L) ->
			 case check_rec_def(L, Opt#opt.records) of
		  		false -> io_lib:format("~ts: ~ts", 
				                      [encode(L, Opt, left,  Depth), 
				                       encode(R, Opt, right, Depth)]);
				_  -> io_lib:format("~ts", 
				                   [encode(R, Opt, right, Depth)])
			 end;
encode(Term, Opt, _, Depth) 
		when is_tuple(Term),
           (Opt#opt.mode =:= 'record')  -> 
			Offset = indent(Opt#opt.indent, (Depth - 1)),
			% Record ?
			Name = element(1, Term),
			% Check if a definition was given in option
			case check_rec_def(Name, Opt#opt.records) of
				  false -> throw({'unable_to_encode', Name}) ;
				  Def   -> X = lists:foldl( fun({A,B}, Acc) -> 
														Acc ++ [io_lib:format("~ts\"~ts\": ~ts", 
																					[Offset, 
		                                                          A, 
		                                                          encode(B, Opt,right, (Depth + 1))])] 
		                                  end, [], record2object(Term, Def)),
							  io_lib:format("~ts~ts{~ts~ts~ts~ts}", 
												[ case Depth of 1 -> "" ; _ -> cr(Opt#opt.nl) end, 
												  Offset, 
												  cr(Opt#opt.nl), 
												  string:join(X, "," ++ cr(Opt#opt.nl)), 
												  cr(Opt#opt.nl), 
												  Offset]) 	  
			end;
encode(Term, Opt, _, _Depth) 
		when is_tuple(Term),
           (Opt#opt.mode =:= map)  -> 
"todo"
			;
encode([{_, _}| _] = Term, Opt, _, Depth) 
		when is_list(Term),
           (Opt#opt.mode =:= proplist)  -> 
								Offset = indent(Opt#opt.indent, Depth),
								X = lists:foldl( fun({A,B}, Acc) -> 
														Acc ++ [io_lib:format("~ts\"~ts\": ~ts", 
																					[case Opt#opt.compact of true -> " " ; _ -> Offset end, 
		                                                          A, 
		                                                          encode(B, Opt,right, (Depth + 1))])] 
		                                  end, [], Term),
								io_lib:format("~ts~ts{~ts~ts~ts~ts}", 
												[ case Depth of 0 -> "" ; _ -> cr(Opt#opt.nl) end, 
												  Offset, 
												  case Opt#opt.compact of true -> "" ; _ -> cr(Opt#opt.nl) end, 
												  string:join(X, "," ++ case Opt#opt.compact of true -> "" ; _ -> cr(Opt#opt.nl) end ) , 
												  case Opt#opt.compact of true -> "" ; _ -> cr(Opt#opt.nl) end, 
												  case Opt#opt.compact of true -> " " ; _ -> Offset end]) 
			;
encode(Term, Opt, _, _Depth) 
		when is_tuple(Term),
           (Opt#opt.mode =:= struct)  -> 
"todo"
			;
encode(Term, _Opt, _, _Depth)
		when is_integer(Term) -> io_lib:format("~p", [Term]) ;
encode(Term, _Opt, _, _Depth)
		when is_float(Term) -> io_lib:format("~ts", [float_to_list(Term, [{decimals, 4}, compact])]) ;
encode(Term, _Opt, _, _Depth)
		when is_binary(Term) -> io_lib:format("\"~ts\"", [binary_to_list(Term)]) ;
encode(null, _Opt, _, _Depth)      -> io_lib:format("null", []) ;
encode(undefined, _Opt, _, _Depth) -> io_lib:format("null", []) ;
encode(true, _Opt, _, _Depth)      -> io_lib:format("true", []) ;
encode(false, _Opt, _, _Depth)     -> io_lib:format("false", []) ;
encode(Term, _Opt, _, _Depth)
		when is_atom(Term) -> io_lib:format("\"~ts\"", [atom_to_list(Term)]) ;
encode(Term, Opt, _, Depth) 
		when is_list(Term) -> Offset = case Opt#opt.mode of
													 proplist -> indent(Opt#opt.indent, Depth) ;
													 _        -> indent(Opt#opt.indent, (Depth - 1))
												 end,
									 case io_lib:printable_unicode_list(Term) of
											 false -> A = lists:foldl(fun(X, Acc) -> 
																				Acc ++ [Offset ++ encode(X, Opt, right, Depth + 1)] 
																			  end, [], Term),
														 io_lib:format("~ts~ts[~ts~ts~ts~ts]", 
																			[ case Depth of 1 -> "" ; _ -> cr(Opt#opt.nl) end, 
																			  Offset,
																			  case Opt#opt.mode of proplist -> "" ; _ -> cr(Opt#opt.nl) end,
																			  string:join(A, ","++ case Opt#opt.mode of proplist -> "" ; _ -> cr(Opt#opt.nl) end ) , 
																			  cr(Opt#opt.nl), 
																			  Offset]) ;
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
			{ok, R}
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

check_rec_def(Name, Opt) -> case Opt of
											[]    -> false ;
											Recs  -> case proplists:get_value(Name, Recs) of
																	undefined -> false ;
																	Res       -> Res
													   end
									 end.


record2object(Term, Def) 
	when is_tuple(Term) -> [_ | T] = erlang:tuple_to_list(Term),
								  lists:zip(Def, T).

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

indent(I, Depth) when is_list(I),(Depth >= 0) -> string:copies(I, Depth);
indent(I, _ ) when is_list(I) -> "";
indent(_, _) -> "".

cr(Nl) when is_list(Nl) -> Nl ;
cr(_) -> "" .
 
options(O) -> I = case proplists:get_value(indent, O) of
							  undefined -> "" ;
							  false -> "" ;
							  true  -> "   "
						end, 
				  N = case proplists:get_value(indent, O) of
							  undefined -> "" ;
							  false -> "" ;
							  true  -> "\n"
						end,
				  C = case proplists:get_value(compact, O) of
							  undefined -> false ;
							  false -> false ;
							  true  -> true ;
							  _     -> false
						end,
				  R = case proplists:get_value(records, O) of
							  undefined -> [] ;
							  X         -> X
						end,
				  M = case proplists:get_value(mode, O) of
							  map      -> map ;
							  proplist -> proplist ;
							  'record' -> 'record' ;
							  _        -> struct 
						end,
				  #opt{nl = N
						,indent = I
                  ,compact = C
						,records = R
                  ,mode = M
						}. 

