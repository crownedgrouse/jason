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
encode(_Term) -> ok.
encode(_Term, _) -> ok.

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




 

