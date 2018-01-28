%%%-----------------------------------------------------------------------------
%%% File:      jason_pp.erl
%%% @author    Eric Pailleau <jason@crownedgrouse.com>
%%% @copyright 2017 crownedgrouse.com
%%% @doc
%%% Jason's Pretty Printing Library
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
%%% Created : 2017-03-12
%%%-----------------------------------------------------------------------------

-module(jason_pp).

-export([indent/1, indent/2]).

-record(pp, {style = 'k&r'
            ,depth = 0
            ,nl    = "\n"
            ,tab   = "   "
            ,context = l
            }
       ).

indent(Data)
   when is_binary(Data) -> indent(binary_to_list(Data), #pp{});

indent(Data)
   when is_list(Data) -> indent(Data, #pp{}).

indent(Data, Opt)
   when is_binary(Data),
        is_list(Opt) -> indent(binary_to_list(Data), Opt);

indent(Data, Opt)
   when is_list(Data),
        is_list(Opt)-> indent(Data, #pp{style = getopt(style, Opt)
                                       ,nl    = getopt(nl, Opt)
                                       ,tab   = getopt(tab, Opt)
                                       });

indent(Data, R)
   when is_binary(Data),
        is_record(R, pp) -> indent(binary_to_list(Data), R);

indent(Data, R)
   when is_list(Data),
        is_record(R, pp) -> {ok, X, _} = jason_lex:string(Data),
                            L = indent(X, R#pp.style, R, []),
                            lists:flatten(L).

%[{'b-o',1,'{'},
% {chr,1,<<"key1">>},
% {'n-s',1,':'},
% {chr,1,<<"value">>},
% {'v-s',1,','},
% {chr,1,<<"key2">>},
% {'n-s',1,':'},
% {'b-a',1,'['},
% {chr,1,<<"aaa">>},
% {'v-s',1,','},
% {chr,1,<<"bbb">>},
% {'e-a',1,']'},
% {'e-o',1,'}'}]

%% k&r
indent([{'chr',_,B}|T], 'k&r', R, Acc) ->
   indent(T, 'k&r',
          R,
          Acc ++ "\"" ++ binary_to_list(B) ++ "\"") ;
indent([{'v-s',_,_}|T], 'k&r', R, Acc) ->
   indent(T, 'k&r',
          R,
          Acc ++ "," ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth));
indent([{'n-s',_,_}|T], 'k&r', R, Acc) ->
   indent(T, 'k&r',
          R,
          Acc ++ ": ");
indent([{'b-o',_,_}|T], 'k&r', R, Acc) ->
   indent(T, 'k&r',
          R#pp{depth = ( R#pp.depth + 1)},
          Acc ++ "{" ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth + 1)) ;
indent([{'e-o',_,_}|T], 'k&r', R, Acc) ->
   indent(T, 'k&r',
          R#pp{depth = ( R#pp.depth - 1)},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, (R#pp.depth - 1)) ++ "}" );
indent([{'b-a',_,_}|T], 'k&r', R, Acc) ->
   indent(T, 'k&r',
          R#pp{depth = ( R#pp.depth + 1)},
          Acc ++ "[" ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth + 1));
indent([{'e-a',_,_}|T], 'k&r', R, Acc) ->
   indent(T, 'k&r',
          R#pp{depth = ( R#pp.depth - 1)},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, (R#pp.depth - 1)) ++ "]" );
%% otbs
indent([{'chr',_,B}|T], 'otbs', R, Acc) ->
   indent(T, 'otbs',
          R#pp{context = case R#pp.context of 'e-o' -> l ; X -> X end},
          Acc ++ case R#pp.context of 'e-o' -> R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth) ; _ -> "" end ++ "\"" ++ binary_to_list(B) ++ "\"") ;
indent([{'v-s',_,_}|T], 'otbs', R, Acc) ->
   indent(T, 'otbs',
          R,
          Acc ++ "," ++ case R#pp.context of 'e-o' -> "" ; _ -> R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth) end );
indent([{'n-s',_,_}|T], 'otbs', R, Acc) ->
   indent(T, 'otbs',
          R,
          Acc ++ ": ");
indent([{'b-o',_,_}|T], 'otbs', R, Acc) ->
   indent(T, 'otbs',
          R#pp{depth = ( R#pp.depth + 1), context= 'b-o'},
          Acc ++ "{" ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth + 1)) ;
indent([{'e-o',_,_}|T], 'otbs', R, Acc) ->
   indent(T, 'otbs',
          R#pp{depth = ( R#pp.depth - 1), context= 'e-o'},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, (R#pp.depth - 1)) ++ "}" );
indent([{'b-a',_,_}|T], 'otbs', R, Acc) ->
   indent(T, 'otbs',
          R#pp{depth = ( R#pp.depth + 1)},
          Acc ++ "[" ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth + 1));
indent([{'e-a',_,_}|T], 'otbs', R, Acc) ->
   indent(T, 'otbs',
          R#pp{depth = ( R#pp.depth - 1)},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, (R#pp.depth - 1)) ++ "]" );
%% stroustrup
indent([{'chr',_,B}|T], 'stroustrup', R, Acc) ->
   indent(T, 'stroustrup',
          R#pp{context = case R#pp.context of 'e-o' -> l ; X -> X end},
          Acc ++ case R#pp.context of 'e-o' -> R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth) ; _ -> "" end ++ "\"" ++ binary_to_list(B) ++ "\"") ;
indent([{'v-s',_,_}|T], 'stroustrup', R, Acc) ->
   indent(T, 'stroustrup',
          R#pp{context = case R#pp.context of 'e-o' -> l ; X -> X end },
          Acc ++ case R#pp.context of 'e-o' -> R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth) ; _ -> "" end ++ "," ++ case R#pp.context of 'e-o' -> "" ; _ -> R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth) end );
indent([{'n-s',_,_}|T], 'stroustrup', R, Acc) ->
   indent(T, 'stroustrup',
          R,
          Acc ++ ": ");
indent([{'b-o',_,_}|T], 'stroustrup', R, Acc) ->
   indent(T, 'stroustrup',
          R#pp{depth = ( R#pp.depth + 1), context= 'b-o'},
          Acc ++ "{" ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth + 1)) ;
indent([{'e-o',_,_}|T], 'stroustrup', R, Acc) ->
   indent(T, 'stroustrup',
          R#pp{depth = ( R#pp.depth - 1), context= 'e-o'},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, (R#pp.depth - 1)) ++ "}" );
indent([{'b-a',_,_}|T], 'stroustrup', R, Acc) ->
   indent(T, 'stroustrup',
          R#pp{depth = ( R#pp.depth + 1)},
          Acc ++ "[" ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth + 1));
indent([{'e-a',_,_}|T], 'stroustrup', R, Acc) ->
   indent(T, 'stroustrup',
          R#pp{depth = ( R#pp.depth - 1)},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, (R#pp.depth - 1)) ++ "]" );
%% allman
indent([{'chr',_,B}|T], 'allman', R, Acc) ->
   indent(T, 'allman',
          R#pp{context = l},
          Acc ++ "\"" ++ binary_to_list(B) ++ "\"") ;
indent([{'v-s',_,_}|T], 'allman', R, Acc) ->
   indent(T, 'allman',
          R#pp{context= 'v-s'},
          Acc ++ "," ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth));
indent([{'n-s',_,_}|T], 'allman', R, Acc) ->
   indent(T, 'allman',
          R#pp{context = l},
          Acc ++ ": ");
indent([{'b-o',_,_}|T], 'allman', R, Acc) ->
   indent(T, 'allman',
          R#pp{depth = ( R#pp.depth + 1), context = l},
          Acc ++ case R#pp.context of
                     'v-s' -> "" ;
                         _ -> R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth )
                 end ++ "{" ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth + 1)) ;
indent([{'e-o',_,_}|T], 'allman', R, Acc) ->
   indent(T, 'allman',
          R#pp{depth = ( R#pp.depth - 1), context = l},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, (R#pp.depth - 1)) ++ "}" );
indent([{'b-a',_,_}|T], 'allman', R, Acc) ->
   indent(T, 'allman',
          R#pp{depth = ( R#pp.depth + 1), context = l},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth ) ++ "[" ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth + 1));
indent([{'e-a',_,_}|T], 'allman', R, Acc) ->
   indent(T, 'allman',
          R#pp{depth = ( R#pp.depth - 1), context = l},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, (R#pp.depth - 1)) ++ "]" );

%% whitesmiths
indent([{'chr',_,B}|T], 'whitesmiths', R, Acc) ->
   indent(T, 'whitesmiths',
          R#pp{context = 'chr'},
          Acc ++ "\"" ++ binary_to_list(B) ++ "\"") ;
indent([{'v-s',_,_}|T], 'whitesmiths', R, Acc) ->
   indent(T, 'whitesmiths',
          R#pp{context= 'v-s'},
          Acc ++ "," ++ case R#pp.context of 'e-o' -> "" ; _ -> R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth - 1) end );
indent([{'n-s',_,_}|T], 'whitesmiths', R, Acc) ->
   indent(T, 'whitesmiths',
          R#pp{context = 'n-s'},
          Acc ++ ": ");
indent([{'b-o',_,_}|T], 'whitesmiths', R, Acc) ->
   indent(T, 'whitesmiths',
          R#pp{depth = ( R#pp.depth + 1), context = 'b-o'},
          Acc ++ case (R#pp.depth > 0 ) of true -> R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth) ; _ -> "" end ++ "{" ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth)) ;
indent([{'e-o',_,_}|T], 'whitesmiths', R, Acc) ->
   indent(T, 'whitesmiths',
          R#pp{depth = ( R#pp.depth - 1), context = 'e-o'},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, (R#pp.depth - 1)) ++ "}" );
indent([{'b-a',_,_}|T], 'whitesmiths', R, Acc) ->
   indent(T, 'whitesmiths',
          R#pp{depth = ( R#pp.depth + 1), context = 'b-a'},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth ) ++ "[" ) ;
indent([{'e-a',_,_}|T], 'whitesmiths', R, Acc) ->
   indent(T, 'whitesmiths',
          R#pp{depth = ( R#pp.depth - 1), context = 'e-a'},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, (R#pp.depth - 1)) ++ "]" );
% gnu TODO

% horstmann
indent([{'chr',_,B}|T], 'horstmann', R, Acc) ->
   indent(T, 'horstmann',
          R#pp{context='chr'},
          Acc ++ "\"" ++ binary_to_list(B) ++ "\"") ;
indent([{'v-s',_,_}|T], 'horstmann', R, Acc) ->
   indent(T, 'horstmann',
          R#pp{context='v-s'},
          Acc ++ case R#pp.context of 'e-o' -> R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth) ; _ -> "" end ++ "," ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth));
indent([{'n-s',_,_}|T], 'horstmann', R, Acc) ->
   indent(T, 'horstmann',
          R#pp{context='n-s'},
          Acc ++ ": ");
indent([{'b-o',_,_}|T], 'horstmann', R, Acc) ->
   indent(T, 'horstmann',
          R#pp{depth = ( R#pp.depth + 1), context = 'b-o'},
          Acc  ++ case  R#pp.context of 'v-s' -> "" ; _ -> R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth ) end ++ "{" ++ (R#pp.tab -- " ") ) ;
indent([{'e-o',_,_}|T], 'horstmann', R, Acc) ->
   indent(T, 'horstmann',
          R#pp{depth = ( R#pp.depth - 1), context = 'e-o'},
           Acc ++ R#pp.nl ++ string:copies(R#pp.tab, (R#pp.depth - 1)) ++ "}" );
indent([{'b-a',_,_}|T], 'horstmann', R, Acc) ->
   indent(T, 'horstmann',
          R#pp{depth = ( R#pp.depth + 1), context = 'b-a'},
          Acc ++ R#pp.nl ++  string:copies(R#pp.tab, R#pp.depth ) ++  "[" );
indent([{'e-a',_,_}|T], 'horstmann', R, Acc) ->
   indent(T, 'horstmann',
          R#pp{depth = ( R#pp.depth - 1), context = 'e-a'},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, (R#pp.depth - 1)) ++ "]");
% pico
indent([{'chr',_,B}|T], 'pico', R, Acc) ->
   indent(T, 'pico',
          R#pp{context='chr'},
          Acc ++ "\"" ++ binary_to_list(B) ++ "\"") ;
indent([{'v-s',_,_}|T], 'pico', R, Acc) ->
   indent(T, 'pico',
          R#pp{context='v-s'},
          Acc ++ "," ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth));
indent([{'n-s',_,_}|T], 'pico', R, Acc) ->
   indent(T, 'pico',
          R#pp{context='n-s'},
          Acc ++ ": ");
indent([{'b-o',_,_}|T], 'pico', R, Acc) ->
   indent(T, 'pico',
          R#pp{depth = ( R#pp.depth + 1), context = 'b-o'},
          Acc ++ case R#pp.context of 'b-a' -> "" ; 'v-s' -> ""; _ -> case (R#pp.depth > 0) of true -> R#pp.nl ; _ -> "" end ++ string:copies(R#pp.tab, R#pp.depth ) end ++ "{" ++ (R#pp.tab -- " ") ) ;
indent([{'e-o',_,_}|T], 'pico', R, Acc) ->
   indent(T, 'pico',
          R#pp{depth = ( R#pp.depth - 1), context = 'e-o'},
           Acc ++ (R#pp.tab -- " ") ++ "}" );
indent([{'b-a',_,_}|T], 'pico', R, Acc) ->
   indent(T, 'pico',
          R#pp{depth = ( R#pp.depth + 1), context = 'b-a'},
          Acc ++ R#pp.nl ++  string:copies(R#pp.tab, R#pp.depth ) ++  "[" ++ (R#pp.tab -- " ") );
indent([{'e-a',_,_}|T], 'pico', R, Acc) ->
   indent(T, 'pico',
          R#pp{depth = ( R#pp.depth - 1), context = 'e-a'},
          Acc ++ (R#pp.tab -- " " ) ++ "]");
% ratliff
indent([{'chr',_,B}|T], 'ratliff', R, Acc) ->
   indent(T, 'ratliff',
          R#pp{context = case R#pp.context of 'e-o' -> l ; X -> X end},
          Acc ++ case R#pp.context of 'e-o' -> R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth) ; _ -> "" end ++ "\"" ++ binary_to_list(B) ++ "\"") ;
indent([{'v-s',_,_}|T], 'ratliff', R, Acc) ->
   indent(T, 'ratliff',
          R#pp{context= 'v-s'},
          Acc ++ "," ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth) );
indent([{'n-s',_,_}|T], 'ratliff', R, Acc) ->
   indent(T, 'ratliff',
          R#pp{context= 'n-s'},
          Acc ++ ": ");
indent([{'b-o',_,_}|T], 'ratliff', R, Acc) ->
   indent(T, 'ratliff',
          R#pp{depth = ( R#pp.depth + 1), context= 'b-o'},
          Acc ++ "{" ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth + 1)) ;
indent([{'e-o',_,_}|T], 'ratliff', R, Acc) ->
   indent(T, 'ratliff',
          R#pp{depth = ( R#pp.depth - 1), context= 'e-o'},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth) ++ "}" );
indent([{'b-a',_,_}|T], 'ratliff', R, Acc) ->
   indent(T, 'ratliff',
          R#pp{depth = ( R#pp.depth + 1), context= 'b-a'},
          Acc ++ "[" ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth + 1));
indent([{'e-a',_,_}|T], 'ratliff', R, Acc) ->
   indent(T, 'ratliff',
          R#pp{depth = ( R#pp.depth - 1), context= 'e-a'},
          Acc ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth) ++ "]" );
% lisp
indent([{'chr',_,B}|T], 'lisp', R, Acc) ->
   indent(T, 'lisp',
          R,
          Acc ++ "\"" ++ binary_to_list(B) ++ "\"") ;
indent([{'v-s',_,_}|T], 'lisp', R, Acc) ->
   indent(T, 'lisp',
          R,
          Acc ++ "," ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth));
indent([{'n-s',_,_}|T], 'lisp', R, Acc) ->
   indent(T, 'lisp',
          R,
          Acc ++ ": ");
indent([{'b-o',_,_}|T], 'lisp', R, Acc) ->
   indent(T, 'lisp',
          R#pp{depth = ( R#pp.depth + 1)},
          Acc ++ "{" ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth + 1)) ;
indent([{'e-o',_,_}|T], 'lisp', R, Acc) ->
   indent(T, 'lisp',
          R#pp{depth = ( R#pp.depth - 1)},
          Acc ++ "}" );
indent([{'b-a',_,_}|T], 'lisp', R, Acc) ->
   indent(T, 'lisp',
          R#pp{depth = ( R#pp.depth + 1)},
          Acc ++ "[" ++ R#pp.nl ++ string:copies(R#pp.tab, R#pp.depth + 1));
indent([{'e-a',_,_}|T], 'lisp', R, Acc) ->
   indent(T, 'lisp',
          R#pp{depth = ( R#pp.depth - 1)},
          Acc  ++ "]");

indent([], _, _, Acc) -> Acc;
indent(_, _, _, _ ) -> throw('unknow_style').

getopt(style, []) -> 'k&r' ;
getopt(style, Opt)
   when is_list(Opt) -> proplists:get_value(style, Opt, 'k&r');
getopt(nl, []) -> "\n" ;
getopt(nl, Opt)
   when is_list(Opt) -> proplists:get_value(nl, Opt, "\n");
getopt(tab,[]) -> "   " ;
getopt(tab, Opt)
   when is_list(Opt) -> proplists:get_value(tab, Opt, "   ").


