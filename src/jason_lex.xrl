Definitions.
CHAR = [\x20|\x21|[\x23-\x5B]|[\x5D-\xFF]|/|\\u[0-9]{4}]
ESC = [\\]
QM = [\x22]
INT = [0-9]
WS = [\s|\t|\r|\n]
EXP = [e|E]

Rules.
\[ : {token, {'b-a', TokenLine, '['}}.
\] : {token, {'e-a', TokenLine, ']'}}.
\{ : {token, {'b-o', TokenLine, '{'}}.
\} : {token, {'e-o', TokenLine, '}'}}.
\: : {token, {'n-s', TokenLine, ':'}}.
\, : {token, {'v-s', TokenLine, ','}}.
false  : {token, {'false', TokenLine}}.
null   : {token, {'null', TokenLine}}.
true   : {token, {'true', TokenLine}}.
\-     : {token, {minus, TokenLine, '-'}}.
\+     : {token, {plus , TokenLine, '+'}}.
\.     : {token, {'d-p', TokenLine, '.'}}.
{EXP} : {token, {exponent, TokenLine, TokenChars}}.

{INT}+ : {token, {'digits', TokenLine, TokenChars}}.


{WS}+   : skip_token.

"[^"\\]*(\\.[^"\\]*)*"  : case unicode:characters_to_binary(unescape(lists:sublist(TokenChars, 2, TokenLen - 2))) of
                              {error, _, _}      -> {error, "unicode error"} ;
                              {incomplete, _, _} -> {error, "unicode error"} ;
                              X                  -> {token,{'chr', TokenLine, X}}
                           end.


{CHAR} : {token, {'chr', TokenLine, unicode:characters_to_binary(TokenChars)}}.

Erlang code.
-compile(inline).
%-compile([native, {hipe, [o3]}]).
-dialyzer([{nowarn_function, [yyrev/2]}]).

unescape([$\\,$\"|Cs]) -> [$\"|unescape(Cs)];
unescape([$\\,$\\|Cs]) -> [$\\|unescape(Cs)];
unescape([$\\,$/|Cs]) -> [$/|unescape(Cs)];
unescape([$\\,$b|Cs]) -> [$\b|unescape(Cs)];
unescape([$\\,$f|Cs]) -> [$\f|unescape(Cs)];
unescape([$\\,$n|Cs]) -> [$\n|unescape(Cs)];
unescape([$\\,$r|Cs]) -> [$\r|unescape(Cs)];
unescape([$\\,$t|Cs]) -> [$\t|unescape(Cs)];
unescape([$\\,$u,C0,C1,C2,C3|Cs]) ->
    C = dehex(C3) bor
	(dehex(C2) bsl 4) bor
	(dehex(C1) bsl 8) bor
	(dehex(C0) bsl 12),
    [C|unescape(Cs)];
unescape([C|Cs]) -> [C|unescape(Cs)];
unescape([]) -> [].

dehex(C) when C >= $0, C =< $9 -> C - $0;
dehex(C) when C >= $a, C =< $f -> C - $a + 10;
dehex(C) when C >= $A, C =< $F -> C - $A + 10.
