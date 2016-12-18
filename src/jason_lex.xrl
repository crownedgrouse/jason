Definitions.
CHAR = [\x20|\x21|[\x23-\x5B]|[\x5D-\xFF]|/|\\u[0-9]{4}]
ESC = [\\]
QM = [\x22]
INT = [0-9]
WS = [\s|\t|\r|\n]
EXP = [e|E]

Rules.
\[ : {token, {'begin-array', TokenLine, '['}}.
\] : {token, {'end-array', TokenLine, ']'}}.
\{ : {token, {'begin-object', TokenLine, '{'}}.
\} : {token, {'end-object', TokenLine, '}'}}.
\: : {token, {'name-separator', TokenLine, ':'}}.
\, : {token, {'value-separator', TokenLine, ','}}.
false  : case get(str) of
              true -> {token, {'chr', TokenLine, TokenChars}} ;
              _    -> {token, {'false', TokenLine}}
         end.
null   : case get(str) of
              true -> {token, {'chr', TokenLine, TokenChars}} ;
              _    -> {token, {'null', TokenLine}}
         end.
true   : case get(str) of
              true -> {token, {'chr', TokenLine, TokenChars}} ;
              _    -> {token, {'true', TokenLine}}
         end.
\-     : case get(str) of
              true -> {token, {'chr', TokenLine, TokenChars}} ;
              _    -> {token, {minus, TokenLine, '-'}}
         end.
\+     : case get(str) of
              true -> {token, {'chr', TokenLine, TokenChars}} ;
              _    -> {token, {plus , TokenLine, '+'}}
         end.
\.     : case get(str) of
              true -> {token, {'chr', TokenLine, TokenChars}} ;
              _    -> {token, {'decimal-point', TokenLine, '.'}}
         end.
{EXP} : case get(str) of
              true -> {token, {'chr', TokenLine, TokenChars}} ;
              _    -> {token, {exponent, TokenLine, TokenChars}}
         end.
{ESC}  : case get(esc) of 
              undefined -> put(esc, true),
                           skip_token ;
              true  -> put(esc, false),
                       {token, {'chr', TokenLine, "\\" ++ TokenChars}};
              false -> put(esc, true),
                       skip_token
         end.
{INT}+ : case get(str) of
              true -> {token, {'chr', TokenLine, TokenChars}} ;
              _    -> {token, {'digits', TokenLine, TokenChars}}
         end.


{WS}+   : skip_token.

"[^"\\]*(\\.[^"\\]*)*"  : {token,{'chr', TokenLine, parse_string(strip(TokenChars, TokenLen))}}.


{CHAR} : {token, {'chr', TokenLine, TokenChars}}.

Erlang code.

strip(TokenChars,TokenLen) -> lists:sublist(TokenChars, 2, TokenLen - 2).

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

parse_string(StringChars) -> 
unescape(StringChars).
