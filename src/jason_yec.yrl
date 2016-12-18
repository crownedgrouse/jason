Nonterminals json array values object members member number integer int float frac exp value literal string chars.

Terminals 'begin-array' 'end-array' 'value-separator' 'begin-object' 'end-object'
          'name-separator' digits minus plus 'decimal-point' 'chr' exponent
          true false null .

Rootsymbol json.

json -> array   : getstrback(lists:flatten('$1')).
json -> object  : getstrback('$1').
json -> number  : '$1' .
json -> string  : getstrback('$1') .
json -> literal : '$1' .

array  -> 'begin-array' 'end-array' : [].
array  -> 'begin-array' values 'end-array' : '$2'.

values -> value : '$1'.
values -> value 'value-separator' values : ['$1'] ++ ['$3'].

object -> 'begin-object' 'end-object' : {struct, []}.
object -> 'begin-object' members 'end-object' : {struct, '$2'}.

members -> member : '$1'.
members -> member 'value-separator' members : lists:flatten(['$1'] ++ ['$3']).

member -> string 'name-separator' value : {'$1', '$3'}.

number -> integer exp : list_to_float('$1' ++ ".0" ++ '$2').
number -> float exp : list_to_float('$1' ++ '$2').
number -> integer : list_to_integer('$1').
number -> float : list_to_float('$1').

integer -> int : '$1'.
integer -> minus int : "-" ++ '$2' .

int -> digits : {_, _, C} = '$1', C.

float -> minus int frac : "-" ++ '$2' ++ '$3'.
float -> int frac : '$1' ++ '$2'.

frac -> 'decimal-point' digits : {_, _, C} = '$2', "." ++ C.

exp -> exponent minus digits : {_, _, C} = '$3', "e-" ++ C.
exp -> exponent plus  digits : {_, _, C} = '$3', "e" ++ C.
exp -> exponent digits : {_, _, C} = '$2', "e" ++ C.

string ->  chars  : {jason_str, lists:flatten('$1')}.

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

getstrback({struct,X}) -> {struct, getstrback(X)};
getstrback({jason_str, X}) -> X ; 
getstrback({{jason_str, X}, Y}) -> getstrback({X, Y});
getstrback({X, {jason_str, Y}}) -> getstrback({X, Y});
getstrback(L) when is_list(L) -> 
        lists:map(fun(X) -> case X of
                     {jason_str, S} -> S ;
                     X              -> getstrback(X)
                 end 
        end, L);
getstrback(X) -> X.


 

