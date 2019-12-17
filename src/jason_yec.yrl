Nonterminals json array values object members member number integer int float frac exp value literal string chars.

Terminals 'b-a' 'e-a' 'v-s' 'b-o' 'e-o'
          'n-s' digits minus plus 'd-p' 'chr' exponent
          true false null .

Rootsymbol json.

json -> array   : '$1' .
json -> object  : '$1' .
json -> number  : '$1' .
json -> string  : detect_date('$1') .
json -> literal : '$1' .

array  -> 'b-a' 'e-a' : [].
array  -> 'b-a' values 'e-a' : '$2'.

values -> value : ['$1'].
values -> value 'v-s' values : ['$1'] ++ '$3' .

object -> 'b-o' 'e-o' : [].
object -> 'b-o' members 'e-o' : case get(jason_mode) of
                                    record   -> jason_lib:recordify('$2');
                                    map      -> jason_lib:mapify('$2');
                                    proplist -> jason_lib:proplistify('$2');
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
value -> string  : detect_date('$1').

literal -> true  : true.
literal -> false : false.
literal -> null  : null.

Erlang code.
-compile(inline).
%-compile([native, {hipe, [o3]}]).

detect_date(Data) when is_binary(Data),(byte_size(Data) == 24)  ->
    case io_lib:fread("~d-~d-~dT~d:~d:~fZ", binary_to_list(Data)) of
         {ok,[Y,M,D,H,I,S],[]} -> {{Y,M,D}, {H,I,S}};
         _ -> Data
    end;

detect_date(Data) when is_binary(Data),(byte_size(Data) == 20)  ->
    case io_lib:fread("~d-~d-~dT~d:~d:~dZ", binary_to_list(Data)) of
         {ok, [Y, M, D, H, I, S],[]} -> {{Y,M,D}, {H,I,S}} ;
         _  -> Data
    end;

detect_date(Data) -> Data.


