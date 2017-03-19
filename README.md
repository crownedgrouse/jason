# jason
Work still in progress...  but Beta testing allowed !

## Overview ##

`jason` is a JSON encode/decode library written in Erlang.

Yet another Nth JSON Erlang project, while there are already good ones ?

Yes, because other projects, which are really good ones, were missing some features mainly for record handling.

`jason` do not use NIF, so if performance is your main interest, see other well known projects using NIFs.

So what makes `jason` usefull for me, then ?

`jason`'s bias is to be able to [encode and decode records at runtime](https://github.com/crownedgrouse/jason/wiki/Records),
without any code manipulation at compile time. 
This make `jason` easy to use, with a minimal footprint in your own source code.

`jason` let you easily convert [JSON object to record](https://github.com/crownedgrouse/jason/wiki/Records) specification, and create for
you ad-hoc modules to handle records. Even with deeply nested JSON objects.

`jason` allow you to use [JSON to records](https://github.com/crownedgrouse/jason/wiki/Records) translation even if the JSON source is not
stable and JSON object can change without any warning. This is particulary interesting when JSON source is coming from tiers.

Float are converted with automatic precision without need to set a precision depth.

[Pretty printing](https://github.com/crownedgrouse/jason/wiki/Pretty-printing) JSON in several indentation format is another rare feature that `jason` gives you, among others.

Only very few options to be user friendly.

See [Wiki](https://github.com/crownedgrouse/jason/wiki) for documentation and tutorial.

Specifications :
* Pure Erlang (no NIF)
* Decoding use Leex/Yecc parser
* JSON object can be decoded in several formats :
   * binary struct
   * proplists
   * maps
   * records with automatic ad hoc module loaded for record handling
* No `parse_transform`

## Data types ##
jason:types/0 show in console how Erlang types are converted to JSON and back to Erlang data.


```
1> jason:types().
Erlang                       JSON                      Erlang
================================================================================

%% Atoms
null                    -> null                 -> null
undefined               -> null                 -> null
true                    -> true                 -> true
false                   -> false                -> false
any                     -> "any"                -> <<"any">>

%% Integer
123                     -> 123                  -> 123

%% Float (Automatic precision)
123.456789              -> 123.456789           -> 123.456789
2.30e+0                 -> 2.3                  -> 2.3
2.30e+3                 -> 2300.0               -> 2.3e3
2.30e-3                 -> 0.0023               -> 0.0023

%% List
[1,2,3]                 -> [1,2,3]              -> [1,2,3] 
[a,"b",<<"c">>]         -> ["a","b","c"]        -> [<<"a">>,<<"b">>,<<"c">>] 

%% Binary (key/value) mode=struct (default)
<<"abc">>               -> "abc"                -> <<"abc">>

%% Struct
%  mode=struct (default)
{<<"abc">>,<<"def">>}   -> {"abc": "def"}       -> [{<<"abc">>,<<"def">>}]
%  mode=proplist
{<<"abc">>,<<"def">>}   -> {"abc": "def"}       -> [{abc,"def"}]
%  mode=map
{<<"abc">>,<<"def">>}   -> {"abc": "def"}       -> #{abc => "def"}
%  mode=record
{<<"abc">>,<<"def">>}   -> {"abc": "def"}       -> {'111259705',"def"} 
                                                with -record('111259705', {abc  = []  :: list()}).

%% Proplist
%  mode=struct (default)
[{abc,<<"def">>}]       -> {"abc": "def"}       -> [{<<"abc">>,<<"def">>}]
%  mode=proplist
[{abc,<<"def">>}]       -> {"abc": "def"}       -> [{abc,"def"}]
%  mode=map
[{abc,<<"def">>}]       -> {"abc": "def"}       -> #{abc => "def"}
%  mode=record
[{abc,<<"def">>}]       -> {"abc": "def"}       -> {'111259705',"def"} 
                                                with -record('111259705', {abc  = []  :: list()}).

%% Map
%  mode=struct (default)
#{"abc" => <<"def">>}   -> {"abc": "def"}       -> [{<<"abc">>,<<"def">>}]
%  mode=proplist
#{"abc" => <<"def">>}   -> {"abc": "def"}       -> [{abc,"def"}]
%  mode=map
#{"abc" => <<"def">>}   -> {"abc": "def"}       -> #{abc => "def"}
%  mode=record
#{"abc" => <<"def">>}   -> {"abc": "def"}       -> {'111259705',"def"} 
                                                with -record('111259705', {abc  = []  :: list()}).

%% Record - encoding using option [{records, [{r, record_info(fields, r)}]}] or [{records, [{r, [k1,k2]}]}]
%  mode=struct (default)
{r,1,<<"ab">>}          -> {"k1": 1,"k2": "ab"} -> [{<<"k1">>,1},
                                                    {<<"k2">>,<<"ab">>}]
%  mode=proplist
{r,1,<<"ab">>}          -> {"k1": 1,"k2": "ab"} -> [{k1,1},{k2,"ab"}]
%  mode=map
{r,1,<<"ab">>}          -> {"k1": 1,"k2": "ab"} -> #{k1 => 1,k2 => "ab"}
%  mode=record
{r,1,<<"ab">>}          -> {"k1": 1,"k2": "ab"} -> {'8056669',1,"ab"} 
                                                with -record('8056669', {k1  = 0  :: integer(), k2  = []  :: list()}).
```


