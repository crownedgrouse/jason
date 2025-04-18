# jason ![Build status](https://github.com/crownedgrouse/jason/actions/workflows/erlang.yml/badge.svg)  #

`jason` is a JSON encode/decode library written in Erlang.

**DEPRECATION NOTE: as a JSON library is now part of ERLANG/OTP, this library will be replaced by another in close future.**

*`json` library will be used instead for parsing, encoding and decoding, but features will be kept. A new name will be chosen in order to be less confusing with the Elixir eponym library.*

## Overview ##

This project was mainly created for easy handling of JSON from/to Erlang records,
but allow binary struct, proplists and maps formats.

Project is available on hex.pm under name [jason_erl](https://hex.pm/packages/jason_erl).

`jason` offer also a rare feature : pretty printing JSON in several indentation styles. 

See [Wiki](https://github.com/crownedgrouse/jason/wiki) for documentation and tutorial.

## Specifications ##
* Pure Erlang (no NIF)
* Decoding use Leex/Yecc parser
* JSON object can be decoded in several formats :
   * binary struct
   * proplists
   * maps
   * records with automatic ad hoc module loaded for record handling
* No `parse_transform` . Record definitions can be given as arguments or extracted from module(s) abstract code at runtime.

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

%% Date
{{1970,1,1},{0,0,0}}    -> "1970-01-01T00:00:00Z"       -> {{1970,1,1},
                                                            {0,0,0}}
{{1970,1,1},{0,0,0.0}}  -> "1970-01-01T00:00:00.000Z"   -> {{1970,1,1},
                                                            {0,0,0.0}}

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

%  mode=record - decoding using option [{records, [{r, [k1,k2]}]}]
                                                -> {r,1,"ab"}
```


