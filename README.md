# jason
Work in progress...  Do not use for now.

## Overview ##

`jason` is a JSON encode/decode library written in Erlang.

What makes `jason` different ?

* Decoding use Leex/Yecc parser
* JSON object can be decoded in several formats :
   * binary struct
   * proplists
   * maps
   * records with automatic ad hoc module loaded for record handling
* No `parse_transform`

`jason`'s bias is to be able to encode and decode records at runtime,
without any code manipulation at compile time. 
This make `jason` easy to use, with a minimal footprint in your own source code.

## Data types ##
```
Erlang                       JSON                          Erlang
================================================================================

null                      -> null                       -> null
undefined                 -> null                       -> null
true                      -> true                       -> true
false                     -> false                      -> false
123                       -> 123                        -> 123
123.4                     -> 123.4                      -> 123.4
[1,2,3]                   -> [1,2,3]                    -> [1,2,3]

(WIP)

```


