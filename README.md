# jason
Work in progress...  Do not use for now.

## Overview ##

`jason` is a JSON encode/decode library written in Erlang.

What makes `jason` different ?

* decoding use Leex/Yecc parser
* JSON object can be decoded in several formats :
   * binary struct
   * record with automatic ad hoc module loaded for record handling

## Automatic ad hoc record module ##

The problem with JSON object translated to Erlang record, is the need to
know the record structure before decoding.

`jason` solve this problem by creating and loading at runtime a module declaring
and exporting record type, and functions to manage the record.

The module and the record are a same atom computed from a hash (phash/2) on
record keys and value types.

### Exemple ###
