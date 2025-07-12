# bin_parser [![Hex Version](https://img.shields.io/hexpm/v/bin_parser.svg)](https://hex.pm/packages/bin_parser)

Binary Parser
The library parses binary data using a given structure.

## Installation

The package can be installed by adding `bin_parser` to your list of dependencies
in 
`rebar.config`:
```erlang
{deps, [{bin_parser, "0.1.0"}]}.
```
`mix.exs`
```elixir
def deps do
  [{:bin_parser, "0.1.0"}]
end
```
## Basic Usage
``` erlang
    Type = [{var1, dword, r},{var2, word, 2},{name, string, n}],
    Bin = <<1,2,3,4,5,6,7,8,"qwe", 16#00, "123", 16#00, "abc", 16#00>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin).
```
 Result map:
``` erlang
    {#{name => [<<"qwe">>,<<"123">>,<<"abc">>],var1 => 67305985,var2 => [1541,2055]}.
```
look at the tests (test/bin_parser_test.erl)

## TEST

    $ rebar3 eunit


