-module(bin_parser_test).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
%rebar3 eunit
%string
%{name,string}
%{name,string,10}
%{name,string,n}
%{name,string,fun/3}
%fun/3 Element,Data,BinNext
%fun true - продолжить, true добавить
%fun false - завершить, true не добавить

string_int_test() ->
    Type = [{name, string, 3}],
    Bin = <<"qwe", 16#00, "123", 16#00, "abc", 16#00>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(BinNext, <<>>),
    ?assertEqual(Result, #{name => [<<"qwe">>, <<"123">>, <<"abc">>]}).

string_one_test() ->
    Type = [{name, string, 1}],
    Bin = <<"qwe", 16#00, "123", 16#00, "abc", 16#00>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(BinNext, <<"123", 16#00, "abc", 16#00>>),
    ?assertEqual(Result, #{name => [<<"qwe">>]}).

string_n_test() ->
    Type = [{name, string, n}],
    Bin = <<"a", 16#00, "b", 16#00, "c", 16#00, "d", 16#00>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(BinNext, <<>>),
    ?assertEqual(Result, #{name => [<<"a">>, <<"b">>, <<"c">>, <<"d">>]}).

string_fun_test() ->
    F = fun(El, _Data, _BinNext) -> {El =/= <<"$">>, true} end,
    Type = [{name, string, F}],
    Bin = <<"a", 16#00, "b", 16#00, "$", 16#00, "d", 16#00>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    FNoAdd = fun(El1, _Data, _BinNext) ->
        if
            (El1 =/= <<"$">>) -> {true, true};
            true -> {false, false}
        end
    end,
    {Result1, BinNext1} = bin_parser:unpack([{name, string, FNoAdd}], Bin),
    ?assertEqual(BinNext, <<"d", 16#00>>),
    ?assertEqual(BinNext, BinNext1),
    ?assertEqual(Result, #{name => [<<"a">>, <<"b">>, <<"$">>]}),
    ?assertEqual(Result1, #{name => [<<"a">>, <<"b">>]}).

%string_patern
%{name,{string_patern,$$},n}
string_patern_test() ->
    Type = [{name, {string_patern, $$}, n}],
    Bin = <<"a", $$, "b", $$, "c", $$, "d", $$>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(BinNext, <<>>),
    ?assertEqual(Result, #{name => [<<"a">>, <<"b">>, <<"c">>, <<"d">>]}).

%group
%{name,{group,param},1}
group_test() ->
    Type = [
        {header,
            {
                group,
                [{str, string, 3}],
                fun(El) ->
                    #{
                        el => maps:get(str, El, []),
                        len => length(maps:get(str, El, []))
                    }
                end
            },
            1}
    ],
    Bin = <<"a", 16#00, "b", 16#00, "c", 16#00, "d", 16#00>>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{header => [#{el => [<<"a">>, <<"b">>, <<"c">>], len => 3}]}).

group_result_test() ->
    Type = [{header, {group, [{str, string, 3}]}, r}],
    Bin = <<"a", 16#00, "b", 16#00, "c", 16#00, "1", 16#00, "2", 16#00, "3", 16#00, "d", 16#00>>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{header => #{str => [<<"a">>, <<"b">>, <<"c">>]}}).

group_2n_test() ->
    Type = [{header, {group, [{str, string, 2}]}, n}],
    Bin = <<"a", 16#00, "b", 16#00, "c", 16#00, "1", 16#00, "2", 16#00, "3", 16#00, "d", 16#00>>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{
        header =>
            [
                #{str => [<<"a">>, <<"b">>]},
                #{str => [<<"c">>, <<"1">>]},
                #{str => [<<"2">>, <<"3">>]},
                #{str => [<<"d">>]}
            ]
    }).

group_funsum1_test() ->
    F = fun(El) -> lists:sum([binary_to_integer(X) || X <- maps:get(str, El, [])]) end,
    Type = [{header, {group, [{str, string, 3}], F}, n}],
    Bin = <<"1", 16#00, "2", 16#00, "3", 16#00, "1", 16#00, "1", 16#00, "1", 16#00, "4", 16#00>>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{header => [6, 3, 4]}).

group_funsum_test() ->
    F = fun(El) -> lists:sum([binary_to_integer(X) || X <- maps:get(str, El, [])]) end,
    Type = [{header, {group, [{str, string, 3}], F}, r}],
    Bin = <<"1", 16#00, "2", 16#00, "3", 16#00, "1", 16#00, "1", 16#00, "1", 16#00, "4", 16#00>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{header => 6}),
    ?assertEqual(BinNext, <<"1", 16#00, "1", 16#00, "1", 16#00, "4", 16#00>>).

byte_number_test() ->
    Type = [{name, {bin, 4}, r}],
    Bin = <<"qwertyu">>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{name => <<"qwer">>}),
    ?assertEqual(BinNext, <<"tyu">>).

%%проверка Acc функцией если не true то erlang:throw
validator_test() ->
    Type = [
        {x, {bin, 4}, r},
        {validator, fun
            (#{x := <<"1234">>}, _) -> true;
            (_Map, _Bin) -> {no_valid, <<"1234">>}
        end}
    ],
    Bin = <<"1234qwertyu">>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{x => <<"1234">>}).

integer_8_test() ->
    Type = [{name, {int, 1}, r}],
    Bin = <<"x123">>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{name => 120}).

byte_test() ->
    Type = [{name, byte, r}],
    Bin = <<"x123">>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{name => 120}).

integer_16_test() ->
    Type = [{name, {int, 2}, r}],
    Bin = <<"x123">>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{name => 30769}).

word_test() ->
    Type = [{name, word, r}],
    Bin = <<"x1">>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{name => 12664}).

integer_24_test() ->
    Type = [{name, {int, 3}, r}],
    Bin = <<"x123">>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{name => 7876914}).

integer_32_test() ->
    Type = [{name, {int, 4}, r}],
    Bin = <<"x123456">>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{name => 2016490035}),
    ?assertEqual(BinNext, <<"456">>).

dword_test() ->
    Type = [{name, dword, r}],
    Bin = <<"x123456">>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{name => 858927480}),
    ?assertEqual(BinNext, <<"456">>).

dword_n_test() ->
    Type = [{var1, dword, n}],
    D1 = 1,
    D2 = 2,
    D3 = 3,
    D4 = 4,
    Bin =
        <<D1:32/little-signed-integer, D2:32/little-signed-integer, D3:32/little-signed-integer,
            D4:32/little-signed-integer>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{var1 => [1, 2, 3, 4]}),
    ?assertEqual(BinNext, <<>>).

signed_dword_test() ->
    Type = [{name, signed_dword, r}],
    Bin = <<16#FF, 16#FF, 16#FF, 16#FF>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{name => -1}),
    ?assertEqual(BinNext, <<>>).

qword_test() ->
    Type = [{name, qword, r}],
    Bin = <<"x123456789">>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{name => 3978425819141910904}),
    ?assertEqual(BinNext, <<"89">>).

%%запуск фунции
fun_test() ->
    Type = [
        {name,
            fun
                (<<E:1/binary, BN/binary>>) -> {binary_to_integer(E) * 2, BN};
                (_) -> {<<>>, <<>>}
            end, r}
    ],
    Bin = <<"2876">>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{name => 4}).

fun_search_test() ->
    F1 = fun(Bin) ->
        bin_parser:search(<<"TEST1:">>, Bin)
    end,
    F2 = fun(Bin) ->
        bin_parser:search(<<"TEST2:">>, Bin)
    end,
    Type = [
        {search1, F1, r},
        {str1, string, r},
        {search2, F2, r},
        {strn, string, n}
    ],
    Bin =
        <<"11111 TEST1:123", 16#00, "hhh TEST2:str111", 16#00, "str22", 16#00, "str333", 16#00,
            "vvvv">>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{
        search1 => true,
        search2 => true,
        str1 => <<"123">>,
        strn => [<<"str111">>, <<"str22">>, <<"str333">>, <<"vvvv">>]
    }).

fun_search_t_test() ->
    Type = [
        {search1, {search, <<"TEST1:">>}, r},
        {str1, string, r},
        {search2, {search, <<"TEST2:">>}, r},
        {strn, string, n}
    ],
    Bin =
        <<"11111 TEST1:123", 16#00, "hhh TEST2:str111", 16#00, "str22", 16#00, "str333", 16#00,
            "vvvv">>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{
        search1 => true,
        search2 => true,
        str1 => <<"123">>,
        strn => [<<"str111">>, <<"str22">>, <<"str333">>, <<"vvvv">>]
    }).

fun_search_t2_test() ->
    Type = [
        {search1, {search, <<"TEST:">>}, 2},
        {strn, string, n}
    ],
    Bin =
        <<"11111 TEST:123", 16#00, "hhh TEST:str111", 16#00, "str22", 16#00, "str333", 16#00,
            "vvvv">>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{
        search1 => [true, true], strn => [<<"str111">>, <<"str22">>, <<"str333">>, <<"vvvv">>]
    }).

eq_test() ->
    Type = [{eq, <<"MQ">>}],
    Bin = <<"MQ111">>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{}),
    ?assertEqual(BinNext, <<"111">>).

%%проверка
eq_string_test() ->
    Type = [{eq, <<"MQ">>}, {str, string, r}],
    Bin = <<"MQ111">>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{str => <<"111">>}),
    ?assertEqual(BinNext, <<>>).

%%пропустить указанную длину
pop_string_test() ->
    Type = [{pop, 2}, {str, string, r}],
    Bin = <<"MQ+1">>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{str => <<"+1">>}).

string_fun_end_result_test() ->
    Bin = <<"a", 16#00, "b", 16#00, "$", 16#00, "d", 16#00>>,
    F = fun(El1, Data, _BinNext) ->
        if
            (El1 =:= <<"$">>) -> {false, {true, [[1, 2, 3] | Data]}};
            true -> {true, true}
        end
    end,
    {Result1, _BinNext1} = bin_parser:unpack([{name, string, F}], Bin),
    ?assertEqual(Result1, #{name => [[1, 2, 3], <<"b">>, <<"a">>]}).

hex4_test() ->
    Type = [{name, {hex, 6}, r}],
    Bin = <<1, 2, 3, 4, 5, 6, 7, 8>>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{name => <<"010203040506">>}).

hex4_group_test() ->
    Type = [
        {var_name1, {group, [{name1, {hex, 2}, r}, {name2, {hex, 2}, r}]}, fun(
            Result, Acc, _BinNext
        ) ->
            %%выход добавить
            New =
                case maps:get(name1, Result) of
                    <<"0102">> -> Result#{flag => true};
                    _ -> Result#{flag => false}
                end,
            IsNext = maps:get(name2, Result, []) =/= <<"0708">>,
            {IsNext, {true, [New | Acc]}}
        end},
        {var_name2, binary, fun(Result, _Acc, _BinNext) ->
            {false, {true, binary:encode_hex(Result)}}
        end}
    ],
    Bin = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(BinNext, <<>>),
    ?assertEqual(Result, #{
        var_name1 =>
            [
                #{flag => false, name1 => <<"0506">>, name2 => <<"0708">>},
                #{flag => true, name1 => <<"0102">>, name2 => <<"0304">>}
            ],
        var_name2 => <<"090A0B0C">>
    }).

bit_test() ->
    Type = [{bit_var, {bit, [bit1, bit2, bit3, bit4, bit5, bit6, bit7, bit8]}, r}],
    Bin = <<88, 1, 3>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{
        bit_var => #{
            bit1 => 0, bit2 => 1, bit3 => 0, bit4 => 1, bit5 => 1, bit6 => 0, bit7 => 0, bit8 => 0
        }
    }),
    ?assertEqual(BinNext, <<1, 3>>).
bit_8_test() ->
    Type = [{bit_var, {bit, [bit1, bit2, bit3, bit4, bit5, bit6, bit7, bit8]}, r}],
    Bin = <<2#01011000>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{
        bit_var => #{
            bit1 => 0, bit2 => 1, bit3 => 0, bit4 => 1, bit5 => 1, bit6 => 0, bit7 => 0, bit8 => 0
        }
    }),
    ?assertEqual(BinNext, <<>>).

bit_x_test() ->
    Type = [{bit_var, {bit, [bit1, x, x, x, x, x, x, bit8]}, r}],
    Bin = <<2#11011001>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{
        bit_var => #{
            bit1 => 1, bit8 => 1
        }
    }),
    ?assertEqual(BinNext, <<>>).
bit_x_2_test() ->
    Type = [{bit_var, {bit, [bit1, x, x, x, x, x, x, bit8]}, 2}],
    Bin = <<2#11011001, 2#01010000>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{bit_var => [#{bit1 => 1, bit8 => 1}, #{bit1 => 0, bit8 => 0}]}),
    ?assertEqual(BinNext, <<>>).

bit_9_2_test() ->
    Type = [
        {bit_var1, {bit, [bit1, x, x, x, x, x, x, x, bit9]}, r}, {bit_var2, {bit, [bit1, bit2]}, r}
    ],
    Bin = <<2#00000000, 2#11010000>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{bit_var1 => #{bit1 => 0, bit9 => 1}, bit_var2 => #{bit1 => 1, bit2 => 0}}),
    ?assertEqual(BinNext, <<16:5>>).

bit_16_test() ->
    Type = [
        {bit_var, {bit, [b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16]}, r}
    ],
    Bin = <<2#11000000, 2#01011000>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(
        Result,
        #{
            bit_var =>
                #{
                    b1 => 1,
                    b2 => 1,
                    b3 => 0,
                    b4 => 0,
                    b5 => 0,
                    b6 => 0,
                    b7 => 0,
                    b8 => 0,
                    b9 => 0,
                    b10 => 1,
                    b11 => 0,
                    b12 => 1,
                    b13 => 1,
                    b14 => 0,
                    b15 => 0,
                    b16 => 0
                }
        }
    ),
    ?assertEqual(BinNext, <<>>).

bit_32_test() ->
    Type = [
        {bit_var,
            {bit, [
                b1,
                b2,
                b3,
                b4,
                b5,
                b6,
                b7,
                b8,
                b9,
                b10,
                b11,
                b12,
                b13,
                b14,
                b15,
                b16,
                b17,
                b18,
                b19,
                b20,
                b21,
                b22,
                b23,
                b24,
                b25,
                b26,
                b27,
                b28,
                b29,
                b30,
                b31,
                b32
            ]},
            r}
    ],
    Bin = <<2#11000000, 2#01011000, 2#10000000, 2#10000000>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(
        Result,
        #{
            bit_var =>
                #{
                    b1 => 1,
                    b2 => 1,
                    b3 => 0,
                    b4 => 0,
                    b5 => 0,
                    b6 => 0,
                    b7 => 0,
                    b8 => 0,
                    b9 => 0,
                    b10 => 1,
                    b11 => 0,
                    b12 => 1,
                    b13 => 1,
                    b14 => 0,
                    b15 => 0,
                    b16 => 0,
                    b17 => 1,
                    b18 => 0,
                    b19 => 0,
                    b20 => 0,
                    b21 => 0,
                    b22 => 0,
                    b23 => 0,
                    b24 => 0,
                    b25 => 1,
                    b26 => 0,
                    b27 => 0,
                    b28 => 0,
                    b29 => 0,
                    b30 => 0,
                    b31 => 0,
                    b32 => 0
                }
        }
    ),
    ?assertEqual(BinNext, <<>>).

bit_2_test() ->
    Type = [{bit_var, {bit, 2}, 2}],
    Bin = <<100>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{bit_var => [1, 2]}),
    ?assertEqual(BinNext, <<4:4>>).

bit_4_n_test() ->
    Type = [{bit_var, {bit, 4}, n}],
    Bin = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{bit_var => [0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 0, 8, 0, 9, 0, 10]}),
    ?assertEqual(BinNext, <<>>).

if_1_test() ->
    F1 = fun
        (#{js := <<"js">>}, _Bin) -> true;
        (_Map, _Bin) -> false
    end,
    Type = [
        {js, string, r},
        {'if', F1, [{var1, dword, r}], [{pop, 1}, {var1, dword, r}]},
        {js2, string, r}
    ],
    Bin1 = <<"js", 0, 123, 1, 2, 4, "js2", 0>>,
    {Result, _BinNext} = bin_parser:unpack(Type, Bin1),
    ?assertEqual(Result, #{js => <<"js">>, js2 => <<"js2">>, var1 => 67240315}),
    Bin2 = <<"x", 0, 123, 1, 2, 4, "js2", 0>>,
    {Result1, _BinNext2} = bin_parser:unpack(Type, Bin2),
    ?assertEqual(Result1, #{js => <<"x">>, js2 => <<"s2">>, var1 => 1778647553}).

float_test() ->
    Type = [{var_float, float, r}],
    Bin = <<1.123:32/float-little, "123">>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{var_float => 1.123}),
    ?assertEqual(BinNext, <<"123">>).

float_1_test() ->
    Type = [{var_float, float, r}],
    Bin = <<87.01:32/float-little>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{var_float => 87.01}),
    ?assertEqual(BinNext, <<>>).

float_zerro_test() ->
    Type = [{var_float, float, r}],
    Bin = <<0, 0, 0, 0>>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{var_float => 0.0}),
    ?assertEqual(BinNext, <<>>).

char_test() ->
    Type = [{var_a, char, r}],
    Bin = <<"ABCD">>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{var_a => <<"A">>}),
    ?assertEqual(BinNext, <<"BCD">>).

char_2_test() ->
    Type = [{var_a, char, r}, {var_b, char, r}, {var_n, char, n}],
    Bin = <<"ABCD">>,
    {Result, BinNext} = bin_parser:unpack(Type, Bin),
    ?assertEqual(Result, #{var_a => <<"A">>, var_b => <<"B">>, var_n => [<<"C">>, <<"D">>]}),
    ?assertEqual(BinNext, <<>>).

%   ?debugFmt("T ~p ~p", [Result,BinNext]), 
