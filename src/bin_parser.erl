-module(bin_parser).

-export([unpack/2, unpack/3]).

-export([
    string/1,
    string_patern/2,
    bin/2,
    binary/1,
    hex/2,
    int/2,
    byte/1,
    word/1,
    dword/1,
    big_dword/1,
    signed_dword/1,
    qword/1,
    bit/2,
    float/1
]).

-export([reverse/1, search/2]).

unpack(L, Bin) -> unpack(L, maps:new(), Bin).

unpack([], Acc, BinNext) ->
    {Acc, BinNext};
unpack(_, Acc, <<>>) ->
    {Acc, <<>>};
unpack([{pop, B} | T], Acc, Bin) ->
    <<_:B/binary, BinNext/binary>> = Bin,
    unpack(T, Acc, BinNext);
unpack([{eq, B} | T], Acc, Bin) ->
    S = size(B),
    case Bin of
        <<B:S/binary, BinNext/binary>> -> unpack(T, Acc, BinNext);
        Er -> erlang:throw(Er)
    end;
unpack([{validator, F} | T], Acc, Bin) ->
    case F(Acc, Bin) of
        true -> unpack(T, Acc, Bin);
        Er -> erlang:throw(Er)
    end;
unpack([{'if', Func, T1, F1} | T], Acc, Bin) ->
    Tpl =
        case Func(Acc, Bin) of
            true -> T1;
            _ -> F1
        end,
    {Result, BinNext} = unpack(Tpl, Acc, Bin),
    unpack(T, Result, BinNext);
unpack([Type | T], Acc, Bin) ->
    {Key, Value, BinNext} = unpack_acc(Type, [], Bin),
    unpack(T, Acc#{Key => Value}, BinNext).

unpack_acc({Key, Type}, Acc, Bin) ->
    unpack_acc({Key, Type, r}, Acc, Bin);
unpack_acc({Key, _, 0}, Acc, Bin) ->
    {Key, lists:reverse(Acc), Bin};
unpack_acc({Key, _, _}, Acc, <<>>) ->
    {Key, lists:reverse(Acc), <<>>};
unpack_acc({Key, Type, N}, Acc, Bin) ->
    {Result, BinNext} = type(Type, Bin),
    X =
        if
            is_integer(N) ->
                unpack_acc({Key, Type, N - 1}, [Result | Acc], BinNext);
            N =:= r ->
                {Key, Result, BinNext};
            is_atom(N) ->
                unpack_acc({Key, Type, N}, [Result | Acc], BinNext);
            is_function(N) ->
                R = N(Result, Acc, BinNext),
                case R of
                    %%продолжить, добавить
                    {true, true} -> unpack_acc({Key, Type, N}, [Result | Acc], BinNext);
                    {true, {true, New}} -> unpack_acc({Key, Type, N}, New, BinNext);
                    %%продолжить, не доб
                    {true, _} -> unpack_acc({Key, Type, N}, Acc, BinNext);
                    %%выход, добавить
                    {false, true} -> {Key, lists:reverse([Result | Acc]), BinNext};
                    %%выход, добавить
                    {false, {true, New}} -> {Key, New, BinNext};
                    %%выход, не доб
                    {false, _} -> {Key, lists:reverse(Acc), BinNext}
                end
        end,

    X.
%%%%
type(Type, Bin) when is_function(Type) -> Type(Bin);
type(Type, Bin) when is_atom(Type) ->
    {_Result, _BinNext} = erlang:apply(?MODULE, Type, [Bin]);
type({group, Param}, Bin) ->
    {Result, BinNext} = unpack(Param, Bin),
    {Result, BinNext};
type({group, Param, F}, Bin) when is_function(F) ->
    {Result, BinNext} = unpack(Param, Bin),
    R = F(Result),
    {R, BinNext};
type({Type, Param}, Bin) when is_atom(Type) ->
    {Result, BinNext} = erlang:apply(?MODULE, Type, [Param, Bin]),
    {Result, BinNext}.

%%TYPE
%%строка с указанным разделителем
string_patern(P, Bin) -> string_patern(P, <<>>, Bin).
string_patern(_P, Str, <<>>) -> {reverse(Str), <<>>};
string_patern(P, Str, <<P:8, BinNext/binary>>) -> {reverse(Str), BinNext};
string_patern(P, Str, <<H, BinNext/binary>>) -> string_patern(P, <<H, Str/binary>>, BinNext).

%%строка с 0x00 разделителем
string(Bin) -> string(<<>>, Bin).
string(Str, <<>>) -> {reverse(Str), <<>>};
string(Str, <<16#00, BinNext/binary>>) -> {reverse(Str), BinNext};
string(Str, <<H, BinNext/binary>>) -> string(<<H, Str/binary>>, BinNext).

%%bin число
bin(N, B) ->
    case B of
        <<Result:N/binary, BinNext/binary>> -> {Result, BinNext};
        <<>> -> {<<>>, <<>>};
        B -> bin(size(B), B)
    end.
binary(Result) -> {Result, <<>>}.

%%hex
hex(N, B) ->
    case B of
        <<Result:N/binary, BinNext/binary>> -> {binary:encode_hex(Result), BinNext};
        <<>> -> {<<>>, <<>>};
        B -> hex(size(B), B)
    end.

%%число
int(Len, Bin) ->
    <<Result:(8 * Len)/integer, BinNext/binary>> = Bin,
    {Result, BinNext}.

bit(L, Bin) when is_list(L) ->
    Size = length(L),
    <<X:Size/bitstring, BitNext/bitstring>> = Bin,
    LBit = [B || <<B:1>> <= X],
    {bit_acc(L, LBit, maps:new()), BitNext};
bit(Size, Bin) ->
    <<X:Size, BitNext/bitstring>> = Bin,
    {X, BitNext}.

bit_acc([], _, Acc) -> Acc;
bit_acc([x | KT], [_ | VT], Acc) -> bit_acc(KT, VT, Acc);
bit_acc([KH | KT], [VH | VT], Acc) -> bit_acc(KT, VT, Acc#{KH => VH}).

%BYTE = unsigned 8 bit value
byte(<<I:1/little-unsigned-integer-unit:8, BinNext/binary>>) -> {I, BinNext}.

%WORD = unsigned 16 bit value
word(<<I:2/little-unsigned-integer-unit:8, BinNext/binary>>) -> {I, BinNext}.

%DWORD = unsigned 32 bit value
dword(<<I:4/little-unsigned-integer-unit:8, BinNext/binary>>) -> {I, BinNext}.

big_dword(<<I:4/big-unsigned-integer-unit:8, BinNext/binary>>) -> {I, BinNext}.

signed_dword(<<I:4/little-signed-integer-unit:8, BinNext/binary>>) -> {I, BinNext}.

float(<<I:32/float-little, BinNext/binary>>) ->
    if
        I =:= 0.0 ->
            {0.0, BinNext};
        true ->
            Factor = math:pow(10, flooring(6 - math:log10(abs(I)))),
            {round(I * Factor) / Factor, BinNext}
    end.

%64
qword(<<I:8/little-unsigned-integer-unit:8, BinNext/binary>>) -> {I, BinNext}.

%%FUNC
search(Var, Bin) -> search(erlang:size(Var), Var, Bin).
search(I, Var, Bin) ->
    case Bin of
        <<Var:I/binary, BinNext/binary>> -> {true, BinNext};
        <<_:1/binary, BinNext/binary>> -> search(I, Var, BinNext);
        _ -> {false, <<>>}
    end.

reverse(B) -> reverse(B, <<>>).
reverse(<<>>, Acc) -> Acc;
reverse(<<H:1/binary, Rest/binary>>, Acc) -> reverse(Rest, <<H/binary, Acc/binary>>).

flooring(Value) ->
    Trunc = trunc(Value),
    if
        Trunc =< Value -> Trunc;
        %% for negative values
        Trunc > Value -> Trunc - 1
    end.
