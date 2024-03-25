-module(as).

-export(
  [
    term/1,
    atom/1,
    node/1,
    module/1,
    boolean/1,
    integer/1,
    non_neg_integer/1,
    timeout/1,
    neg_integer/1,
    pos_integer/1,
    float/1,
    number/1,
    binary/1,
    bitstring/1,
    char/1,
    byte/1,
    arity/1,
    string/1,
    nonempty_string/1,
    nil/1,
    list/1,
    nonempty_list/1,
    maybe_improper_list/1,
    nonempty_maybe_improper_list/1,
    iolist/1,
    iodata/1,
    tuple/1,
    map/1,
    'fun'/1,
    function/1,
    pid/1,
    port/1,
    reference/1,
    identifier/1,
    mfa/1,
    none/1,
    nonempty_improper_list/1,
    no_return/1
  ]
).

-ifdef(OTP_RELEASE).
  -if(?OTP_RELEASE >= 24).
    -export(
      [
        nonempty_binary/1,
        nonempty_bitstring/1
      ]
    ).
  -endif.
-endif.

-spec term(term()) -> term().
term(Term) -> Term.

-spec atom(term()) -> atom().
atom(Atom) when is_atom(Atom) -> Atom.

-spec node(term()) -> node().
node(Atom) -> atom(Atom).

-spec module(term()) -> module().
module(Atom) -> atom(Atom).

-spec boolean(term()) -> boolean().
boolean(true) -> true;
boolean(false) -> false.

-spec integer(term()) -> integer().
integer(Int) when is_integer(Int) -> Int.

-spec non_neg_integer(term()) -> non_neg_integer().
non_neg_integer(Int) when is_integer(Int) andalso Int >= 0 -> Int.

-spec timeout(term()) -> timeout().
timeout(infinity) -> infinity;
timeout(Int) -> non_neg_integer(Int).

-spec neg_integer(term()) -> neg_integer().
neg_integer(Int) when is_integer(Int) andalso Int < 0 -> Int.

-spec pos_integer(term()) -> pos_integer().
pos_integer(Int) when is_integer(Int) andalso Int > 0 -> Int.

-spec float(term()) -> float().
float(Float) when is_float(Float) -> Float.

-spec number(term()) -> number().
number(Num) when is_number(Num) -> Num.

-spec binary(term()) -> binary().
binary(Bin) when is_binary(Bin) -> Bin.

-spec bitstring(term()) -> bitstring().
bitstring(Bin) when is_bitstring(Bin) -> Bin.

-spec char(term()) -> char().
char(Chr) when is_integer(Chr) andalso Chr >= 0 andalso Chr =< 16#10ffff -> Chr.

-spec byte(term()) -> byte().
byte(Byte) when is_integer(Byte) andalso Byte >= 0 andalso Byte =< 255 -> Byte.

-spec arity(term()) -> arity().
arity(Arity) -> byte(Arity).

-spec string(term()) -> string().
string(Str) -> string(Str, "").

-spec string(term(), string()) -> string().
string([Chr | Rest], Acc) -> string(Rest, [char(Chr) | Acc]);
string([], Acc) -> lists:reverse(Acc).

-spec nonempty_string(term()) -> nonempty_string().
nonempty_string(Str) when Str =/= "" -> string(Str).

-spec nil(term()) -> nil().
nil([]) -> [].

-spec list(term()) -> list().
list(List) when is_list(List) andalso length(List) >= 0 -> List.

-spec nonempty_list(term()) -> nonempty_list().
nonempty_list(List) when List =/= [] -> list(List).

-spec maybe_improper_list(term()) -> maybe_improper_list().
maybe_improper_list(List) when is_list(List) -> List.

-spec nonempty_maybe_improper_list(term()) -> nonempty_maybe_improper_list().
nonempty_maybe_improper_list(List) when List =/= [] -> maybe_improper_list(List).

-spec iolist(term()) -> iolist().
iolist([Byte | Rest]) when is_integer(Byte) andalso Byte >= 0 andalso Byte =< 255 andalso is_binary(Rest) -> [Byte | Rest];
iolist([Bin | Rest]) when is_binary(Bin) andalso is_binary(Rest) -> [Bin | Rest];
iolist([IOList | Rest]) when is_binary(Rest) -> [iolist(IOList) | Rest];
iolist([Byte | Rest]) when is_integer(Byte) andalso Byte >= 0 andalso Byte =< 255 -> [Byte | iolist(Rest)];
iolist([Bin | Rest]) when is_binary(Bin) -> [Bin | iolist(Rest)];
iolist([IOList | Rest]) -> [iolist(IOList) | iolist(Rest)];
iolist([]) -> [].
-dialyzer({no_improper_lists, iolist/1}).

-spec iodata(term()) -> iodata().
iodata(Bin) when is_binary(Bin) -> Bin;
iodata(IOList) -> iolist(IOList).

-spec tuple(term()) -> tuple().
tuple(Tuple) when is_tuple(Tuple) -> Tuple.

-spec map(term()) -> map().
map(Map) when is_map(Map) -> Map.

-spec 'fun'(term()) -> fun().
'fun'(Fun) when is_function(Fun) -> Fun.

-spec function(term()) -> function().
function(Fun) -> 'fun'(Fun).

-spec pid(term()) -> pid().
pid(Pid) when is_pid(Pid) -> Pid.

-spec port(term()) -> port().
port(Port) when is_port(Port) -> Port.

-spec reference(term()) -> reference().
reference(Ref) when is_reference(Ref) -> Ref.

-spec identifier(term()) -> identifier().
identifier(Pid) when is_pid(Pid) -> Pid;
identifier(Port) when is_port(Port) -> Port;
identifier(Ref) when is_reference(Ref) -> Ref.

-spec mfa(term()) -> mfa().
mfa({M, F, A}) -> {module(M), atom(F), arity(A)}.

-spec none(term()) -> none().
none(_) -> no_return(ever).
-dialyzer({no_return, none/1}).

-spec no_return(term()) -> no_return().
no_return(no_return) -> no_return(ever).

-ifdef(OTP_RELEASE).
  -if(?OTP_RELEASE >= 24).

-spec nonempty_binary(term()) -> nonempty_binary().
nonempty_binary(Bin) when Bin =/= <<>> -> binary(Bin).

-spec nonempty_bitstring(term()) -> nonempty_bitstring().
nonempty_bitstring(Bin) when Bin =/= <<>> -> bitstring(Bin).


  -endif.
-endif.

-spec nonempty_improper_list(term()) -> nonempty_improper_list(any(), any()).
nonempty_improper_list([Head | Rest]) when is_list(Rest) -> [Head | nonempty_improper_list(Rest)];
nonempty_improper_list([Head | Rest]) -> [Head | Rest].

