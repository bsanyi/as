-module(as_utils).

-export(
  [
    parse_type/1,
    pp_type/1,
    quote/1,

    range/3, % TODO delete calls of these...
    exactly/2,

    proper_list/2,
    improper_list/3,
    tuple/2,
    union/2
  ]
).

parse_type(TypeString) ->
  {ok, T, _} = erl_scan:string("-type a_type() :: " ++ TypeString ++ "."),
  {ok, Attr} = erl_parse:parse_form(T),
  {attribute, _, type, {a_type, Type, []}} = Attr,
  Type.

-spec pp_type(erl_parse:abstract_type()) -> binary().
pp_type(ParsedType) ->
  <<"-type t() :: ", Type/binary>> =
    erlang:iolist_to_binary(
      re:replace(
        erl_pp:attribute(
          as_erl_pp_abstract_form(
            {attribute, erl_anno:new(1), type, {t, ParsedType, []}}
          )
        ),
        <<"[\s\n\r\t]+">>,
        <<"\s">>,
        [global]
      )
    ),
  binary:part(Type, {0, byte_size(Type) - 2}).

-spec as_erl_pp_abstract_form(term()) -> erl_parse:abstract_form().
as_erl_pp_abstract_form(Term) ->
  % eqwalizer:ignore
  Term.

quote(FormString) ->
  {ok, T, _} = erl_scan:string(FormString),
  {ok, FormAST} = erl_parse:parse_form(T),
  FormAST.

-spec range(integer(), integer(), term()) -> integer().
range(Term, From, To) when is_integer(Term) andalso From =< Term andalso Term =< To -> Term.

exactly(A, A) -> A.

-spec to_is(term(), fun((_) -> _)) -> boolean().
to_is(Term, Type) ->
  try
    Type(Term) =:= Term
  catch
    _:_ -> false
  end.

-spec improper_list(Subject, Type1, Type2) -> Subject | no_return()
  when
    Subject :: list(),
    Type1 ::  fun((term()) -> term()),
    Type2 :: fun((term()) -> term()).
improper_list([H | T], Type1, Type2) ->
  Type1(H),
  case to_is(T, Type2) of
    true -> [H | T];
    false -> [ H | improper_list(T, Type1, Type2)]
  end.

-spec proper_list(term(), fun((_) -> _)) -> list().
proper_list(List, Type) when is_list(List) ->
  lists:map(Type, List).

-spec tuple(term(), [fun((_) -> _)]) -> tuple() | no_return().
tuple(Tuple, Types) when is_tuple(Tuple) andalso tuple_size(Tuple) == length(Types) ->
  IndexedTypes = lists:zip(lists:seq(1, tuple_size(Tuple)), Types),
  case
    lists:all(fun ({I, Type}) -> to_is(element(I, Tuple), Type) end, IndexedTypes)
  of
    true -> Tuple;
    false -> erlang:error(function_clause, [Tuple, Types])
  end.

-spec union(term(), [fun((_) -> _)]) -> term() | no_return().
union(Term, Types) ->
  case
    lists:any(fun (Type) -> to_is(Term, Type) end, Types)
  of
    true -> Term;
    false -> erlang:error(function_clause, [Term, Types])
  end.

