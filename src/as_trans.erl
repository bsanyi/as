-module(as_trans).

-export([parse_transform/2]).

parse_transform(Forms, Options) ->
  put(module_under_compilation, Module = grab_module_name(Forms)),
  ets:new(Module, [named_table]),
  TransformerFun =
    case lists:member({d, as_without_runtime_type_checks}, Options) of
      false ->
        fun (Node) -> transform_annotations(erl_syntax:revert(Node)) end;
      true ->
        Forms,
        fun (Node) -> skip_annotations(erl_syntax:revert(Node)) end
    end,
  Result =
    lists:reverse(
      add_extra_functions(
        lists:reverse(
          erl_syntax:revert_forms(
            [erl_syntax_lib:map(TransformerFun, Form) || Form <- Forms]
          )
        )
      )
    ),
  ets:delete(Module),
  Result.

grab_module_name([{attribute, _, module, Module} | _]) ->
  Module;
grab_module_name([_ | Rest]) ->
  grab_module_name(Rest).

skip_annotations({remote, _ ,Term, {string, _, _TypeString}}) ->
  Term;
skip_annotations(Other) ->
  Other.

transform_annotations({remote, Annot1 ,Term, {string, Annot2, TypeString}}) ->
  ParsedType = as_utils:parse_type(TypeString),
  case gen(ParsedType) of
    {_, {as, F, Args}, _, _} ->
      {call, Annot1, {remote, Annot2, {atom, Annot2, as},{atom, Annot2, F}}, [Term | Args]};
    _ ->
      FunctionName = register_function(ParsedType),
      {call, Annot1, {atom, Annot2, FunctionName}, [Term]}
  end;
transform_annotations(Other) ->
  Other.

register_function(ParsedType) ->
  FunctionName = binary_to_atom(<<"Typed as ", (as_utils:pp_type(ParsedType))/binary>>),
  ets:insert(get(module_under_compilation), {FunctionName, ParsedType}),
  FunctionName.

add_extra_functions([EOF = {eof, _} | Forms]) ->
  [EOF | add_extra_functions(Forms)];
add_extra_functions(Forms) ->
  lists:foldr(
    fun ({Name, Type}, Acc) ->
      [generate_body(Name, Type), generate_spec(Name, Type) | Acc]
    end,
    Forms,
    ets:tab2list(get(module_under_compilation))
  ).

generate_spec(Name, Type) ->
  {attribute, 1, spec,
    {
      {Name, 1},
      [
        {type, 1, 'fun', [{type, 1, product, [{type, 1, term, []}]}, Type]}
      ]
    }
  }.

generate_body(Name, Type) ->
  Clauses =
    case gen(Type) of
      {_, {Mod, Fun, Args}, _, _} ->
        forward_clause(Mod, Fun, Args);
      {Clau, _, _, _} when Clau =/= nil ->
        Clau;
      {_, _, _, Func} when Func =/= nil ->
        Func
    end,
  {function, 1, Name, 1, Clauses}.

-spec clauses(iolist()) -> list().
clauses(ErlangClauses) ->
  ClausesString = erlang:binary_to_list(erlang:iolist_to_binary(ErlangClauses)),
  {ok, Tokens, _} = erl_scan:string(ClausesString),
  {ok, Forms} = erl_parse:parse_form(Tokens),
  {function, _, _Name, _, Clauses} = Forms,
  Clauses.

-spec forward_clause(module(), atom(), list()) -> list().
forward_clause(Mod, Fun, Args) ->
  M = atom_to_list(Mod),
  F = atom_to_list(Fun),
  A = io_lib:format("~w", [Args]),
  clauses(["f(Term) -> apply(", M, ", ", F, ", [Term | ", A, "])."]).

-define(TYPE(T), ?TYPE(T, [])).
-define(TYPE(T, Args), {type, _, T, Args}).
-define(ATOM(Atom), {atom, _, Atom}).
-define(INTEGER(Int), {integer, _, Int}).
-define(RANGE(From, To), {type, _, range, [{integer, _, From}, {integer, _, To}]}).
-define(LIST_OF(Type), {type, _, list, [Type]}). % ?TYPE(list, [Type])
-define(TUPLE(Types), {type, _, tuple, Types}).
-define(EMPTY_MAP, ?MAP([])).
-define(MAP(TypePairs), {type, _, map, TypePairs}).
-define(UNION_OF(Type), {type, _, union, Type}).

gen(?TYPE(term)) ->
  {
    clauses("f(Term) -> Term."),
    {as, term, []},
    true,
    nil
  };
gen(?TYPE(any)) ->
  {
    clauses("f(Term) -> Term."),
    {as, term, []},
    true,
    nil
  };
gen(?TYPE(byte)) ->
  {
    clauses("f(Byte) when is_integer(Byte) andalso Byte >= 0 andalso Byte =< 255 -> Byte."),
    {as, byte, []},
    {"is_integer(Byte) andalso Byte >= 0 andalso Byte =< 255", "Byte"},
    nil
  };
gen(?TYPE(char)) ->
  {
    clauses("f(Chr) when is_integer(Chr) andalso Chr >= 0 andalso Chr =< 16#10ffff -> Chr."),
    {as, char, []},
    {"is_integer(Chr) andalso Chr >= 0 andalso Chr =< 16#10ffff", "Chr"},
    nil
  };
gen(?TYPE(arity)) ->
  {
    clauses("f(Arity) when is_integer(Arity) andalso Arity >= 0 andalso Arity =< 255 -> Arity."),
    {as, arity, []},
    {"is_integer(Arity) andalso Arity >= 0 andalso Arity =< 255", "Arity"},
    nil
  };
gen(?TYPE(atom)) ->
  {
    clauses("f(Atom) when is_atom(Atom) -> Atom."),
    {as, atom, []},
    {"is_atom(Atom)", "Atom"},
    nil
  };
gen(?ATOM(Atom)) ->
  A = erlang:atom_to_list(Atom),
  {
    clauses(["f(", A, ") -> ", A, "."]),
    {as_utils, exactly, [Atom]},
    {["Atom =:= ", A], {"Atom", leading}},
    nil
  };
gen(?TYPE(node)) ->
  {
    clauses("f(Node) when is_atom(Node) -> Node."),
    {as, node, []},
    {"is_atom(Node)", "Node"},
    nil
  };
gen(?TYPE(module)) ->
  {
    clauses("f(Module) when is_atom(Module) -> Module."),
    {as, module, []},
    {"is_atom(Module)", "Module"},
    nil
  };
gen(?TYPE(boolean)) ->
  {
    clauses("f(true) -> true;"
            "f(false) -> false."),
    {as, boolean, []},
    {"is_boolean(Bool)", "Bool"},
    nil
  };
gen(?TYPE(integer)) ->
  {
    clauses("f(Int) when is_integer(Int) -> Int."),
    {as, integer, []},
    {"is_integer(Int)", "Int"},
    nil
  };
gen(?TYPE(non_neg_integer)) ->
  {
    clauses("f(Int) when is_integer(Int) andalso Int >= 0 -> Int."),
    {as, non_neg_integer, []},
    {"is_integer(Int) andalso Int >= 0", "Int"},
    nil
  };
gen(?TYPE(neg_integer)) ->
  {
    clauses("f(Int) when is_integer(Int) andalso Int < 0 -> Int."),
    {as, neg_intger, []},
    {"is_integer(Int) andalso Int < 0", "Int"},
    nil
  };
gen(?TYPE(pos_integer)) ->
  {
    clauses("f(Int) when is_integer(Int) andalso Int > 0 -> Int."),
    {as, pos_intger, []},
    {"is_integer(Int) andalso Int > 0", "Int"},
    nil
  };
gen(?INTEGER(Int)) ->
  I = erlang:integer_to_list(Int),
  {
    clauses(["f(", I, ") -> ", I, "."]),
    {as_utils, exactly, [Int]},
    {["Int =:= ", I], "Int"},
    nil
  };
gen(?RANGE(From, To)) ->
  F = integer_to_list(From),
  T = integer_to_list(To),
  {
    clauses(["f(Int) when is_integer(Int) andalso ", F, " =< Int andalso Int =< ", T, " -> Int."]),
    {as_utils, range, [From, To]},
    {["is_integer(Int) andalso ", F, " =< Int andalso Int =< ", T], "Int"},
    nil
  };
gen(?TYPE(timeout)) ->
  {
    clauses("f(infinity) -> infinity;"
            "f(Int) when is_integer(Int) andalso Int >= 0 -> Int."),
    {as, timeout, []},
    {"(Timeout =:= infinity orelse is_integer(Timeout) andalso Timeout >= 0)", "Timeout"},
    nil
  };
gen(?TYPE(mfa)) ->
  {
    clauses("f({M, F, A}) when is_atom(M) andalso is_atom(F)"
            "  andalso is_integer(A) andalso 0 =< A andalso A =< 255 -> {M, F, A}."),
    {as, mfa, []},
    {"is_atom(element(1, MFA)) andalso is_atom(element(2, MFA)) andalso "
     "is_integer(element(3, MFA)) andalso "
     "0 =< element(3, MFA) andalso element(3, MFA) =< 255", "MFA"},
    nil
  };
gen(?TYPE(float)) ->
  {
    clauses("f(Float) when is_float(Float) -> Float."),
    {as, float, []},
    {"is_float(Float)", "Float"},
    nil
  };
gen(?TYPE(number)) ->
  {
    clauses("f(Num) when is_number(Num) -> Num."),
    {as, number, []},
    {"is_number(Num)", "Num"},
    nil
  };
gen(?TYPE(binary)) ->
  {
    clauses("f(Bin) when is_binary(Bin) -> Bin."),
    {as, binary, []},
    {"is_binary(Bin)", "Bin"},
    nil
  };
gen(?TYPE(bitstring)) ->
  {
    clauses("f(Bin) when is_bitstring(Bin) -> Bin."),
    {as, bitstring, []},
    {"is_bitstring(Bin)", "Bin"},
    nil
  };
gen(?TYPE(string)) ->
  {
    nil,
    {as, string, []},
    nil,
    nil
  };
gen(?TYPE(list)) ->
  {
    clauses("f(List) when is_list(List) andalso length(List) >= 0 -> List."),
    {as, list, []},
    {"is_list(List) andalso length(List) >= 0", "List"},
    nil
  };
gen(?TYPE(nonempty_list)) ->
  {
    clauses("f(List) when is_list(List) andalso length(List) > 0 -> List."),
    {as, nonempty_list, []},
    {"is_list(List) andalso length(List) > 0", "List"},
    nil
  };
gen(?TYPE(maybe_improper_list)) ->
  {
    clauses("f(List) when is_list(List) -> List."),
    {as, maybe_imporper_list, []},
    {"is_list(List)", "List"},
    nil
  };
gen(?TYPE(nonempty_maybe_improper_list)) ->
  {
    clauses("f(List) when is_list(List) andalso List =/= [] -> List."),
    {as, nonempty_maybe_improper_list, []},
    {"is_list(List) andalso List =/= []", "List"},
    nil
  };
gen(?LIST_OF(Type)) ->
  case gen(Type) of
    {_, _} -> nil % FIXME
  end,
  {
    nil,
    {as_utils, list}, % FIXME
    nil,
    nil
  };
gen(?TYPE(iolist)) ->
  {
    nil,
    {as, iolist, []},
    nil,
    nil
  };
gen(?TYPE(nil)) ->
  {
    clauses("f([]) -> []."),
    {as, nil, []},
    {"X =:= []", "X"},
    nil
  };
gen(?TYPE('fun')) ->
  {
    clauses("f(Fun) when is_function(Fun) -> Fun."),
    {as, function, []},
    {"is_function(Fun)", "Fun"},
    nil
  };
gen(?TYPE(function)) ->
  {
    clauses("f(Fun) when is_function(Fun) -> Fun."),
    {as, function, []},
    {"is_function(Fun)", "Fun"},
    nil
  };
gen(?TYPE(pid)) ->
  {
    clauses("f(Pid) when is_pid(Pid) -> Pid."),
    {as, pid, []},
    {"is_pid(Pid)", "Pid"},
    nil
  };
gen(?TYPE(port)) ->
  {
    clauses("f(Port) when is_port(Port) -> Port."),
    {as, port, []},
    {"is_port(Port)", "Port"},
    nil
  };
gen(?TYPE(reference)) ->
  {
    clauses("f(Ref) when is_reference(Ref) -> Ref."),
    {as, reference, []},
    {"is_reference(Ref)", "Ref"},
    nil
  };
gen(?TYPE(identifier)) ->
  {
    clauses("f(Pid) when is_pid(Pid) -> Pid;"
            "f(Port) when is_port(Port) -> Port;"
            "f(Ref) when is_reference(Ref) -> Ref."),
    {as, identifier, []},
    {"(is_pid(Id) orelse is_port(Id) orelse is_reference(Id)", "Id"},
    nil
  };
gen(?TUPLE([])) ->
  {
    clauses("f({}) -> {}."),
    nil,
    {"EmptyTuple =:= {}", "EmptyTuple"},
    nil
  };
gen(?TUPLE(any)) ->
  {
    clauses("f(Tuple) when is_tuple(Tuple) -> Tuple."),
    {as, tuple, []},
    {"is_tuple(Tuple)", "Tuple"},
    nil
  };
gen(?TUPLE(Types)) ->
  ArgNames = lists:map(fun (I) -> "T" ++ integer_to_list(I) end, lists:seq(1, length(Types))),
  PlainTypes = lists:zip(ArgNames, Types),
  TypeList = lists:zip(ArgNames, lists:map(fun gen/1, Types)),
  Guards = filter_guards(TypeList),
  Guards1 = lists:filter(fun ({_Var, {_, _, true, _}}) -> false; (_) -> true end, Guards),
  Guards2 =
    lists:map(
      fun
        ({Var, {_, _, {GuardStr, {GuardVar, Direction}}, _}}) ->
          string:replace(GuardStr, GuardVar, Var, Direction);
        ({Var, {_, _, {GuardStr, GuardVar}, _}}) ->
          string:replace(GuardStr, GuardVar, Var, all)
      end,
      Guards1
    ),
  Guards3 =
    case Guards2 of
      [] ->
        "";
      G ->
        ["when ", lists:join(" andalso ", G)]
    end,
  Functions = filter_funs(TypeList) -- Guards,
  Functions1 = lists:map(fun ({Var, {_, {M, F, A}, _, _}}) -> {Var, {M, F, A}} end, Functions),
  Compound = filter_compound(TypeList) -- (Guards ++ Functions),
  Compound1 =
    lists:map(
      fun ({Var, _}) ->
        T = proplists:get_value(Var, PlainTypes),
        {Var, register_function(T)}
      end,
      Compound
    ),
  Elems =
    lists:map(
      fun (Var) ->
        Mfa = proplists:get_value(Var, Functions1, nil),
        Comp = proplists:get_value(Var, Compound1, nil),
        case {Mfa, Comp} of
          {{M, F, A}, _} ->
            ArgsInspect = binary_to_list(erlang:iolist_to_binary(io_lib:format("~w", [A]))),
            "apply(" ++ atom_to_list(M) ++ ", " ++ atom_to_list(F) ++ ", [" ++ Var ++ " | " ++ ArgsInspect ++ "])";
          {_, C} when C =/= nil ->
            FunName = binary_to_list(erlang:iolist_to_binary(io_lib:format("~w", [C]))),
            FunName ++ "(" ++ Var ++ ")";
          _ ->
            Var
        end
      end,
      ArgNames
    ),
  {
    nil,
    nil,
    nil,
    case Functions1 ++ Compound1 of
      [] ->
        clauses(["f({", lists:join(",", ArgNames), "}) ", Guards3, " -> "
                 "  {", lists:join(",", Elems), "}."]);
      _ ->
        clauses(["f(Term = {", lists:join(",", ArgNames), "}) ", Guards3, " -> "
                 "try "
                 "  {", lists:join(",", Elems), "} "
                 "catch _:_ -> "
                 "  erlang:error(function_clause, [Term]) "
                 "end."])
    end
  };
% maps... TODO
% gen(?EMPTY_MAP) ->
%   {
%     clauses("f(Map) when Map =:= #{} -> Map."),
%     nil,
%     {"Map =:= #{}", "Map"},
%     nil
%   };
% gen(?MAP(Associations)) ->
%   ...
gen(?UNION_OF(Types)) ->
  ArgNames = lists:map(fun (I) -> "T" ++ integer_to_list(I) end, lists:seq(1, length(Types))),
  PlainTypes = lists:zip(ArgNames, Types),
  TypeList = lists:zip(ArgNames, lists:map(fun gen/1, Types)),
  Clauses = filter_clauses(TypeList),
  Clauses1 =
    lists:flatmap(
      fun ({_, {C, _, _, _}}) -> C end,
      Clauses
    ),
  Functions = filter_funs(TypeList) -- Clauses,
  Functions1 =
    lists:map(
      fun ({_Var, {_, {M, F, A}, _, _}}) ->
        {M, F, A}
      end,
      Functions
    ),
  Compound = filter_compound(TypeList) -- (Clauses ++ Functions),
  Compound1 =
    lists:map(
      fun ({Var, _}) ->
        T = proplists:get_value(Var, PlainTypes),
        register_function(T)
      end,
      Compound
    ),
  {
    case Compound1 ++ Functions1 of
      [] -> Clauses1;
      _ -> nil
    end,
    nil,
    nil,
    Clauses1 ++
      clauses(["f(Term) -> ",
        io_lib:format("FunCalls = ~p,~n", [Functions1]),
        "  case"
        "    lists:any("
        "      fun"
        "        ({M, F, A}) -> try apply(M, F, [Term | A]) =:= Term catch _:_ -> false end"
        "    end, FunCalls)"
        "  of"
        "    true -> Term;"
        "    false ->"
        "      case",
        [["       (try ", io_lib:format("~w", [F]), "(Term) =:= Term catch _:_ -> false end) orelse "] || F <- Compound1 ], " false"
        "      of"
        "        true -> Term;"
        "        false -> erlang:error(function_clause, [Term])"
        "      end"
        "  end."])
  }.

filter_clauses(TypeList) ->
  lists:filter(
    fun
      ({_, {Clause, _, _, _}}) when Clause =/= nil ->
        true;
      (_) ->
        false
    end,
    TypeList
  ).

filter_guards(TypeList) ->
  lists:filter(
    fun
      ({_, {_, _, {_GuardString, _GuardVariable}, _}}) ->
        true;
      ({_, {_, _, true, _}}) ->
        true;
      (_) ->
        false
    end,
    TypeList
  ).

filter_funs(TypeList) ->
  lists:filter(
    fun ({_, {_, F, _, _}}) -> F =/= nil end,
    TypeList
  ).

filter_compound(TypeList) ->
  lists:filter(
    fun ({_, {_, _, _, C}}) -> C =/= nil end,
    TypeList
  ).

