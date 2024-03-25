-module(prop_as).

-compile([{parse_transform, as_trans}]).

-include_lib("proper/include/proper.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  MAIN

-export([all/0]).
-define(N, 1000).

all() ->
    proper:quickcheck(prop_integer()),
    proper:quickcheck(prop_anything_but_integer()),
    proper:quickcheck(prop_ints_or_atoms()),
    proper:quickcheck(prop_anything_but_ints_or_atoms()),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  PROPERTIES

prop_integer() ->
  ?FORALL(X, integer(),
          X == as:integer(X)).

prop_anything_but_integer() ->
  ?FORALL(X, anything_but(integer()),
          X =/= catch(as:integer(X))).

prop_ints_or_atoms() ->
  ?FORALL(X, oneof([integer(), atom()]),
          X == X:"integer() | atom()").

prop_anything_but_ints_or_atoms() ->
  ?FORALL(X, anything_but(oneof([integer(), atom()])),
          X =/= catch(X:"integer() | atom()")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  HELPERS

anything_but(Type) ->
  ?SUCHTHAT(Instance, term(),
            (not proper_types:is_instance(Instance, Type))).

