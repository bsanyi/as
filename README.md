# AS

## Introduction

Welcome to "as", a library for Erlang/OTP that enables users to annotate their
code with types **inside** functions. (Erlang only allows you to annotate with
typespecs in the header of your functions.)  With "as", you can add type
annotations to your code with the same syntax used in typespecs, but with the
added flexibility of being able to use it inside functions, not only to
function arguments and return values.


## How

To run the tests of "as" just run the `rebar3 proper` command in the root of
the project.

> #### WARNING {: .warning}
>
> The code in this library is not well tested in production, and may contain
> bugs or other issues. **Use at your own risk!**

To start using "as" in your project, simply add it to the dependencies and also
add the following line to your module:

```erlang
-compile([{parse_transform, as_trans}]).
```

This will activate a parse transform that enables the use of type annotations
inside functions in your module.  In other words it extends the standard Erlang
syntax with more flexible type annotations.


### Type Annotations

Type annotations in "as" follow the exact same syntax as typespecs.  For
example, you can use the following syntax to annotate a variable
`SomeExpression` with the type `list(atom() | float())`:

```erlang
SomeExpression:"list(atom() | float())"
```

This means that `SomeExpression` is annotated with the type `list(atom() |
float())`, which is a list containing atoms or floating point numbers.

You can also use the alternative syntax `as:integer(SomeExpression)` for simple
types, but this only works for the basic types defined in the `as` module. Look
at the `as` module for the complete list of available basic types. The name of
the project comes from this kind of type of annotation.  It's recommended to
use the `Expression:"..."` syntax everywhere for consistency and clarity.
On the other hand, feel free to look at the source code of the `as` module
for better understanding of the inner workings of "as".


### Operator Precedence

The parse transform (ab)uses the `Module:Function(Args)` syntax in Erlang to
make the `Expression:"...typespec..."` annotation available. (It just uses a
`"string"` in place of the `Function(...)` part, that doesn't make any sense in
erlang, that's why no one will miss it in normal Erlang code.)

Due to operator precedence reasons, the expression before the `:` sometimes has
to be put inside parentheses. For example:

```erlang
(case ... of ... end):"{ok, atom()} | {error, string()}"
```

or

```erlang
(my:spawn(...)):"timeout | pid()"
```

Parentheses are necessary in this case to ensure that the type annotation is
properly applied to the expression.  If you're unsure, just use parentheses.


### What's under the hood?

When you use the `as:integer` function or other functions in the `as` module,
they check the type of their argument at runtime. If the type is correct, the
function simply returns the argument untouched. But if the type is wrong, it
fails with a `function_clause` error. In addition these functions have proper
typespecs in the `as` module.  This means you can use them like typespecs in
your code, and Dialyzer and Eqwalizer can take advantage of them to make better
type inferences.  Here's the exact same implementation of `as:integer/1` that
you'll find in the `as` module:

```erlang
-spec integer(term()) -> integer().
integer(Int) when is_integer(Int) -> Int.

```

In other terms `as:integer/1` accepts any term as it's argument as long as that
term is actually an integer. It's not a conversion function like
`erlang:binary_to_integer/1`, since it does exactly nothing to it's argument,
it just returns it unchanged. It simply states that the argument should be
looked at "as an integer".  It is meant to say that to the best knowledge of
the programmer, this thing is an integer.

What's more, the `Expression:"integer()"` syntax is the same as calling
`as:integer(Expression)`. This is true for the simple types that have
corresponding functions in the `as` module. (non_neg_integer, pos_integer,
float, number, binary, bitstring, char, pid, reference, iolist, etc.)


For compound types, a new private function is created in your module just for
checking that type. So with "as", you can add type annotations throughout your
code and make it easier for tools to understand and check your types.  Here's
how you can imagine the generated function for the annotation
`Something:"{ok,integer()} | error"`:

```erlang
-spec 'Typed as {ok, integer()} | error'(term()) -> {ok, integer()} | error.

'Typed as {ok, integer()} | error'(error) ->
      error;

'Typed as {ok, integer()} | error'({ok, Int}) when is_integer(Int) ->
      {ok, Int}.
```

If you forgive me the strange name of this function for a second, you can see
that it has a proper typespec. This is what enables type tools to play nicely
with "as".  This function only accepts the specified type, just like the
`as:integer/1` does, so it also makes your code fail at runtime (including
testing) when the specified type is violated.

### Do annotation checks slow down my code at runtime?

It does, indeed.  If you are worried about it, here's how you can switch it
off.  Define the `as_without_runtime_type_checks` option when compiling your
code and all annotations with the `:"..."` syntax are going to be eliminated.

You can have the benefits of "as" while testing and debugging your code, and
you can just switch it off in production if performance is important.  Add this
to your `rebar.config` to go without "as":

```erlang
{erl_opts, [{d,as_without_runtime_type_checks}]}.
```


## Why should you even consider "as"?

Using type annotations has three main effects:

1. **Improved Type Inference**: Tools like Dialyzer and Eqwalizer can extract
   type information from the type annotations and make better type inference,
   which can help catch potential errors and improve code readability.
2. **Runtime Type Checking**: When the expression isn't of the specified type,
   the annotation will fail at runtime, providing a clear error message that
   highlights the issue.
3. **Improve code readability**: If used wisely it can help convey what is the
   programmer's intent.


## Conclusion

"as" provides an elegant and convenient way to add type annotations to your
Erlang/OTP code, allowing you to catch more potential errors as early as
possible. It works together with Dialyzer, Equalizer and any other tools that
rely on the standard typespecs. It fails your code early in development and
testing phases when types diverge. It is compatibile with the existing type
description language used in `-spec`s.

