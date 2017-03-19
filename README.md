formulerl
=====

A math formula DSL with Erlang

What is it ?
-----
You can evaluate your formula with variables. For example:
```
if (x > 23) then
{ x^5 + 2*x^3 + 5*x^2 + 48 }
else
{ x^3 + 2*x^4 + 5*x + 48 }
```
can be calculated given the value of variable `x`.

So what's interesting ? It looks just 'yet another calculator example'.
-----
It supports three modes:

 1. `formulerl_eval` takes a parse tree and key-value pair, and evaluate the expression.
 2. `formulerl_fun` takes a parse tree and returns a fun which takes key-value pair and evaluates.
 3. `formulerl_beam` takes a user-specified module name and a parse tree,  turns the tree into a module with the specified module name, then compiles and loads it. The module has an exported function `calc/1`. It becomes available from everywhere on the node.

Why `formulerl_beam` matters ?
-----
This is an example that, when you have a large amount/number of states and don't want to keep those in your process heap, you can turn those into modules and call the functions. Then you can avoid having pools of processes or sending messages to processes with the large state.

Example
----------
```
11> {ok, Tokens, _} = formulerl_lexer:string("if (x > 23) then { x^5 + 2*x^3 + 5*x^2 + 48 } else { x^3 + 2*x^4 + 5*x + 48 }").
{ok,[{'if',1},
     {'(',1},
     {identifier,"x",1},
     {'>',1},
     {integer,23,1},
     {')',1},
     {then,1},
     {'{',1},
     {identifier,"x",1},
     {'^',1},
     {integer,5,1},
     {'+',1},
     {integer,2,1},
     {'*',1},
     {identifier,"x",1},
     {'^',1},
     {integer,3,1},
     {'+',1},
     {integer,5,1},
     {'*',1},
     {identifier,"x",1},
     {'^',1},
     {integer,2,1},
     {'+',1},
     {integer,48,...},
     {'}',...},
     {...}|...],
    1}
12> {ok, Tree} = formulerl_parser:parse(Tokens).
{ok,{'if',{'>',{identifier,"x",1},{integer,23,1}},
          {'+',{'+',{'+',{'^',{identifier,"x",1},{integer,5,1}},
                         {'*',{integer,2,1},{'^',{identifier,"x",1},{integer,3,1}}}},
                    {'*',{integer,5,1},{'^',{identifier,"x",1},{integer,2,1}}}},
               {integer,48,1}},
          {'+',{'+',{'+',{'^',{identifier,"x",1},{integer,3,1}},
                         {'*',{integer,2,1},{'^',{identifier,"x",1},{integer,4,1}}}},
                    {'*',{integer,5,1},{identifier,"x",1}}},
               {integer,48,1}}}}
13> formulerl_eval:do(Tree, dict:store("x", 2, dict:new())).
98.0
14> {ok, F} = formulerl_fun:compile(Tree).
{ok,#Fun<formulerl_fun.6.110363020>}
15> F(dict:store("x", 2, dict:new())).
98.0
16> formulerl_beam:compile(calc_example_mod, Tree).
ok
17> calc_example_mod:calc(dict:store("x", 2, dict:new())).
98.0
18>
```

Example of generated module `calc_example_mod.erl`:
-----
```
cat calc_example_mod.erl
-module(calc_example_mod).

-export([calc/1]).

calc(KVs) -> compile_if36(KVs).

compile_if36(KVs) ->
    case compile_comp3(KVs) of
      true -> compile_comp20(KVs);
      false -> compile_comp35(KVs)
    end.

compile_comp35(KVs) -> compile_comp33(KVs) + compile_integer34(KVs).

compile_integer34(_KVs) -> 48.

compile_comp33(KVs) -> compile_comp29(KVs) + compile_comp32(KVs).

compile_comp32(KVs) -> compile_integer30(KVs) * compile_id31(KVs).

compile_id31(KVs) ->
    case dict:find("x", KVs) of
      {ok, Value} -> Value;
      error -> throw(id_not_found)
    end.

compile_integer30(_KVs) -> 5.

compile_comp29(KVs) -> compile_comp23(KVs) + compile_comp28(KVs).

compile_comp28(KVs) -> compile_integer24(KVs) * compile_comp27(KVs).

compile_comp27(KVs) -> math:pow(compile_id25(KVs), compile_integer26(KVs)).

compile_integer26(_KVs) -> 4.

compile_id25(KVs) ->
    case dict:find("x", KVs) of
      {ok, Value} -> Value;
      error -> throw(id_not_found)
    end.

compile_integer24(_KVs) -> 2.

compile_comp23(KVs) -> math:pow(compile_id21(KVs), compile_integer22(KVs)).

compile_integer22(_KVs) -> 3.

compile_id21(KVs) ->
    case dict:find("x", KVs) of
      {ok, Value} -> Value;
      error -> throw(id_not_found)
    end.

compile_comp20(KVs) -> compile_comp18(KVs) + compile_integer19(KVs).

compile_integer19(_KVs) -> 48.

compile_comp18(KVs) -> compile_comp12(KVs) + compile_comp17(KVs).

compile_comp17(KVs) -> compile_integer13(KVs) * compile_comp16(KVs).

compile_comp16(KVs) -> math:pow(compile_id14(KVs), compile_integer15(KVs)).

compile_integer15(_KVs) -> 2.

compile_id14(KVs) ->
    case dict:find("x", KVs) of
      {ok, Value} -> Value;
      error -> throw(id_not_found)
    end.

compile_integer13(_KVs) -> 5.

compile_comp12(KVs) -> compile_comp6(KVs) + compile_comp11(KVs).

compile_comp11(KVs) -> compile_integer7(KVs) * compile_comp10(KVs).

compile_comp10(KVs) -> math:pow(compile_id8(KVs), compile_integer9(KVs)).

compile_integer9(_KVs) -> 3.

compile_id8(KVs) ->
    case dict:find("x", KVs) of
      {ok, Value} -> Value;
      error -> throw(id_not_found)
    end.

compile_integer7(_KVs) -> 2.

compile_comp6(KVs) -> math:pow(compile_id4(KVs), compile_integer5(KVs)).

compile_integer5(_KVs) -> 5.

compile_id4(KVs) ->
    case dict:find("x", KVs) of
      {ok, Value} -> Value;
      error -> throw(id_not_found)
    end.

compile_comp3(KVs) -> compile_id1(KVs) > compile_integer2(KVs).

compile_integer2(_KVs) -> 23.

compile_id1(KVs) ->
    case dict:find("x", KVs) of
      {ok, Value} -> Value;
      error -> throw(id_not_found)
    end.
```

Build
-----

    $ ./rebar co


