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

Build
-----

    $ ./rebar co


