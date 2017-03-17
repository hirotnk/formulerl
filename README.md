formulerl
=====

A math formula DSL with Erlang

What is it ?
-----
You can dynamically load your formula with variables. For example:
```
if (x > 23) then
{ x^5 + 2*x^3 + 5*x^2 + 48 }
else
{ x^3 + 2*x^4 + 5*x + 48 }
```
can be compiled into a module. Then you can calculate the value of that expressions, giving variable `x`.

Example
----------
```
33> f(), {ok, Tokens, _} = formulerl_lexer:string("if (x > 23) then { x^5 + 2*x^3 + 5*x^2 + 48 } else { x^3 + 2*x^4 + 5*x + 48 }").
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
34> {ok, Tree} = formulerl_parser:parse(Tokens).
{ok,{'if',{'>',{identifier,"x",1},{integer,23,1}},
          {'+',{'+',{'+',{'^',{identifier,"x",1},{integer,5,1}},
                         {'*',{integer,2,1},{'^',{identifier,"x",1},{integer,3,1}}}},
                    {'*',{integer,5,1},{'^',{identifier,"x",1},{integer,2,1}}}},
               {integer,48,1}},
          {'+',{'+',{'+',{'^',{identifier,"x",1},{integer,3,1}},
                         {'*',{integer,2,1},{'^',{identifier,"x",1},{integer,4,1}}}},
                    {'*',{integer,5,1},{identifier,"x",1}}},
               {integer,48,1}}}}
35> D1 = dict:store("x", 2, dict:new()).
{dict,1,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[["x"|2]]}}}
36> formulerl:compile(calc_mod1, Tree).
ok
37> calc_mod1:calc(D1).
98.0
38> D2 = dict:store("x", 24, dict:new()).
{dict,1,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[["x"|24]]}}}
39> calc_mod1:calc(D2).
7.9932e6
40>
```

Build
-----

    $ ./rebar co



