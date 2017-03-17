Nonterminals
  Statements
  Statement
  EqExpr
  CompExpr
  ArithExpr
  Variable.

Terminals
  '('
  ')'
  or_op
  and_op
  'if'
  'then'
  'else'
  '{'
  '}'
  '<'
  '>'
  '>='
  '<='
  '+'
  '-'
  '*'
  '/'
  '^'
  '=='
  '!='
  integer
  float
  identifier
  .

Rootsymbol
  Statements.

Nonassoc 200 '==' '!='.
Nonassoc 300 '<' '>' '<=' '>='.
Left 400 '+' '-'.
Left 500 '*' '/'.
Nonassoc 600 '^'.

Statements -> Statement and_op Statements : {and_op, '$1','$3'}.
Statements -> Statement or_op Statements  : {or_op, '$1','$3'}.
Statements -> Statement                   : '$1'. 

Statement -> 'if' Statements 'then' '{' Statements '}' 'else' '{' Statements '}' : {'if', '$2', '$5', '$9'}.
Statement -> EqExpr                       : '$1'.

EqExpr    -> EqExpr '==' EqExpr           : {'==', '$1', '$3'}.
EqExpr    -> EqExpr '!=' EqExpr           : {'!=', '$1', '$3'}.
EqExpr    -> CompExpr                     : '$1'.

CompExpr -> CompExpr '<'  CompExpr        : {'<',  '$1', '$3'}.
CompExpr -> CompExpr '>'  CompExpr        : {'>',  '$1', '$3'}.
CompExpr -> CompExpr '<=' CompExpr        : {'<=', '$1', '$3'}.
CompExpr -> CompExpr '>=' CompExpr        : {'>=', '$1', '$3'}.
CompExpr -> ArithExpr                     : '$1'.

ArithExpr -> ArithExpr '^'  ArithExpr     : {'^',  '$1', '$3'}.
ArithExpr -> ArithExpr '+'  ArithExpr     : {'+',  '$1', '$3'}.
ArithExpr -> ArithExpr '-'  ArithExpr     : {'-',  '$1', '$3'}.
ArithExpr -> ArithExpr '*'  ArithExpr     : {'*',  '$1', '$3'}.
ArithExpr -> ArithExpr '/'  ArithExpr     : {'/',  '$1', '$3'}.
ArithExpr -> '(' Statements ')'           : '$2'.
ArithExpr -> Variable                     : '$1'.

Variable -> integer                       : '$1'.
Variable -> float                         : '$1'.
Variable -> identifier                    : '$1'.
 

Endsymbol '$end'.


