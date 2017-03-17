Definitions.

D = [0-9]+
L = [a-z]([A-Za-z_0-9])*
S = (\s|\t|\n|\r)+ 

Rules.

and        : {token, {and_op, TokenLine}}.
or         : {token, {or_op, TokenLine}}.
if         : {token, {'if', TokenLine}}.
then       : {token, {'then', TokenLine}}.
else       : {token, {'else', TokenLine}}.
true       : {token, {'true', TokenLine}}.
false      : {token, {'false', TokenLine}}.
\(         : {token, {'(', TokenLine}}.
\)         : {token, {')', TokenLine}}.
\{         : {token, {'{', TokenLine}}.
\}         : {token, {'}', TokenLine}}.
\=\=       : {token, {'==', TokenLine}}.
\!\=       : {token, {'!=', TokenLine}}.
\<         : {token, {'<', TokenLine}}.
\>         : {token, {'>', TokenLine}}.
\<=        : {token, {'<=', TokenLine}}.
\>=        : {token, {'>=', TokenLine}}.
\^         : {token, {'^', TokenLine}}.
\+         : {token, {'+', TokenLine}}.
\-         : {token, {'-', TokenLine}}.
\*         : {token, {'*', TokenLine}}.
\/         : {token, {'/', TokenLine}}.
{D}        : {token, {integer, list_to_integer(TokenChars), TokenLine}}.
{D}\.{D}   : {token, {float, list_to_float(TokenChars), TokenLine}}.
{L}        : {token, {identifier, TokenChars, TokenLine}}.


{S}  : skip_token.

Erlang code.

