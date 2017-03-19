-module(formulerl_eval).

-export([
    do/2
  ]).

do({identifier, Id, _}, KVs) ->
  case dict:find(Id, KVs) of
    {ok, Value} -> Value;
    error       -> throw(id_not_found)
  end;

do({Num, Val, _}, _KVs) when Num == float;
                             Num == integer ->
  Val;

do({CompOP, LeftTree, RightTree}, KVs) when CompOP == '==';
                                            CompOP == '!=';
                                            CompOP == '<';
                                            CompOP == '>';
                                            CompOP == '<=';
                                            CompOP == '>=' ->
  LVal = do(LeftTree, KVs),
  RVal = do(RightTree, KVs),
  case CompOP of
    '==' -> LVal == RVal;
    '!=' -> LVal /= RVal;
    '<'  -> LVal <  RVal;
    '>'  -> LVal >  RVal;
    '<=' -> LVal =< RVal;
    '>=' -> LVal >= RVal
  end;

do({ArithOP, LeftTree, RightTree}, KVs) when ArithOP == '+';
                                             ArithOP == '-';
                                             ArithOP == '*';
                                             ArithOP == '/';
                                             ArithOP == '^' ->
  LVal = do(LeftTree, KVs),
  RVal = do(RightTree, KVs),
  case ArithOP of
    '^' -> math:pow(LVal, RVal);
    '+' -> LVal + RVal;
    '-' -> LVal - RVal;
    '*' -> LVal * RVal;
    '/' -> LVal / RVal
  end;

do({and_op, LeftTree, RightTree}, KVs) ->
  case do(LeftTree, KVs) of
    true  -> do(RightTree, KVs);
    false -> false
  end;

do({or_op, LeftTree, RightTree}, KVs) ->
  case do(LeftTree, KVs) of
    true  -> true;
    false -> do(RightTree, KVs)
  end;

do({'if', IfPredicate, IfConsequent, IfAlternate}, KVs) ->
  case do(IfPredicate, KVs) of
    true  -> do(IfConsequent, KVs);
    false -> do(IfAlternate, KVs)
  end.

