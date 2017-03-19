-module(formulerl_fun).

-export([
    compile/1
  ]).

compile(Tree) ->
  {ok, to_fun(Tree)}.

to_fun({identifier, Id, _}) ->
  fun (KVs) ->
    case dict:find(Id, KVs) of
      {ok, Value} -> Value;
      error       -> throw(id_not_found)
    end
  end;

to_fun({Num, Val, _}) when Num == float;
                           Num == integer ->
  fun (_) -> Val end;

to_fun({CompOP, LeftTree, RightTree}) when CompOP == '==';
                                           CompOP == '!=';
                                           CompOP == '<';
                                           CompOP == '>';
                                           CompOP == '<=';
                                           CompOP == '>=' ->
  LFun = to_fun(LeftTree),
  RFun = to_fun(RightTree),
  fun (KVs) ->
    case CompOP of
      '==' -> LFun(KVs) == RFun(KVs);
      '!=' -> LFun(KVs) /= RFun(KVs);
      '<'  -> LFun(KVs) <  RFun(KVs);
      '>'  -> LFun(KVs) >  RFun(KVs);
      '<=' -> LFun(KVs) =< RFun(KVs);
      '>=' -> LFun(KVs) >= RFun(KVs)
    end
  end;

to_fun({ArithOP, LeftTree, RightTree}) when ArithOP == '+';
                                            ArithOP == '-';
                                            ArithOP == '*';
                                            ArithOP == '/';
                                            ArithOP == '^' ->
  LFun = to_fun(LeftTree),
  RFun = to_fun(RightTree),
  fun (KVs) ->
    case ArithOP of
      '^' -> math:pow(LFun (KVs), RFun (KVs));
      '+' -> LFun(KVs) + RFun (KVs);
      '-' -> LFun(KVs) - RFun (KVs);
      '*' -> LFun(KVs) * RFun (KVs);
      '/' -> LFun(KVs) / RFun (KVs)
    end
  end;

to_fun({and_op, LeftTree, RightTree}) ->
  LFun = to_fun(LeftTree),
  RFun = to_fun(RightTree),
  fun (KVs) ->
    case LFun (KVs) of
      true  -> RFun (KVs);
      false -> false
    end
  end;

to_fun({or_op, LeftTree, RightTree}) ->
  LFun = to_fun(LeftTree),
  RFun = to_fun(RightTree),
  fun (KVs) ->
    case LFun (KVs) of
      true  -> true;
      false -> RFun (KVs)
    end
  end;

to_fun({'if', IfPredicate, IfConsequent, IfAlternate}) ->
  Fun1 = to_fun(IfPredicate),
  Fun2 = to_fun(IfConsequent),
  Fun3 = to_fun(IfAlternate),
  fun (KVs) ->
    case Fun1(KVs) of
      true -> Fun2(KVs);
      false -> Fun3(KVs)
    end
  end.

