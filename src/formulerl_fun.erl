-module(formulerl_fun).

-export([
    compile/1
  ]).

-include_lib( "syntax_tools/include/merl.hrl" ).


compile({identifier, Id, _}) ->
  fun (KVs) ->
    case dict:find(Id, KVs) of
      {ok, Value} -> Value;
      error       -> throw(id_not_found)
    end
  end;

compile({Num, Val, _}) when Num == float;
                            Num == integer ->
  fun (_) -> Val end;

compile({CompOP, LeftTree, RightTree}) when CompOP == '==';
                                            CompOP == '!=';
                                            CompOP == '<';
                                            CompOP == '>';
                                            CompOP == '<=';
                                            CompOP == '>=' ->
  LFun = compile(LeftTree),
  RFun = compile(RightTree),
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

compile({ArithOP, LeftTree, RightTree}) when ArithOP == '+';
                                             ArithOP == '-';
                                             ArithOP == '*';
                                             ArithOP == '/';
                                             ArithOP == '^' ->
  LFun = compile(LeftTree),
  RFun = compile(RightTree),
  fun (KVs) ->
    case ArithOP of
      '^' -> math:pow(LFun (KVs), RFun (KVs));
      '+' -> LFun(KVs) + RFun (KVs);
      '-' -> LFun(KVs) - RFun (KVs);
      '*' -> LFun(KVs) * RFun (KVs);
      '/' -> LFun(KVs) / RFun (KVs)
    end
  end;

compile({and_op, LeftTree, RightTree}) ->
  LFun = compile(LeftTree),
  RFun = compile(RightTree),
  fun (KVs) ->
    case LFun (KVs) of
      true  -> RFun (KVs);
      false -> false
    end
  end;
compile({or_op, LeftTree, RightTree}) ->
  LFun = compile(LeftTree),
  RFun = compile(RightTree),
  fun (KVs) ->
    case LFun (KVs) of
      true  -> true;
      false -> RFun (KVs)
    end
  end;

compile({'if', IfPredicate, IfConsequent, IfAlternate}) ->
  Fun1 = compile(IfPredicate),
  Fun2 = compile(IfConsequent),
  Fun3 = compile(IfAlternate),
  fun (KVs) ->
    case Fun1(KVs) of
      true -> Fun2(KVs);
      false -> Fun3(KVs)
    end
  end.

