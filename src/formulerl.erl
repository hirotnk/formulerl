-module(formulerl).

-export([
    compile/2
  ]).

-include_lib( "syntax_tools/include/merl.hrl" ).

compile (Mod, ParseTree) ->
  {ok, FuncName, Functions, _Seq} = compile(ParseTree, 1, []),
  ExecFuncBody =
    [
    "(KVs) -> '@func_name'(KVs)"
    ],
  CurrentFunc = build_function("calc", ExecFuncBody, [{func_name, FuncName}]),
  Module = ?Q("-module('@Mod@')."),
  Export = ?Q("-export([calc/1])."),
  FunctionForms = [?Q("'@_F'() -> _.") || F <- [CurrentFunc | Functions]],
  Forms = lists:flatten([Module, Export, FunctionForms]),
  file:write_file(
    lists:concat([Mod, ".erl"]),
    erl_prettypr:format(erl_syntax:form_list(Forms), [{paper,160},{ribbon,80}])
  ),
  %% Compile the module and load it.
  {ok, Mod0, Bin} = compile:forms(
      erl_syntax:revert_forms(Forms),
      [binary, debug_info, verbose, report_errors, report_warnings]),
  code:load_binary(Mod0, atom_to_list(Mod0) ++ ".erl", Bin),
  ok.


compile({identifier, Id, _}, Seq, Functions) ->
  FuncName = lists:concat(["compile_id", Seq]),
  FuncBodyList =
    [
      "(KVs) ->",
      "  case dict:find('@identifier', KVs) of",
      "    {ok, Value} -> Value;",
      "    error       -> throw(id_not_found)",
      "  end"
    ],
  Env =
    [
      {identifier, merl:term(Id)}
    ],
  CurrentFunc = build_function(FuncName, FuncBodyList, Env),
  {ok, erl_syntax:function_name(CurrentFunc), [CurrentFunc | Functions], Seq + 1};

compile({Num, Val, _}, Seq, Functions) when Num == float;
                                            Num == integer ->
  FuncName = lists:concat(["compile_float", Seq]),
  FuncBodyList =
    [
      "(_KVs) -> '@val'"
    ],
  Env =
    [
      {val, merl:term(Val)}
    ],
  CurrentFunc = build_function(FuncName, FuncBodyList, Env),
  {ok, erl_syntax:function_name(CurrentFunc), [CurrentFunc | Functions], Seq + 1};

compile({CompOP, LeftTree, RightTree}, Seq, Functions) when CompOP == '==';
                                                            CompOP == '!=';
                                                            CompOP == '<';
                                                            CompOP == '>';
                                                            CompOP == '<=';
                                                            CompOP == '>=' ->
  {ok, LFuncName, LeftAst, Seq1} = compile(LeftTree, Seq, Functions),
  {ok, RFuncName, RightAst, Seq2} = compile(RightTree, Seq1, LeftAst),
  FuncName = lists:concat(["compile_comp", Seq2]),
  FuncBodyList =
    [
      "(KVs) -> '@left_func' (KVs) "
      ++
      case CompOP of
        '!=' -> "/=";
        '<=' -> "=<";
        _    -> atom_to_list(CompOP)
      end
      ++
      " '@right_func' (KVs)"
    ],
  Env =
    [
      {left_func, LFuncName},
      {right_func, RFuncName}
    ],
  CurrentFunc = build_function(FuncName, FuncBodyList, Env),
  {ok, erl_syntax:function_name(CurrentFunc), [CurrentFunc | RightAst], Seq2 + 1};

compile({ArithOP, LeftTree, RightTree}, Seq, Functions) when ArithOP == '+';
                                                             ArithOP == '-';
                                                             ArithOP == '*';
                                                             ArithOP == '/';
                                                             ArithOP == '^' ->
  {ok, LFuncName, LeftAst, Seq1} = compile(LeftTree, Seq, Functions),
  {ok, RFuncName, RightAst, Seq2} = compile(RightTree, Seq1, LeftAst),
  FuncName = lists:concat(["compile_comp", Seq2]),
  FuncBodyList =
    [
      case ArithOP of
        '^' -> "(KVs) -> math:pow('@left_func' (KVs), '@right_func' (KVs))";
        _   -> "(KVs) -> '@left_func' (KVs) "
               ++ atom_to_list(ArithOP) ++
               " '@right_func' (KVs)"
      end
    ],
  Env =
    [
      {left_func, LFuncName},
      {right_func, RFuncName}
    ],
  CurrentFunc = build_function(FuncName, FuncBodyList, Env),
  {ok, erl_syntax:function_name(CurrentFunc), [CurrentFunc, RightAst], Seq2 + 1};

compile({and_op, LeftTree, RightTree}, Seq, Functions) ->
  {ok, LFuncName, LeftAst, Seq1} = compile(LeftTree, Seq, Functions),
  {ok, RFuncName, RightAst, Seq2} = compile(RightTree, Seq1, LeftAst),
  FuncName = lists:concat(["compile", Seq2]),
  FuncBodyList =
    [
      "(KVs) ->",
      "  case '@left_ast' (KVs) of",
      "    true -> '@right_ast' (KVs);",
      "    _    -> false",
      "  end"
    ],
  Env =
    [
      {left_ast, LFuncName},
      {right_ast, RFuncName}
    ],
  CurrentFunc = build_function(FuncName, FuncBodyList, Env),
  {ok, erl_syntax:function_name(CurrentFunc), [CurrentFunc, RightAst], Seq2 + 1};

compile({or_op, LeftTree, RightTree}, Seq, Functions) ->
  {ok, LFuncName, LeftAst, Seq1} = compile(LeftTree, Seq, Functions),
  {ok, RFuncName, RightAst, Seq2} = compile(RightTree, Seq1, LeftAst),
  FuncName = lists:concat(["compile", Seq2]),
  FuncBodyList =
    [
      "(KVs) ->",
      "  case '@left_ast' (KVs) of",
      "    true  -> true;",
      "    false -> '@right_ast' (KVs)",
      "  end"
    ],
  Env =
    [
      {left_ast, LFuncName},
      {right_ast, RFuncName}
    ],
  CurrentFunc = build_function(FuncName, FuncBodyList, Env),
  {ok, erl_syntax:function_name(CurrentFunc), [CurrentFunc, RightAst], Seq2 + 1};

compile({'if', IfPredicate, IfConsequent, IfAlternate}, Seq, Functions) ->
  {ok, FuncName1, Ast1, Seq1} = compile(IfPredicate, Seq, Functions),
  {ok, FuncName2, Ast2, Seq2} = compile(IfConsequent, Seq1, Ast1),
  {ok, FuncName3, Ast3, Seq3} = compile(IfAlternate, Seq2, Ast2),
  FuncName = lists:concat(["compile_if", Seq3]),
  FuncBodyList =
    [
      "(KVs) ->",
      "  case '@if_pred' (KVs) of",
      "    true  -> '@if_consequent' (KVs);",
      "    false -> '@if_alternate' (KVs)",
      "  end"
    ],
  Env =
    [
      {if_pred, FuncName1},
      {if_consequent, FuncName2},
      {if_alternate, FuncName3}
    ],
  CurrentFunc = build_function(FuncName, FuncBodyList, Env),
  {ok, erl_syntax:function_name(CurrentFunc), [CurrentFunc, Ast3], Seq3 + 1}.


build_function (FName, FuncBody, Env) ->
  erl_syntax:function(erl_syntax:atom(FName), [?Q(FuncBody, Env)]).
