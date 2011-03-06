### Type definitions (TODO symbols) ###

val priTypeSym = "sym";
val priTypeInt = "int";
val priTypeFloat = "float";
val priTypeBool = "bool";
val priTypeChar = "char";
val priTypeBinop = "binop";
val priTypeCond = "if";
val priTypeDef = "def";

### Accessor functions ###

val priNodeType = fun x -> x at 0;

val priIntResultType = fun x y ->
   let i = x at 1 in
   let j = y at 1 in
      if (i is int) and (j is int) then true
      else if (i is float) or (j is float) then false
      else "non-numeric type";

### Constructor functions ###

val priMakeSym = fun s -> [priTypeSym, s];
val priMakeInt = fun i -> [priTypeInt, i];
val priMakeFloat = fun f -> [priTypeFloat, f];
val priMakeChar = fun c -> [priTypeChar, c];
val priMakeBool = fun b -> [priTypeBool, b];
val priMakeBinOp = fun op arg1 arg2 -> [priTypeBinop, op, arg1, arg2];
val priMakeIf = fun p c a -> [priTypeCond, p, c, a];
val priMakeDef = fun s e -> [priTypeDef, s, e];

### Environment handling ###

val environmentExtend = fun env def -> def :: env;

val environmentLookup = fun env sym ->
   if env == []
   then show("symbol unbound")
   else let def = head(env) in
        let sym2 = def at 1 in
        let exp = def at 2 in
            if sym == sym2
            then exp
            else environmentLookup(tail(env), sym);

val environmentUpdate = fun ds k v ->
   if head(ds) != []
   then if head(head(ds)) == k
        then environmentExtend(tail(ds), k, v)
        else head(ds) :: environmentUpdate(tail(ds), k, v)
   else head(ds);

val symbolEq = fun sym1 sym2 ->
   let s1 = head(sym1) in
   let s2 = head(sym2) in
      s1 == s2;

val defintionEq = fun sym def ->
   let sym2 = def at 1 in symbolEq(sym, sym2);

val symbolBound = fun sym env ->
   exists (fun b -> definitionEq(sym, b), env);

val bind = fun params args env ->
   let bindOne = fun sym exp env -> priMakeDef(sym, exp) :: env in
   let rebindOne = fun sym exp env ->
                       replaceOneBy(definitionEq, sym, priMakeDef(sym, exp), env)
   in foldl(fun e b -> # b is a zipper of [sym, exp]
               let sym = b at 0 in
               let exp = b at 1 in
                   if symbolBound(sym, e)
                   then rebindOne(sym, exp, e)
                   else bindOne(sym, exp, e),
            env, zip(params, args));

### Primitive operators ###

val priIsTrue = fun exp ->
   priNodeType(exp) == priTypeBool and exp at 1 == true;

val priAdd = fun i j ->
   if priIntResultType(i, j)
   then priMakeInt((i at 1) + (j at 1))
   else priMakeFloat((i at 1) + (j at 1));

val priMul = fun i j ->
   if priIntResultType(i, j)
   then priMakeInt((i at 1) * (j at 1))
   else priMakeFloat((i at 1) * (j at 1));

val priEvalBinOp = fun exp lhs rhs ->
   if exp == "+" then priAdd(lhs, rhs)
   else if exp == "*" then priMul(lhs, rhs)
   else "unknown operator";

### Evaluator ####

val priEval = fun exp env ->
   let kind = head(exp) in
      if kind == "int" then exp
      else if kind == "float" then exp
      else if kind == "bool" then exp
      else if kind == "char" then exp
      else if kind == "sym" then priEval(environmentLookup(env, exp), env)
      else if kind == "binop" then priEvalBinOp(exp at 1,
                                                priEval(exp at 2, env),
                                                priEval(exp at 3, env),
                                                env)
      else if kind == "if" then if priIsTrue(priEval(exp at 1, env))
                                then priEval(exp at 2, env)
                                else priEval(exp at 3, env)
      else "unknown expression type";

### Test data ###

val testSym = priMakeSym("x");
val testDef = priMakeDef(testSym, priMakeInt(100));
val testInt = priMakeInt(44);
val testFloat = priMakeFloat(3.14);
val testAdd = priMakeBinOp("+", priMakeInt(5), priMakeInt(12));
val testMul = priMakeBinOp("*", priMakeFloat(5.4), testSym);
val testAddMul = priMakeBinOp("+", priMakeInt(5),
                 priMakeBinOp("*", priMakeFloat(5.4), priMakeInt(12)));
val testIf = priMakeIf(priMakeBool(true), priMakeInt(1), priMakeInt(0));

### Top-level ###

val initialToplevel = [];

# TODO the reader will return a list/stream of expressions as AST nodes
val read = [testDef, testInt, testFloat, testAdd, testMul, testAddMul, testIf];

val repl = fun exps env ->
   if exps == []
   then show("exit")
   else
      let exp = head(exps) in
      let rest = tail(exps) in
         if priNodeType(exp) == "def"
         then
            let ext = environmentExtend(env, exp) in
               repl(rest, ext)
         else
            begin
               show(priEval(exp, env)) ;
               repl(rest, env)
            end;

repl(read, initialToplevel);