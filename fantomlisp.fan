class LObj {
  Str tag
  Int? num
  Str? str
  LObj? car
  LObj? cdr
  LObj? args
  LObj? body
  LObj? env
  |LObj->LObj|? fn

  new makeNil() {
    this.tag = "nil"
    this.str = "nil"
  }
  new makeError(Str s) {
    this.tag = "error"
    this.str = s
  }
  new makeNum(Int n) {
    this.tag = "num"
    this.num = n
  }
  new makeSym(Str s) {
    this.tag = "sym"
    this.str = s
  }
  new makeCons(LObj a, LObj d) {
    this.tag = "cons"
    this.car = a
    this.cdr = d
  }
  new makeSubr(|LObj->LObj| fn) {
    this.tag = "subr"
    this.fn = fn
  }
  new makeExpr(LObj args, LObj env) {
    this.tag = "expr"
    this.args = args.car
    this.body = args.cdr
    this.env = env
  }
}

class ParseState {
  LObj obj
  Str next
  new make(LObj o, Str n) {
    this.obj = o
    this.next = n
  }
}

class Lisp {
  Int kLPar := '('
  Int kRPar := ')'
  Int kQuote := '\''
  LObj kNil := LObj.makeNil()
  LObj loop_val := LObj.makeNil()

  LObj safeCar(LObj obj) {
    if (obj.tag == "cons") return obj.car
    return kNil
  }
  LObj safeCdr(LObj obj) {
    if (obj.tag == "cons") return obj.cdr
    return kNil
  }

  Str:LObj sym_table := [:]
  LObj makeSym(Str str) {
    if (str == "nil")  return kNil
    if (!sym_table.containsKey(str)) {
      sym_table[str] = LObj.makeSym(str)
    }
    return sym_table[str]
  }
  LObj sym_t := makeSym("t")
  LObj sym_quote := makeSym("quote")
  LObj sym_if := makeSym("if")
  LObj sym_lambda := makeSym("lambda")
  LObj sym_defun := makeSym("defun")
  LObj sym_setq := makeSym("setq")
  LObj sym_loop := makeSym("loop")
  LObj sym_return := makeSym("return")

  LObj nreverse(LObj lst) {
    ret := kNil
    while (lst.tag == "cons") {
      tmp := lst.cdr
      lst.cdr = ret
      ret = lst
      lst = tmp
    }
    return ret
  }

  LObj pairlis(LObj lst1, LObj lst2) {
    ret := kNil
    while (lst1.tag == "cons" && lst2.tag == "cons") {
      ret = LObj.makeCons(LObj.makeCons(lst1.car, lst2.car), ret)
      lst1 = lst1.cdr
      lst2 = lst2.cdr
    }
    return nreverse(ret)
  }

  Bool isSpace(Int c) {
    return c == '\n' || c == '\r' || c == '\t' || c == ' '
  }
  Bool isDelimiter(Int c) {
    return c == kLPar || c == kRPar || c == kQuote || isSpace(c)
  }

  Str skipSpaces(Str s) {
    return Regex.fromStr("^\\s+").matcher(s).replaceFirst("")
  }

  LObj makeNumOrSym(Str str) {
    Int? n := str.toInt(10, false)
    if (n != null) {
      return LObj.makeNum(n)
    }
    return makeSym(str)
  }

  ParseState readAtom(Str str) {
    next := ""
    for (i := 0; i < str.size(); ++i) {
      if (isDelimiter(str[i])) {
        next = str[i..-1]
        str = str[0..i-1]
        break
      }
    }
    return ParseState.make(makeNumOrSym(str), next)
  }

  ParseState read(Str str) {
    str = skipSpaces(str)
    if (str.size() == 0) {
      return ParseState.make(LObj.makeError("empty input"), "")
    } else if (str[0] == kRPar) {
      return ParseState.make(LObj.makeError("invalid syntax" + str), "")
    } else if (str[0] == kLPar) {
      return readList(str[1..-1])
    } else if (str[0] == kQuote) {
      tmp := read(str[1..-1])
      return ParseState(LObj.makeCons(sym_quote,
                                      LObj.makeCons(tmp.obj, kNil)),
                        tmp.next)
    }
    return readAtom(str)
  }

  ParseState readList(Str str) {
    ret := kNil
    while (true) {
    str = skipSpaces(str)
      if (str.size() == 0) {
        return ParseState.make(LObj.makeError("unfinished parenthesis"), "")
      } else if (str[0] == kRPar) {
        break
      }
      ParseState tmp := read(str)
      if (tmp.obj.tag == "error") {
        return tmp
      }
      ret = LObj.makeCons(tmp.obj, ret)
      str = tmp.next
    }
    return ParseState.make(nreverse(ret), str[1..-1])
  }

  Str printObj(LObj obj) {
    if (obj.tag == "sym" || obj.tag == "nil") {
      return obj.str
    } else if (obj.tag == "num") {
      return obj.num.toStr()
    } else if (obj.tag == "error") {
      return "<error: " + obj.str + ">"
    } else if (obj.tag == "cons") {
      return printList(obj)
    } else if (obj.tag == "subr" || obj.tag == "expr") {
      return "<" + obj.tag + ">"
    }
    return "<unknown>"
  }

  Str printList(LObj obj) {
    ret := ""
    first := true
    while (obj.tag == "cons") {
      if (first) {
        first = false
      } else {
        ret += " "
      }
      ret += printObj(obj.car)
      obj = obj.cdr
    }
    if (obj === kNil) {
      return "(" + ret + ")"
    }
    return "(" + ret + " . " + printObj(obj) + ")";
  }

  LObj findVar(LObj sym, LObj env) {
    while (env.tag == "cons") {
      alist := env.car
      while (alist.tag == "cons") {
        if (alist.car.car === sym) {
          return alist.car
        }
        alist = alist.cdr
      }
      env = env.cdr
    }
    return kNil
  }

  Void addToEnv(LObj sym, LObj val, LObj env) {
    env.car = LObj.makeCons(LObj.makeCons(sym, val), env.car)
  }

  LObj eval(LObj obj, LObj env) {
    if (obj.tag == "nil" || obj.tag == "num" || obj.tag == "error") {
      return obj
    } else if (obj.tag == "sym") {
      bind := findVar(obj, env)
      if (bind === kNil) {
        return LObj.makeError(obj.str + " has no value")
      }
      return bind.cdr
    }

    op := safeCar(obj)
    args := safeCdr(obj)
    if (op === sym_quote) {
      return safeCar(args)
    } else if (op === sym_if) {
      cond := eval(safeCar(args), env)
      if (cond.tag == "error") {
        return cond
      } else if (cond === kNil) {
        return eval(safeCar(safeCdr(safeCdr(args))), env)
      }
      return eval(safeCar(safeCdr(args)), env)
    } else if (op === sym_lambda) {
      return LObj.makeExpr(args, env)
    } else if (op === sym_defun) {
      expr := LObj.makeExpr(safeCdr(args), env)
      sym := safeCar(args)
      addToEnv(sym, expr, g_env)
      return sym
    } else if (op === sym_setq) {
      val := eval(safeCar(safeCdr(args)), env)
      if (val.tag == "error") {
        return val
      }
      sym := safeCar(args)
      bind := findVar(sym, env)
      if (bind === kNil) {
        addToEnv(sym, val, g_env)
      } else {
        bind.cdr = val
      }
      return val
    } else if (op === sym_loop) {
      return loop(args, env)
    } else if (op === sym_return) {
      loop_val = eval(safeCar(args), env)
      return LObj.makeError("")
    }
    return apply(eval(op, env), evlis(args, env), env)
  }

  LObj evlis(LObj lst, LObj env) {
    ret := kNil
    while (lst.tag == "cons") {
      elm := eval(lst.car, env)
      if (elm.tag == "error") return elm
      ret = LObj.makeCons(elm, ret)
      lst = lst.cdr
    }
    return nreverse(ret)
  }

  LObj progn(LObj body, LObj env) {
    ret := kNil
    while (body.tag == "cons") {
      ret = eval(body.car, env)
      if (ret.tag == "error") {
        return ret
      }
      body = body.cdr
    }
    return ret
  }

  LObj loop(LObj body, LObj env) {
    while (true) {
      ret := progn(body, env)
      if (ret.tag == "error") {
        if (ret.str == "") {
          return loop_val
        }
        return ret
      }
    }
    return kNil  // Not reached.
  }

  LObj apply(LObj fn, LObj args, LObj env) {
    if (fn.tag == "error") {
      return fn
    } else if (args.tag == "error") {
      return args
    } else if (fn.tag == "subr") {
      return fn.fn(args)
    } else if (fn.tag == "expr") {
      return progn(fn.body, LObj.makeCons(pairlis(fn.args, args), fn.env))
    }
    return LObj.makeError(printObj(fn) + " is not function")
  }

  |LObj->LObj| subrCar := |LObj args -> LObj| {
    return safeCar(safeCar(args))
  }

  |LObj->LObj| subrCdr := |LObj args -> LObj| {
    return safeCdr(safeCar(args))
  }

  |LObj->LObj| subrCons := |LObj args -> LObj| {
    return LObj.makeCons(safeCar(args), safeCar(safeCdr(args)))
  }

  |LObj->LObj| subrEq := |LObj args -> LObj| {
    x := safeCar(args)
    y := safeCar(safeCdr(args))
    if (x.tag == "num" && y.tag == "num") {
      if (x.num == y.num) {
        return sym_t
      }
      return kNil
    } else if (x == y) {
      return sym_t
    }
    return kNil
  }

  |LObj->LObj| subrAtom := |LObj args -> LObj| {
    if (safeCar(args).tag == "cons") {
      return kNil
    }
    return sym_t
  }

  |LObj->LObj| subrNumberp := |LObj args -> LObj| {
    if (safeCar(args).tag == "num") {
      return sym_t
    }
    return kNil
  }

  |LObj->LObj| subrSymbolp := |LObj args -> LObj| {
    if (safeCar(args).tag == "sym") {
      return sym_t
    }
    return kNil
  }

  |LObj->LObj| subrAddOrMul(|Int,Int->Int| fn, Int init_val) {
    return |LObj args -> LObj| {
      ret := init_val
      while (args.tag == "cons") {
        if (args.car.tag != "num") return LObj.makeError("wrong type")
        ret = fn(ret, args.car.num)
        args = args.cdr
      }
      return LObj.makeNum(ret)
    }
  }

  |LObj->LObj| subrSubOrDivOrMod(|Int,Int->Int| fn) {
    return |LObj args -> LObj| {
      x := safeCar(args)
      y := safeCar(safeCdr(args))
      if (x.tag != "num" || y.tag != "num") return LObj.makeError("wrong type")
      return LObj.makeNum(fn(x.num, y.num))
    }
  }

  |LObj->LObj| subrAdd
  |LObj->LObj| subrMul
  |LObj->LObj| subrSub
  |LObj->LObj| subrDiv
  |LObj->LObj| subrMod
  LObj g_env := LObj.makeCons(kNil, kNil)
  new make() {
    subrAdd = subrAddOrMul(|Int x, Int y -> Int| { return x + y }, 0)
    subrMul = subrAddOrMul(|Int x, Int y -> Int| { return x * y }, 1)
    subrSub = subrSubOrDivOrMod(|Int x, Int y -> Int| { return x - y })
    subrDiv = subrSubOrDivOrMod(|Int x, Int y -> Int| { return x / y })
    subrMod = subrSubOrDivOrMod(|Int x, Int y -> Int| { return x % y })

    addToEnv(makeSym("car"), LObj.makeSubr(subrCar), g_env)
    addToEnv(makeSym("cdr"), LObj.makeSubr(subrCdr), g_env)
    addToEnv(makeSym("cons"), LObj.makeSubr(subrCons), g_env)
    addToEnv(makeSym("eq"), LObj.makeSubr(subrEq), g_env)
    addToEnv(makeSym("atom"), LObj.makeSubr(subrAtom), g_env)
    addToEnv(makeSym("numberp"), LObj.makeSubr(subrNumberp), g_env)
    addToEnv(makeSym("symbolp"), LObj.makeSubr(subrSymbolp), g_env)
    addToEnv(makeSym("+"), LObj.makeSubr(subrAdd), g_env)
    addToEnv(makeSym("*"), LObj.makeSubr(subrMul), g_env)
    addToEnv(makeSym("-"), LObj.makeSubr(subrSub), g_env)
    addToEnv(makeSym("/"), LObj.makeSubr(subrDiv), g_env)
    addToEnv(makeSym("mod"), LObj.makeSubr(subrMod), g_env)
    addToEnv(sym_t, sym_t, g_env)
  }
}

class Main {
  static Void main() {
    Lisp lisp := Lisp.make()
    env := Env.cur()
    env.out().print("> ")
    env.out().flush()
    line := env.in().readLine()
    while (line != null) {
      echo(lisp.printObj(lisp.eval(lisp.read(line).obj, lisp.g_env)))
      env.out().print("> ")
      env.out().flush()
      line = env.in().readLine()
    }
  }
}
