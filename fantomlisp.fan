class LObj {
  Str tag
  Num? num
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
  new makeNum(Num n) {
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
    Num? n := str.toDecimal(false)
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
      return ParseState(LObj.makeCons(makeSym("quote"),
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

  new make() {
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
      echo(lisp.printObj(lisp.read(line).obj))
      env.out().print("> ")
      env.out().flush()
      line = env.in().readLine()
    }
  }
}