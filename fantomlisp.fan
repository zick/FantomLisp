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
      return ParseState.make(LObj.makeError("noimpl"), "")
    } else if (str[0] == kQuote) {
      return ParseState.make(LObj.makeError("noimpl"), "")
    }
    return readAtom(str)
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
      env.out().print("> ")
      env.out().flush()
      echo(lisp.read(line).obj.str)
      line = env.in().readLine()
    }
  }
}
