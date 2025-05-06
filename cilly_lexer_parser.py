def error(src, msg):
    raise Exception(f"{src} : {msg}")


# 创建token对象，格式为[类型标签, 值]
def mk_tk(tag, val=None):
    return [tag, val]


# 获取token的类型标签
def tk_tag(t):
    return t[0]


# 获取token的值
def tk_val(t):
    return t[1]


# 创建字符流读取器，支持peek、match和next操作
def make_str_reader(s, err):
    cur = None
    pos = -1

    def peek(p=0):
        if pos + p >= len(s):
            return "eof"
        else:
            return s[pos + p]

    def match(c):
        if c != peek():
            err(f"期望{c}, 实际{peek()}")
        return next()

    def next():
        nonlocal pos, cur
        old = cur
        pos = pos + 1
        if pos >= len(s):
            cur = "eof"
        else:
            cur = s[pos]
        return old

    next()
    return peek, match, next


cilly_op1 = [
    "(",
    ")",
    "{",
    "}",
    ",",
    ";",
    "+",
    "-",
    "*",
    "/",
    "^",
    "?",
    ":",
]

cilly_op2 = {
    ">": ">=",
    "<": "<=",
    "=": "==",
    "!": "!=",
    "&": "&&",
    "|": "||",
}

cilly_keywords = [
    "var",
    "if",
    "else",
    "while",
    "break",
    "continue",
    "return",
    "fun",
    "print",
    "for",
]


# Cilly语言词法分析器：将源代码字符串转换为token序列
def cilly_lexer(prog):
    def err(msg):
        error("cilly lexer", msg)

    peek, match, next = make_str_reader(prog, err)

    def program():
        r = []
        while True:
            skip_ws()
            if peek() == "eof":
                break
            t = token()
            r.append(t)

        return r

    def token():
        c = peek()
        if is_digit(c):
            return num()
        if c == '"':
            return string()
        if c == "_" or is_alpha(c):
            return id()
        if c in cilly_op1:
            next()
            return mk_tk(c)
        if c in cilly_op2:
            next()
            if peek() == cilly_op2[c][1]:
                next()
                return mk_tk(cilly_op2[c])
            else:
                return mk_tk(c)

        err(f"非法字符{c}")

    def skip_ws():
        while True:
            c = peek()
            if c == "#":
                while c not in ["\n", "\r", "eof"]:
                    next()
                    c = peek()
                if c in ["\n", "\r"]:
                    next()
            elif c in [" ", "\t", "\r", "\n"]:
                next()
            else:
                break

    def is_digit(c):
        return c >= "0" and c <= "9"

    def num():
        r = ""
        while is_digit(peek()):
            r = r + next()
        if peek() == ".":
            r = r + next()
            while is_digit(peek()):
                r = r + next()
        return mk_tk("num", float(r) if "." in r else int(r))

    def string():
        match('"')
        r = ""
        while peek() != '"' and peek() != "eof":
            r = r + next()
        match('"')
        return mk_tk("str", r)

    def is_alpha(c):
        return (c >= "a" and c <= "z") or (c >= "A" and c <= "Z")

    def is_digit_alpha__(c):
        return c == "_" or is_digit(c) or is_alpha(c)

    def id():
        r = "" + next()
        while is_digit_alpha__(peek()):
            r = r + next()
        if r in cilly_keywords:
            return mk_tk(r)
        return mk_tk("id", r)

    return program()


EOF = mk_tk("eof")


def make_token_reader(ts, err):
    pos = -1
    cur = None

    def peek(p=0):
        if pos + p >= len(ts):
            return "eof"
        else:
            return tk_tag(ts[pos + p])

    def match(t):
        if peek() != t:
            err(f"期望{t},实际为{cur}")
        return next()

    def next():
        nonlocal pos, cur
        old = cur
        pos = pos + 1
        if pos >= len(ts):
            cur = EOF
        else:
            cur = ts[pos]
        return old

    next()
    return peek, match, next


# Cilly语言语法分析器：将token序列转换为抽象语法树(AST)
def cilly_parser(tokens):
    def err(msg):
        error("cilly parser", msg)

    peek, match, next = make_token_reader(tokens, err)

    def program():
        r = []
        while peek() != "eof":
            s = statement()
            r.append(s)
        return ["program", r]

    def statement():
        t = peek()

        if t == "var":
            return define_stat()
        if t == "id" and peek(1) == "=":
            return assign_stat()
        if t == "print":
            return print_stat()
        if t == "if":
            return if_stat()
        if t == "while":
            return while_stat()
        if t == "break":
            return break_stat()
        if t == "continue":
            return continue_stat()
        if t == "return":
            return return_stat()
        if t == "for":
            return for_stat()
        if t == "fun" and peek(1) == "id":
            return fun_statement()
        if t == "{":
            return block_stat()
        return expr_stat()

    def define_stat():
        match("var")
        id = tk_val(match("id"))
        match("=")
        e = expr()
        match(";")
        return ["define", id, e]

    def assign_stat():
        id = tk_val(match("id"))
        match("=")
        e = expr()
        if peek() != ")":
            match(";")
        return ["assign", id, e]

    def print_stat():
        match("print")
        match("(")
        if peek() == ")":
            alist = []
        else:
            alist = args()
        match(")")
        match(";")
        return ["print", alist]

    def args():
        r = [expr()]
        while peek() == ",":
            match(",")
            r.append(expr())
        return r

    def if_stat():
        match("if")
        match("(")
        cond = expr()
        match(")")
        true_stat = statement()

        if peek() == "else":
            match("else")
            false_stat = statement()
        else:
            false_stat = None

        return ["if", cond, true_stat, false_stat]

    def while_stat():
        match("while")
        match("(")
        cond = expr()
        match(")")
        body = statement()

        return ["while", cond, body]

    def break_stat():
        match("break")
        match(";")
        return ["break"]

    def continue_stat():
        match("continue")
        match(";")
        return ["continue"]

    def return_stat():
        match("return")
        if peek() == ";":
            e = None
        else:
            e = expr()
        match(";")
        return ["return", e]

    def block_stat():
        match("{")
        r = []
        while peek() != "}":
            r.append(statement())
        match("}")
        return ["block", r]

    def for_stat():
        match("for")
        match("(")
        init = statement()
        cond = statement()
        if peek() != ")":
            update = statement()
        else:
            update = None
        match(")")
        body = statement()
        return ["for", init, cond, update, body]

    def fun_statement():
        match("fun")
        name = tk_val(match("id"))
        match("(")
        params = []
        if peek() != ")":
            params = params_list()
        match(")")
        body = block_stat()
        return ["fun_def", name, params, body]

    def params_list():
        params = [tk_val(match("id"))]
        while peek() == ",":
            match(",")
            params.append(tk_val(match("id")))
        return params

    def expr_stat():
        if peek() == ";":
            match(";")
            return ["expr_stat", None]
        e = expr()
        match(";")
        return ["expr_stat", e]

    def literal(bp=0):
        return next()

    def unary(bp=0):
        op = tk_tag(next())
        e = expr(bp)

        return ["unary", op, e]

    def parens(bp=0):
        match("(")
        e = expr()
        match(")")
        return e

    # 解析函数定义：fun(参数列表){函数体}
    def fun(bp=0):
        match("fun")
        match("(")
        if peek() == ")":
            plist = []
        else:
            plist = params_list()
        match(")")
        body = block_stat()  # 函数体是块语句
        return ["fun", plist, body]

    # Pratt解析器核心数据结构
    op1 = {
        # 基础表达式解析规则（最高优先级）
        "num": (100, literal),  # 数字字面量
        "id": (100, literal),  # 标识符
        "str": (100, literal),  # 字符串
        "true": (100, literal),  # 布尔值
        "false": (100, literal),
        "null": (100, literal),
        "(": (100, parens),  # 括号表达式
        "fun": (95, fun),  # 函数定义（优先级稍低于字面量）
        # 一元运算符
        "-": (90, unary),  # 负号
        "!": (90, unary),  # 逻辑非
    }

    def get_op1_parser(t):
        if t not in op1:
            err(f"非法token:{t}")

        return op1[t]

    def binary(left, bp=0):
        op = tk_tag(next())
        right = expr(bp)
        return ["binary", op, left, right]

    # 解析三元条件表达式：条件 ? 真分支: 假分支
    def ternary(left, bp=0):
        match("?")
        true_expr = expr()  # 解析真分支（允许嵌套低优先级表达式）
        match(":")
        # 假分支使用bp-1确保右结合性：a ? b : c ? d : e 解析为 (a ? b : (c ? d : e))
        false_expr = expr(bp - 1)
        return ["ternary", left, true_expr, false_expr]

    # 解析函数调用：函数名(参数1, 参数2, ...)
    def call(fun_expr, bp=0):
        match("(")
        if peek() == ")":
            alist = []
        else:
            alist = args()
        match(")")

        return ["call", fun_expr, alist]

    op2 = {
        # 运算符优先级表（左结合优先级，右结合优先级，处理函数）
        "(": (95, 96, call),  # 函数调用（最高优先级）
        "^": (92, 91, binary),  # 幂运算（右结合）
        "*": (80, 81, binary),  # 乘法（左结合）
        "/": (80, 81, binary),  # 除法（左结合）
        "+": (70, 71, binary),  # 加法（左结合）
        "-": (70, 71, binary),  # 减法（左结合）
        ">": (60, 61, binary),  # 大于比较
        ">=": (60, 61, binary),
        "<": (60, 61, binary),
        "<=": (60, 61, binary),
        "==": (50, 51, binary),  # 等于比较
        "!=": (50, 51, binary),
        "&&": (40, 41, binary),  # 逻辑与
        "||": (30, 31, binary),  # 逻辑或
        "?": (25, 24, ternary),  # 三元条件运算符（右结合）
    }

    def get_op2_parser(t):
        if t not in op2:
            return 0, 0, None

        return op2[t]

    # Pratt解析器核心：递归解析表达式，处理运算符优先级
    def expr(bp=0):
        # 1. 解析基础表达式（数字/标识符/括号等）
        r_bp, parser = get_op1_parser(peek())
        left = parser(r_bp)
        # 2. 循环处理中缀运算符
        while True:
            # 获取当前运算符的优先级和处理函数
            l_bp, r_bp, parser = get_op2_parser(peek())
            # 优先级不足时退出循环
            if parser == None or l_bp <= bp:
                break
            # 递归解析右侧表达式（关键：r_bp决定结合方向）
            left = parser(left, r_bp)
        return left

    return program()


p1 = """
1 + (6 - 5) / 10 * 2;
1 ^ 2 * 3 ^ -3 ^ 5;
"""
p2 = '''
a>b || a<b && !(a>=c);
'''

p3 = """
    var pi = 3.1415926;
    
    var area = fun(r) {
        return pi * r * r;
    } ;
    
    print(area(10), area(20)); 
"""
p4 = """
# 递归函数
var fact = fun(n) {
        if(n == 0)
            return 1;
        else
            return n * fact(n-1);
    };
"""
p5 = """
# while循环语句
var fact2 = fun(n) {
       var r = 1;
       var i = n;
       while(n > 0) {
           r = n * r;
           n = n - 1;
       }
       return r; 
   };
"""

p6 = """
for (var i=0; i<10; i=i+1) {
    print(i);
    i=i+1;
}
"""
p7 = " 10+15!=11 ? 10:15;"
p8 = """
var a = fun(i) {
    var i = 0;
    while (i < 10) {
        i=i+1;  # 递增计数器
        if (i / 2 == 0) {
            continue;
        }
        # 使用 break 提前终止循环
        if (i == 5) {
            break;    # 跳出整个 while 循环
        }
        print(i);
    }
};
"""

p9 = """
fun fact (i,j){
    i=1;
    j=2;
}
"""
p_test_for = '''
fun fern(len) {
    if(len > 5){
        forward(len);
        right(10);
        fern(len - 10);
        left(40);
        fern(len - 10);
        right(30);
        backward(len);
    }
};

# pencolor("green");
# left(90);
# penup();
# backward(200);
# pendown();
# fern(100);
'''

tokens = cilly_lexer(p6)
ast = cilly_parser(tokens)

import pprint
#pprint.pprint(ast)
