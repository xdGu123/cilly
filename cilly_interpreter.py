class IncompleteInputError(Exception):
    pass

class SyntaxError(Exception):
    pass

def error(src, msg):
    raise Exception(f'{src} : {msg}')

def mk_tk(tag, val=None):
    return [tag, val]

def tk_tag(t):
    return t[0]

def tk_val(t):
    return t[1]

def make_str_reader(s, err):
    cur = None
    pos = -1

    def peek(p=0):
        if pos + p >= len(s):
            return 'eof'
        else:
            return s[pos + p]

    def match(c):
        if c != peek():
            err(f'期望{c}, 实际{peek()}')

        return next()

    def next():
        nonlocal pos, cur

        old = cur
        pos = pos + 1
        if pos >= len(s):
            cur = 'eof'
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
    "%",
    "^",
    "?",
    ":",
    "[",
    "]",
    ".",
]

cilly_op2 = {
    '>': '>=',
    '<': '<=',
    '=': '==',
    '!': '!=',
    '&': '&&',
    '|': '||',
}

cilly_keywords = [
    'var','print','if','else', 'while','break','continue','return','fun',
    'true', 'false', 'null', 'for',
]

def cilly_lexer(prog):
    def err(msg):
        error('cilly lexer', msg)
    peek, match, next = make_str_reader(prog, err)
    def program():
        r = []
        while True:
            skip_ws()
            if peek() == 'eof':
                break
            r.append( token() )
        return r

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

    def token():
        c = peek()
        if is_digit(c):
            return num()
        if c == '"':
            return string()
        if c == '_' or is_alpha(c):
            return id()
        if c in cilly_op1 :
            next()
            return mk_tk(c)
        if c in cilly_op2:
            next()
            if peek() == cilly_op2[c][1]:
                next()
                return mk_tk(cilly_op2[c])
            else:
                return mk_tk(c)
        err(f'非法字符{c}')

    def is_digit(c):
        return c >= '0' and c <= '9'

    def num():
        r = ''
        while is_digit(peek()):
            r = r + next()
        if peek() == '.':
            r = r + next()
            while is_digit(peek()):
                r = r + next()
        return mk_tk('num', float(r) if '.' in r else int(r))

    def string():
        match('"')
        r = ''
        while peek() != '"' and peek() != 'eof':
            r = r + next()
        match('"')
        return mk_tk('str', r)

    def is_alpha(c):
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z')

    def is_digit_alpha__(c):
        return c == '_' or is_digit(c) or is_alpha(c)

    def id():
        r = '' + next()
        while is_digit_alpha__(peek()):
            r = r + next()
        if r in cilly_keywords:
            return mk_tk(r)
        return mk_tk('id', r)
    return program()
            
EOF = mk_tk('eof')

def make_token_reader(ts, err):
    pos = -1
    cur = None

    def peek(p=0):
        if pos + p >= len(ts):
            return 'eof'
        else:
            return tk_tag(ts[pos + p])

    def match(t):
        if peek() != t:
            err(f'期望{t},实际为{cur}')

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
        
def cilly_parser(tokens):
    def err(msg):
        error('cilly parser',  msg)
        
    peek, match, next = make_token_reader(tokens, err)
    
    def program():
        r = []
        while peek() != 'eof':
            r.append( statement() )
        return ['program', r]
    
    def statement():
        t = peek()
        if t == 'var':
            return define_stat()
        if is_assignable():
            return assign_stat()
        if t == 'print':
            return print_stat()
        if t == 'if':
            return if_stat()
        if t == 'while':
            return while_stat()
        if t == 'break':
            return break_stat()
        if t == 'continue':
            return continue_stat()
        if t == 'return':
            return return_stat()
        if t == '{':
            return block_stat()
        if t == "for":
            return for_stat()
        if t == "fun" and peek(1) == "id":
            return fun_statement()
        return expr_stat()

    def is_assignable():
        # 检查是否是 a = ..., a[0] = ..., obj.prop = ...
        if peek() == "id" and peek(1) == "=":
            return True
        if peek() == "id" and peek(1) in ["[", "."]:
            # 读取完整的复合表达式，看它后面是否是 =
            i = 0
            depth = 0
            while True:
                tag = peek(i)
                if tag in ("[", "."):
                    depth += 1
                elif tag == "=" and depth > 0:
                    return True
                elif tag in (";", "eof"):
                    return False
                i += 1

    def define_stat():
        match('var')
        id = tk_val ( match('id') )
        match('=')
        e = expr()
        match(';')
        return ['define', id, e]

    # 赋值语句
    def assign_stat():
        target = expr()
        match("=")
        e = expr()
        if peek() != ")":
            match(";")
        return ["assign", target, e]

    def print_stat():
        match('print')
        match('(')
        if peek() == ')':
            alist = []
        else:
            alist = args()
        match(')')
        match(';')
        return ['print', alist]
    
    def args():
        r = [expr()]
        while peek() == ',':
            match(',')
            r.append( expr() )
        return r
    
    def if_stat(): # if ( expr ) statement (else statment)?
        match('if')
        match('(')
        cond = expr()
        match(')')
        true_stat = statement()
        if peek() == 'else':
            match('else')
            false_stat = statement()
        else:
            false_stat = None
        return ['if', cond , true_stat, false_stat]
    
    def while_stat():
        match('while')
        match('(')
        cond = expr()
        match(')')
        body = statement()
        return ['while', cond, body]
    
    def continue_stat():
        match('continue')
        match(';')
        return ['continue']
    
    def break_stat():
        match('break')
        match(';')
        return ['break']
    
    def return_stat():
        match('return')
        if peek() != ';':
            e = expr()
        else:
            e = None
        match(';')
        return ['return', e]

    def block_stat():
        match('{')
        r = []
        while peek() != '}':
            r.append( statement() )
        match('}')
        return ['block', r]

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
            return ["expr_stat", ["null"]]  # 返回null节点
        e = expr()
        match(";")
        return ["expr_stat", e]
    
    def literal(bp=0):
        return next()
    
    def unary(bp=0):
        op = tk_tag( next() )
        e = expr(bp)
        return ['unary', op, e]
    
    def fun_expr(bp=0):
        match('fun')
        match( '(' )
        if peek() == ')':
            plist = []
        else:
            plist = params()
        match(')')
        body = block_stat()
        return ['fun', plist, body]
    
    def params():
        r = [ tk_val( match('id') )]
        while peek() == ',':
            match(',')
            r.append ( tk_val( match('id') ) )
        return r

    def array_literal(bp=0):
        match("[")
        elements = []
        if peek() != "]":
            elements.append(expr())
            while peek() == ",":
                match(",")
                elements.append(expr())
        match("]")
        return ["array", elements]

    def object_literal(bp=0):
        match("{")
        pairs = []
        if peek() != "}":
            pairs.append(object_pair())
            while peek() == ",":
                match(",")
                pairs.append(object_pair())
        match("}")
        return ["object", pairs]


    def index_expr(arr_expr, bp=0):
        match("[")
        index = expr()
        match("]")
        return ["index", arr_expr, index]

    def object_pair():
        key = tk_val(match("id"))
        match(":")
        val_expr = expr()
        return [key, val_expr]

    def prop_expr(obj_expr, bp=0):
        match(".")
        name = tk_val(match("id"))
        return ["prop", obj_expr, name]

    def parens(bp=0):
        match('(')
        e = expr()
        match(')')
        return e
    
    op1 = {
        # 基础表达式解析规则（最高优先级）
        "num": (100, literal),  # 数字字面量
        "id": (100, literal),  # 标识符
        "str": (100, literal),  # 字符串
        "true": (100, literal),  # 布尔值
        "false": (100, literal),
        "null": (100, literal),
        "(": (100, parens),  # 括号表达式
        "[": (100, array_literal),
        "{": (100, object_literal),
        "fun": (95, fun_expr),  # 函数定义（优先级稍低于字面量）
        # 一元运算符
        "-": (90, unary),  # 负号
        "!": (90, unary),  # 逻辑非
    }
    
    def get_op1_parser(t):
        if t not in op1:
            err(f'非法token: {t}')
        return op1[t]

    def binary(left, bp):
        op = tk_tag( next() )
        right = expr(bp)
        return ['binary', op, left, right]

    # 解析三元条件表达式：条件 ? 真分支: 假分支
    def ternary(left, bp=0):
        match("?")
        true_expr = expr()  # 解析真分支（允许嵌套低优先级表达式）
        match(":")
        # 假分支使用bp-1确保右结合性：a ? b : c ? d : e 解析为 (a ? b : (c ? d : e))
        false_expr = expr(bp - 1)
        return ["ternary", left, true_expr, false_expr]
    
    def call(fun_expr, bp=0):
        match('(')
        if peek() != ')':
            alist = args()
        else:
            alist = []
        match(')')
        return ['call', fun_expr, alist]
    
    op2 = {
        # 运算符优先级表（左结合优先级，右结合优先级，处理函数）
        "(": (95, 96, call),  # 函数调用（最高优先级）
        "[": (95, 96, index_expr),  # 数组下标调用
        ".": (95, 96, prop_expr),  # 结构体元素调用
        "^": (92, 91, binary),  # 幂运算（右结合）
        "*": (80, 81, binary),  # 乘法（左结合）
        "/": (80, 81, binary),  # 除法（左结合）
        "%": (75, 76, binary),  # 除法（左结合）
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
            return (0,0,None)
        else:
            return op2[t]
            
    def expr(bp = 0):
        r_bp, parser = get_op1_parser( peek() )
        left = parser(r_bp)
        while True:
            l_bp, r_bp, parser = get_op2_parser( peek() )
            if parser == None or l_bp <= bp:
                break
            left = parser(left, r_bp)
        return left
    return program()


def mk_num(i):
    return ['num', i]

def mk_str(s):
    return ['str', s]

def mk_proc(params, body, env):
    return ['proc', params, body, env]

def mk_primitive_proc(f):
    return ['primitive', f]

TRUE = ['bool', True]
FALSE = ['bool', False]

def mk_bool(b):
    return TRUE if b else FALSE

NULL = ['null', None]

def val(v):
    if isinstance(v, list):
        tag = v[0]
        if tag in ("num", "str", "string"):
            return v[1]
    return v

# environment : ( {x:1,y:2,...}, parent_env)
def lookup_var(env, name):
    while env:
        e, env = env
        if name in e:
            return e[name]
    error('lookup var', f'变量未定义{name}')

def set_var(env, name, val):
    while env:
        e, env = env
        if name in e:
            e[name] = val
            return
    error('set var', f'变量未定义{name}')
    
def define_var(env, name, val):
    e, _ = env
    if name in e:
        error('define var', f'变量已定义{name}')
    e[name] = val
    
def extend_env(vars, vals, env):
    e = {var:val for (var, val) in zip(vars, vals)}
    return (e, env)

    
env = ({}, None)

def cilly_eval(ast, env):
    def err(msg):
        return error('cilly eval', msg)
    
    def ev_program(node, env):
        _, statements = node
        r = NULL
        for s in statements:
            r = visit(s, env)
        return r

    def ev_expr_stat(node, env):
        _, e = node
        if e[0] == "null":
            return NULL
        return visit(e, env)
    
    
    def ev_print(node, env):
        _, args = node
        for a in args:
            print( val( visit(a, env) ) , end=' ')
        print('')
        return NULL
        
    def ev_literal(node, env):
        tag, val = node
        if tag in ['num','str']:
            return node
        if tag in ['true', 'false']:
            return TRUE if tag == 'true' else FALSE
        if tag == 'null':
            return NULL
        err(f'非法字面量{node}')

    def ev_unary(node, env):
        _, op, e = node
        v = val( visit(e, env) )
        if op == '-':
            return mk_num( -v )
        if op == '!':
            return mk_bool( not(v) )
        
        err(f'非法一元运算符{op}')
        
    def ev_binary(node, env):
        _, op, e1, e2 = node
        
        v1 = val( visit(e1, env) )
        if op == '&&':
            if v1 == False:
                return FALSE
            else:
                return visit(e2, env)
        if op == '||':
            if v1 == True:
                return TRUE
            else:
                return visit(e2, env)
                
        v2 = val( visit(e2, env) )
        if op == '+':
            # Cilly 中你允许 "name:" + age，这就需要我们在解释器的 + 运算里自动将数字转换为字符串。
            if isinstance(v1, str) or isinstance(v2, str):
                return mk_str(str(v1) + str(v2))  #  自动转换为字符串拼接
            return mk_num(v1 + v2)

        if op == '-':
            return mk_num( v1 - v2 )
        if op == '*':
            return mk_num( v1 * v2 )
        if op == '/':
            return mk_num( v1 / v2 )
        if op == '%':
            return mk_num( v1 % v2 )
        if op == '^':
            return mk_num( v1 ** v2 )
        if op == '>':
            return mk_bool( v1 > v2 )
        if op == '>=':
            return mk_bool( v1 >= v2 )
        if op == '<':
            return mk_bool( v1 < v2 )
        if op == '<=':
            return mk_bool( v1 <= v2 )
        if op == '==':
            return mk_bool( v1 == v2 )
        if op == '!=':
            return mk_bool( v1 != v2 )
        err(f'非法二元运算符{op}')

    def ev_ternary(node, env):
        _, cond, true_expr, false_expr = node
        cond_val = visit(cond, env)
        if cond_val == TRUE:
            return visit(true_expr, env)
        else:
            return visit(false_expr, env)

    def ev_if(node, env):
        _, cond, true_stat, false_stat = node
        
        if visit(cond, env) == TRUE:
            return visit(true_stat, env)
        
        if false_stat != None:
            return visit(false_stat, env)
        
        return NULL
    
    def ev_while(node, env):
        _, cond, body = node
    
        r = NULL
        prev_r = NULL
        while visit(cond, env) == TRUE:
            r = visit(body, env)
            if r[0] == 'continue':
                continue
            if r[0] == 'break':
                r = prev_r
                break
            prev_r = r
        return r

    def ev_for(node, env):
        _, init, cond, update, body = node
        # 执行初始化语句
        visit(init, env)
        result = NULL
        while True:
            # 检查条件
            cond_val = visit(cond, env)
            if cond_val != TRUE:
                break
            # 执行循环体
            result = visit(body, env)
            # 处理break/continue
            if result[0] == "break":
                break
            if result[0] == "continue":
                pass  # 继续执行update
            # 执行更新语句
            if update is not None:
                visit(update, env)
        return result


    def ev_break(node, env):
        return ['break']
    
    def ev_continue(node, env):
        return ['continue']
    
    def ev_block(node, env):
        _, statements = node
        
        r = NULL
        
        block_env = extend_env({}, {}, env)
        
        for s in statements:
    
            r = visit(s, block_env)
            if r[0] in ['break', 'continue', 'return']:
                return r
            
        return r
    
    def ev_id(node, env):
        _, name = node
        
        return lookup_var(env, name)
    
    def ev_define(node, env):
        _, name, e = node
        
        v = visit(e, env)
        
        define_var(env, name, v)
        
        return NULL

    def ev_assign(node, env):
        _, target, e = node
        value = visit(e, env)

        if target[0] == 'id':
            name = target[1]
            set_var(env, name, value)
            return NULL

        elif target[0] == 'index':
            _, arr_expr, index_expr = target
            arr = visit(arr_expr, env)
            index = val(visit(index_expr, env))
            if arr[0] != 'array_val':
                err("只能对数组进行下标赋值")
            arr[1][index] = value  # 直接修改数组的值
            return NULL

        elif target[0] == 'prop':
            _, obj_expr, name = target
            obj = visit(obj_expr, env)
            if obj[0] != 'object_val':
                err("只能对对象字段赋值")
            obj[1][name] = value  # 直接修改对象的属性值
            return NULL

        else:
            err(f"不支持的赋值目标：{target}")

    def ev_fun(node, env):
        _, params, body = node
        
        return mk_proc(params, body, env)

    def ev_fun_def(node, env):
        _, name, params, body = node
        # 创建闭包并绑定到当前环境
        proc = mk_proc(params, body, env)
        define_var(env, name, proc)
        return NULL

    def ev_return(node, env):
        _, e = node
        
        if e != None:
            return visit(e, env)
        else:
            return NULL

    def ev_call(node, env):
        _, f_expr, args = node

        # 处理 obj.method(...) 的情况
        is_method = f_expr[0] == "prop"
        this_obj = None
        if is_method:
            _, obj_expr, method_name = f_expr
            obj_val = visit(obj_expr, env)
            if obj_val[0] != "object_val":
                err("试图调用非对象的方法")
            this_obj = obj_val
            proc = obj_val[1].get(method_name)
            if proc is None:
                err(f"对象没有方法 {method_name}")
        else:
            proc = visit(f_expr, env)

        if proc[0] not in ["proc", "primitive"]:
            err(f"非法函数调用：{proc}")

        if proc[0] == "primitive":
            f = proc[1]
            eval_args = [val(visit(a, env)) for a in args]
            return f(*eval_args)

        # 正常闭包函数调用
        _, params, body, saved_env = proc
        eval_args = [visit(a, env) for a in args]

        # 如果是方法调用，则自动注入 this 到参数表首位（模拟绑定）
        if is_method:
            params = ["this"] + params
            eval_args = [this_obj] + eval_args

        call_env = extend_env(params, eval_args, saved_env)
        result = visit(body, call_env)
        return result[1] if result[0] == "return" else result

    def ev_array(node, env):
        _, elements = node
        values = [visit(e, env) for e in elements]
        return ['array_val', values]

    def ev_object(node, env):
        _, pairs = node
        obj = {}
        for key, e in pairs:
            obj[key] = visit(e, env)
        return ['object_val', obj]

    # 数组下标索引
    def ev_index(node, env):
        _, arr_expr, index_expr = node
        arr = visit(arr_expr, env)
        index = val(visit(index_expr, env))
        if arr[0] != 'array_val':
            err("试图索引非数组对象")
        return arr[1][index]

    def ev_prop(node, env):
        _, obj_expr, name = node
        obj = visit(obj_expr, env)
        if obj[0] != 'object_val':
            err("试图访问非对象属性")
        if name not in obj[1]:
            err(f"对象没有属性：{name}")
        return obj[1][name]

    visitors = {
        'program': ev_program,
        'expr_stat': ev_expr_stat,
        'print': ev_print,
        'if': ev_if,
        'for': ev_for,
        'while': ev_while,
        'break': ev_break,
        'continue': ev_continue,
        
        'define': ev_define,
        'assign': ev_assign,

        'block': ev_block,

        'array': ev_array,  # 数组
        'object': ev_object,  # 结构体
        'index': ev_index,  # 数组下标调用解析
        'prop': ev_prop,  # 结构体元素调用解析

        'unary': ev_unary,
        'binary': ev_binary,
        "ternary": ev_ternary,

        'id': ev_id,

        "fun_def": ev_fun_def,
        'fun': ev_fun,
        'return': ev_return,
        'call': ev_call,
        
        'num': ev_literal,
        'str': ev_literal,
        'true': ev_literal,
        'false': ev_literal,
        'null': ev_literal,
    }

    def visit(node, env):
        if node is None:
            err("AST节点不能为None")
        t = node[0]
        if t not in visitors:
            err(f'非法节点{node}')
        return visitors[t](node, env)
    
    return visit(ast, env)

def greet(name):
    print("hello " + name)
    return NULL


import turtle
import matplotlib.pyplot as plt
import matplotlib
matplotlib.rcParams['font.family'] = 'SimHei'
matplotlib.rcParams['axes.unicode_minus'] = False


def turtle_forward(x):
    turtle.forward(val(x))
    return NULL

def turtle_backward(x):
    turtle.backward(val(x))
    return NULL

def turtle_right(x):
    turtle.right(val(x))
    return NULL

def turtle_left(x):
    turtle.left(val(x))
    return NULL

def turtle_pendown():
    turtle.pendown()
    return NULL

def turtle_penup():
    turtle.penup()
    return NULL

def turtle_pencolor(color):
    turtle.pencolor(val(color))
    return NULL

def matplotlib_plot(x, y, label):
    # 将Cilly数组转换为Python列表
    x_py = [val(e) for e in x[1]] if x[0] == 'array_val' else val(x)
    y_py = [val(e) for e in y[1]] if y[0] == 'array_val' else val(y)
    plt.plot(x_py, y_py, label=val(label))
    return NULL

def matplotlib_xlabel(label):
    plt.xlabel(val(label))
    return NULL

def matplotlib_ylabel(label):
    plt.ylabel(val(label))
    return NULL

def matplotlib_legend():
    plt.legend()
    return NULL

def matplotlib_show():
    plt.show()
    return NULL


def cilly_repl(env):
    print("Cilly交互式环境（输入'exit()'退出，输入'reset()'清空环境，输入load filename加载cilly程序）")
    buffer = []
    brace_level = 0  # 大括号嵌套计数器

    while True:
        try:
            # 提示符根据是否在代码块中变化
            prompt = "cilly> " if brace_level == 0 else "...... "
            line = input(prompt)

            if line.strip() == "exit()":
                break

            if line.strip() == "reset()":
                print("环境已重置。")
                env[0].clear()
                env[0].update({
                    'greet': mk_primitive_proc(greet),
                    'forward': mk_primitive_proc(turtle_forward),
                    'backward': mk_primitive_proc(turtle_backward),
                    'right': mk_primitive_proc(turtle_right),
                    'left': mk_primitive_proc(turtle_left),
                    'penup': mk_primitive_proc(turtle_penup),
                    'pendown': mk_primitive_proc(turtle_pendown),
                    'pencolor': mk_primitive_proc(turtle_pencolor),
                    'plot': mk_primitive_proc(matplotlib_plot),
                    'xlabel': mk_primitive_proc(matplotlib_xlabel),
                    'ylabel': mk_primitive_proc(matplotlib_ylabel),
                    'legend': mk_primitive_proc(matplotlib_legend),
                    'show': mk_primitive_proc(matplotlib_show)
                })
                buffer.clear()
                brace_level = 0
                continue

            # 支持 load 指令
            if line.strip().startswith("load "):
                filename = line.strip().split(" ", 1)[1]
                try:
                    with open(filename, "r", encoding="utf-8") as f:
                        source = f.read()
                        print(f"正在加载并运行：{filename} ...")
                        tokens = cilly_lexer(source)
                        ast = cilly_parser(tokens)
                        result = cilly_eval(ast, env)
                        if result != NULL:
                            print("=>", val(result))
                except FileNotFoundError:
                    print(f"错误：找不到文件 {filename}")
                except Exception as e:
                    print("执行出错:", e)
                continue  # 不进入缓冲区处理

            code_only = line.split("#", 1)[0].strip()
            buffer.append(line)

            # 更新大括号层级
            brace_level += code_only.count("{") - code_only.count("}")
            brace_level = max(0, brace_level)

            # 大括号匹配完毕，准备执行
            if brace_level == 0 and (code_only.endswith(";") or code_only.endswith("}")):
                source = "\n".join(buffer)
                buffer.clear()

                tokens = cilly_lexer(source)
                ast = cilly_parser(tokens)
                result = cilly_eval(ast, env)
                if result != NULL:
                    print("=>", val(result))

        except Exception as e:
            print("错误:", e)  # 直接打印异常消息
            buffer.clear()
            brace_level = 0

env = (
    {
        'greet': mk_primitive_proc(greet),
        'forward': mk_primitive_proc(turtle_forward),
        'backward': mk_primitive_proc(turtle_backward),
        'right': mk_primitive_proc(turtle_right),
        'left': mk_primitive_proc(turtle_left),
        'penup': mk_primitive_proc(turtle_penup),
        'pendown': mk_primitive_proc(turtle_pendown),
        'pencolor': mk_primitive_proc(turtle_pencolor),
        'plot': mk_primitive_proc(matplotlib_plot),
        'xlabel': mk_primitive_proc(matplotlib_xlabel),
        'ylabel': mk_primitive_proc(matplotlib_ylabel),
        'legend': mk_primitive_proc(matplotlib_legend),
        'show': mk_primitive_proc(matplotlib_show)
    },
    None
)
#cilly_repl(env)
