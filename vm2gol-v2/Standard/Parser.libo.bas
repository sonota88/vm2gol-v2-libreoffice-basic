rem -*- mode: basic -*-

Option Explicit

dim pos as integer
dim tok_ci0, tok_ri0
dim tokens_sheet

' --------------------------------

Sub onclick_parse()
    dim _tokens_sheet, tree

    Utils.setup()
    Utils.init_log2()

    _tokens_sheet = Utils.get_sheet("tokens")
    sh_tree = Utils.get_sheet("tree")

    tree = parse(_tokens_sheet, 0, 0)

    dim tree_json
    tree_json = Json.to_json(tree)

    _clear_output
    wait(100)
    box_text_set("tokens", "tree", tree_json)
    box_text_set("tree", "tree", tree_json)

    Utils.log "done"
End Sub


sub onclick_clear()
    Utils.init_log2()
    _clear_output()
end sub


sub _clear_output
    box_text_set("tokens", "tree", "")
    box_text_set("tree"  , "tree", "")
end sub


' --------------------------------

type Token
    kind as String
    val as String
    type_ as string
end type


function Token_inspect(t) as string
    dim rv

    rv = "<Token"
    rv = rv & " " & t.kind
    rv = rv & ":" & t.val
    rv = rv & ">"

    Token_inspect = rv
end function


function Token_new
    dim rv as New Token
    rv.type_ = "Token"
    Token_new = rv
end function


function peek(optional offset as integer)
    if IsMissing(offset) then
        offset = 0
    end if

    dim t
    t = Token_new()
    t.kind = cell_get(tokens_sheet, tok_ci0, tok_ri0 + pos + offset)
    t.val = cell_get(tokens_sheet, tok_ci0 + 1, tok_ri0 + pos + offset)
    peek = t
end function


function _get_num_tokens() as integer
    dim ri, val, max

    ri = tok_ri0
    max = get_ri_max(tokens_sheet)

    do while ri <= max
        val = cell_get(tokens_sheet, tok_ci0, ri)
        if val = "" then
            exit do
        end if
        ri = ri + 1
    loop

    _get_num_tokens = ri - tok_ri0
end function


function _is_end() as boolean
    _is_end = _get_num_tokens() <= pos
end function


sub _assert_value(pos, exp)
    dim t
    t = peek(0)

    if t.val <> exp then
      msgbox "Assertion failed: expected (" & exp & "), actual (" & t.val & ")"
      __ERROR_assertion_failed__
    end if
end sub


sub _consume(str)
    _assert_value(pos, str)
    pos = pos + 1
end sub

rem --------------------------------

function _parse_arg
    dim rv

    dim t
    t = peek()

    if t.kind = "ident" then
        pos = pos + 1
        rv = t.val
    elseif t.kind = "int" then
        pos = pos + 1
        rv = CInt(t.val)
    else
        __not_yet_impl__
    end if

    _parse_arg = rv
end function


function _parse_args_first
    dim rv

    if peek().val = ")" then
        _parse_args_first = null
        exit function
    end if

    rv = _parse_arg()
    _parse_args_first = rv
end function


function _parse_args_rest
    dim rv

    if peek().val = ")" then
        _parse_args_rest = null
        exit function
    end if

    _consume ","

    rv = _parse_arg()
    _parse_args_rest = rv
end function


function _parse_args
    dim rv

    dim args
    args = List.new_()

    dim first_arg
    first_arg = _parse_args_first()

    if IsNull(first_arg) then
        _parse_args = args
        exit function
    else
        List.add(args, first_arg)
    end if 

    dim rest_arg
    do while true
        rest_arg = _parse_args_rest()
        if IsNull(rest_arg) then
            exit do
        else
            List.add(args, rest_arg)
        end if 
    loop

    rv = args

    _parse_args = rv
end function


function _parse_func
    Utils.logkv("-->> _parse_func", peek(1).val)

    dim retval
    dim args, stmts

    _consume "func"

    dim t, func_name ' TODO rename => fn_name
    t = peek(0)
    pos = pos + 1
    func_name = t.val

    _consume "("
    args = _parse_args()
    _consume ")"

    _consume "{"

    stmts = List.new_()
    do while true
        t = peek()
        if t.val = "}" then
            exit do
        end if

        if t.val = "var" then
            List.add(stmts, _parse_var())
        else
            List.add(stmts, _parse_stmt())
        end if
    loop

    _consume "}"

    retval = List.new_()
    List.add(retval, "func")
    List.add(retval, func_name)
    List.add(retval, args)
    List.add(retval, stmts)

    _parse_func = retval
end function


function _parse_var_declare()
    dim rv

    dim t, var_name
    t = peek(0)
    pos = pos + 1

    var_name = t.val

    _consume ";"

    rv = List.new_()
    List.add(rv, "var")
    List.add(rv, var_name)

    _parse_var_declare = rv
end function


function _parse_var_init
    dim rv

    dim t
    t = peek()
    pos = pos + 1

    dim var_name as string
    var_name = t.val

    _consume "="

    dim expr
    expr = _parse_expr()

    _consume ";"

    rv = List.new_()
    List.add(rv, "var")
    List.add(rv, var_name)
    List.add(rv, expr)

    _parse_var_init = rv
end function


function _parse_var()
    dim rv

    _consume "var"

    dim t
    t = peek(1) 

    if t.val = ";" then
      rv = _parse_var_declare()
    elseif t.val = "=" then
      rv = _parse_var_init()
    else
      __parse_error__
    end if

    _parse_var = rv
end function


function _parse_expr_right(expr_l)
    ' Utils.logkv("-->> _parse_expr_right()", peek())
    dim rv
    dim t, expr_r

    t = peek()

    if t.val = ";" or t. val = ")" then
        rv = expr_l
        _parse_expr_right = rv
        exit function
    end if

    select case t.val
        case "+"
            _consume "+"
            expr_r = _parse_expr()
            rv = List.from_array(Array("+", expr_l, expr_r))
        case "*"
            _consume "*"
            expr_r = _parse_expr()
            rv = List.from_array(Array("*", expr_l, expr_r))
        case "=="
            _consume "=="
            expr_r = _parse_expr()
            rv = List.from_array(Array("eq", expr_l, expr_r))
        case "!="
            _consume "!="
            expr_r = _parse_expr()
            rv = List.from_array(Array("neq", expr_l, expr_r))
        case else
            __ERR_parse_error__
    end select

    _parse_expr_right = rv
end function


function _parse_expr
    ' Utils.logkv("-->> _parse_expr()", peek())
    dim rv
    dim t, expr_l, expr_r, t_left

    t_left = peek()

    if t_left.val = "(" then
        _consume "("
        expr_l = _parse_expr()
        _consume ")"

        rv = _parse_expr_right(expr_l)
        _parse_expr = rv
        exit function
    end if

    if t_left.kind = "int" then
        pos = pos + 1
        expr_l = CInt(t_left.val)
        rv = _parse_expr_right(expr_l)
    elseif t_left.kind = "ident" then
        pos = pos + 1
        expr_l = t_left.val
        rv = _parse_expr_right(expr_l)
    else
        __not_yet_impl__
    end if

    _parse_expr = rv
end function


function _parse_set
    ' Utils.logkv("-->> _parse_set()", peek())
    dim rv
    dim t, var_name, expr

    _consume "set"

    t = peek()
    pos = pos + 1
    var_name = t.val

    _consume "="

    expr = _parse_expr()

    _consume ";"

    rv = List.new_()
    List.add(rv, "set")
    List.add(rv, var_name)
    List.add(rv, expr)

    _parse_set = rv
end function


function _parse_call
    dim rv
    dim t, func_name, args

    _consume "call"

    t = peek()
    pos = pos + 1
    func_name = t.val

    _consume "("
    args = _parse_args()
    _consume ")"

    _consume ";"

    rv = List.new_()
    List.add(rv, "call")
    List.add(rv, func_name)
    List.add_els(rv, args)

    _parse_call = rv
end function


function _parse_funcall
    ' Utils.logkv("-->> _parse_funcall()", inspect(peek()))
    dim rv

    dim t, fn_name, args
    t = peek()
    pos = pos + 1
    fn_name = t.val

    _consume "("
    args = _parse_args()
    _consume ")"

    rv = List.new_()
    List.add(rv, fn_name)
    List.add_els(rv, args)

    _parse_funcall = rv
end function


function _parse_call_set
    ' Utils.logkv("-->> _parse_call_set()", inspect(peek()))
    dim rv

    _consume "call_set"

    dim t, var_name, expr
    t = peek()
    pos = pos + 1
    var_name = t.val

    _consume "="

    expr = _parse_funcall()

    _consume ";"

    rv = List.new_()
    List.add(rv, "call_set")
    List.add(rv, var_name)
    List.add(rv, expr)

    _parse_call_set = rv
end function


function _parse_return
    dim rv

    _consume "return"

    dim t
    t = peek()

    rv = List.new_()
    List.add(rv, "return")

    if t.val <> ";" then
        dim expr
        expr = _parse_expr()
        List.add(rv, expr)
    end if

    _consume ";"

    _parse_return = rv
end function


function _parse_when_clause
    dim rv

    dim t
    t = peek()
    if t.val = "}" then
        _parse_when_clause = null
        exit function
    end if

    _consume "("
    dim expr
    expr = _parse_expr()
    _consume ")"

    _consume "{"
    dim stmts
    stmts = _parse_stmts()
    _consume "}"

    rv = List.create()
    List.add(rv, expr)
    List.add_els(rv, stmts)

    _parse_when_clause = rv
end function


function _parse_case
    dim rv

    _consume "case"
    _consume "{"

    dim when_clauses
    when_clauses = List.create()

    dim when_clause
    do while true
        when_clause = _parse_when_clause()
        if IsNull(when_clause) then
            exit do
        else
            List.add(when_clauses, when_clause)
        end if
    loop

    _consume "}"

    rv = List.create()
    List.add(rv, "case")
    List.add_els(rv, when_clauses)

    _parse_case = rv
end function


function _parse_while
    dim rv

    _consume "while"

    dim expr, stmts

    _consume "("
    expr = _parse_expr()
    _consume ")"

    _consume "{"
    stmts = _parse_stmts()
    _consume "}"

    rv = List.from_array(Array("while", expr, stmts))
    _parse_while = rv
end function


function _parse_vm_comment
    dim rv

    _consume "_cmt"
    _consume "("

    dim t
    t = peek()
    pos = pos + 1
    dim comment
    comment = t.val

    _consume ")"
    _consume ";"

    rv = List.from_array(Array("_cmt", comment))
    _parse_vm_comment = rv
end function


function _parse_stmt()
    ' Utils.logkv("-->> _parse_stmt()", inspect(peek()))
    dim rv
    dim t

    t = peek()
    select case t.val
      case "set"
          rv = _parse_set()
      case "call"
          rv = _parse_call()
      case "call_set"
          rv = _parse_call_set()
      case "return"
          rv = _parse_return()
      case "while"
          rv = _parse_while()
      case "case"
          rv = _parse_case()
      case "_cmt"
          rv = _parse_vm_comment()
      case else
          Utils.logkv2("t", t)
          Utils.logkv2("pos", pos)
          __not_yet_impl__
    end select

    _parse_stmt = rv
end function


function _parse_stmts
    dim rv

    dim stmts
    stmts = List.new_()

    dim stmt

    do while true
        if peek().val = "}" then
            exit do
        end if

        stmt = _parse_stmt()
        List.add(stmts, stmt)
    loop

    rv = stmts
    _parse_stmts = rv
end function


function _parse_top_stmt()
    dim retval ' TODO rename => rv

    dim t
    t = peek(0)

    if t.val = "func" then
        retval = _parse_func()
    else
        __unexpected_token__
    end if

    _parse_top_stmt = retval
end function


function _parse_top_stmts(sheet)
    Utils.log("-->> _parse_top_stmts")

    dim retval
    dim t, top_stmts

    top_stmts = List.new_()
    List.add(top_stmts, "top_stmts")

    do while true
        if _is_end() then
            exit do
        end if

        List.add(top_stmts, _parse_top_stmt())
    loop

    retval = top_stmts
    _parse_top_stmts = retval
end function


function parse( _
    _tokens_sheet, _tok_ci0, _tok_ri0 _
  )
    dim ri
    pos = 0
    tok_ci0 = _tok_ci0
    tok_ri0 = _tok_ri0
    tokens_sheet = _tokens_sheet

    dim tree
    tree = _parse_top_stmts(tokens_sheet)
    parse = tree
end function
