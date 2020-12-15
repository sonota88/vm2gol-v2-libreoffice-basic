rem -*- mode: basic -*-

Option Explicit

const CI_OUT_TREE = 3

dim g_asm_code as string
dim g_label_id as integer

sub onclick_codegen
    Utils.setup()
    Utils.init_log()

    dim asm_code as string
    asm_code = codegen()

    box_text_set("tree", "asm", asm_code)
    box_text_set("asm", "asm", asm_code)

    Utils.log "done"
end sub

' --------------------------------

sub onclick_clear()
    Utils.init_log()
    Utils.log "."

    _clear_output()
end sub


sub _clear_output
    box_text_set("tree", "asm", ".")
    box_text_set("asm" , "asm", ".")
end sub

' --------------------------------

function match_int(str as string) as boolean
    dim rv
    dim i, c

    rv = true
    for i = 0 to len(str) - 1
      c = char_at(str, i)
      if Utils.is_numeric(c) or c = "-" then
        rem OK
      else
        rv = false
        exit for
      end if
    next

    match_int = rv
end function


function _to_fn_arg_addr(fn_arg_names, fn_arg_name)
    dim rv
    dim i

    i = List.index(fn_arg_names, fn_arg_name)
    rv = "[bp+" & (i + 2) & "]"
  
    _to_fn_arg_addr = rv
end function


function _to_lvar_addr(lvar_names, lvar_name)
    dim rv
    dim i

    i = List.index(lvar_names, lvar_name)
    rv = "[bp-" & (i + 1) & "]"
  
    _to_lvar_addr = rv
end function


function _simple_deref(fn_arg_names, lvar_names, val)
    dim rv

    select case TypeName(val)
        case "Integer"
            rv = val
        case "String"
            if List.include(fn_arg_names, val) then
                rv = _to_fn_arg_addr(fn_arg_names, val)
            elseif List.include(lvar_names, val) then
                rv = _to_lvar_addr(lvar_names, val)
            else
                rv = null
            end if
        case else
            rv = null
    end select

    _simple_deref = rv
end function

' --------------------------------

sub _gen_var(fn_arg_names, lvar_names, stmt_rest)
    _puts("  sub_sp 1")

    if List.size(stmt_rest) = 2 then
        _gen_set(fn_arg_names, lvar_names, stmt_rest)
    end if
end sub


sub _gen_case(fn_arg_names, lvar_names, when_blocks)
    g_label_id = g_label_id + 1
    dim label_id as integer
    label_id = g_label_id

    dim when_idx as integer
    when_idx = -1

    dim label_end           as string
    dim label_when_head     as string
    dim label_end_when_head as string
    label_end           = "end_case_" & label_id
    label_when_head     = "when_"     & label_id
    label_end_when_head = "end_when_" & label_id

    _puts ""
    _puts "  # -->> case_" & label_id

    dim i as integer
    dim when_block
    for i = 0 to List.size(when_blocks) - 1
        when_idx = when_idx + 1
        when_block = List.get_(when_blocks, i)

        dim cond, rest
        cond = List.head(when_block)
        rest = List.rest(when_block)

        dim cond_head, cond_rest
        cond_head = List.head(cond)
        cond_rest = List.rest(cond)

        _puts "  # when_" & label_id & "_" & when_idx & ": " & inspect(cond)

        if cond_head = "eq" then
            _puts "  # -->> expr"
            _gen_expr(fn_arg_names, lvar_names, cond)
            _puts "  # <<-- expr"

            _puts "  set_reg_b 1"
            _puts "  compare"
            _puts "  jump_eq " & label_when_head & "_" & when_idx
            _puts "  jump " & label_end_when_head & "_" & when_idx

            _puts "label " & label_when_head & "_" & when_idx

            _puts "  # -->> stmts"
            _gen_stmts(fn_arg_names, lvar_names, rest)
            _puts "  # <<-- stmts"

            _puts "  jump " & label_end

            _puts "label " & label_end_when_head & "_" & when_idx
        else
            __not_yet_impl__
        end if
    next

    _puts "label " & label_end
    _puts "  # <<-- case_" & label_id
    _puts ""
end sub


sub _gen_while(fn_arg_names, lvar_names, rest_)
    dim cond_expr, body
    cond_expr = List.get_(rest_, 0)
    body      = List.get_(rest_, 1)

    g_label_id = g_label_id + 1
    dim label_id as integer
    label_id = g_label_id

    dim label_begin as string
    dim label_end   as string
    dim label_true  as string
    label_begin = "while_"     & label_id
    label_end   = "end_while_" & label_id
    label_true  = "true_"      & label_id

    _puts ""
    _puts "label " & label_begin

    _gen_expr(fn_arg_names, lvar_names, cond_expr)
    _puts "  set_reg_b 1"
    _puts "  compare"

    _puts "  jump_eq " & label_true
    _puts "  jump " & label_end
    _puts "label " & label_true

    _gen_stmts(fn_arg_names, lvar_names, body)

    _puts "  jump " & label_begin
    _puts "label " & label_end
    _puts ""
end sub


sub _gen_expr_push(fn_arg_names, lvar_names, val)
    dim val_to_push

    dim deref_result
    deref_result = _simple_deref(fn_arg_names, lvar_names, val)

    if not IsNull(deref_result) then
        val_to_push = deref_result
    else

        select case type_name_ex(val)
            case "List"
                _gen_expr(fn_arg_names, lvar_names, val)
                val_to_push = "reg_a"
            case "String"
                __not_yet_impl__
            case else
                __not_yet_impl__
        end select

    end if

    _puts "  push " & val_to_push
end sub


sub _gen_expr_add
    _puts "  pop reg_b"
    _puts "  pop reg_a"
    _puts "  add_ab"
end sub


sub _gen_expr_mult
    _puts "  pop reg_b"
    _puts "  pop reg_a"
    _puts "  mult_ab"
end sub


sub _gen_expr_eq
    g_label_id = g_label_id + 1
    dim label_id
    label_id = g_label_id

    dim label_end  as string
    dim label_then as string
    label_end = "end_eq_" & label_id
    label_then = "then_" & label_id

    _puts "  pop reg_b"
    _puts "  pop reg_a"

    _puts "  compare"
    _puts "  jump_eq " & label_then

    ' else
    _puts "  set_reg_a 0"
    _puts "  jump " & label_end

    ' then
    _puts "label " & label_then
    _puts "  set_reg_a 1"

    _puts "label " & label_end
end sub


sub _gen_expr_neq
    g_label_id = g_label_id + 1
    dim label_id
    label_id = g_label_id

    dim label_end  as string
    dim label_then as string
    label_end = "end_neq_" & label_id
    label_then = "then_" & label_id

    _puts "  pop reg_b"
    _puts "  pop reg_a"

    _puts "  compare"
    _puts "  jump_eq " & label_then

    ' else
    _puts "  set_reg_a 1"
    _puts "  jump " & label_end

    ' then
    _puts "label " & label_then
    _puts "  set_reg_a 0"

    _puts "label " & label_end
end sub


sub _gen_expr(fn_arg_names, lvar_names, expr)
    dim operator, params
    operator = List.head(expr)
    params   = List.rest(expr)

    dim arg_l, arg_r
    arg_l = List.get_(params, 0)
    arg_r = List.get_(params, 1)

    _gen_expr_push(fn_arg_names, lvar_names, arg_l)
    _gen_expr_push(fn_arg_names, lvar_names, arg_r)

    select case operator
        case "+"
            _gen_expr_add()
        case "*"
            _gen_expr_mult()
        case "eq"
            _gen_expr_eq()
        case "neq"
            _gen_expr_neq()
        case else
            __not_yet_impl__
    end select
end sub


sub _gen_call_push_fn_arg(fn_arg_names, lvar_names, fn_arg)
    dim push_arg

    dim deref_result
    deref_result = _simple_deref(fn_arg_names, lvar_names, fn_arg)
    if not IsNull(deref_result) then
        push_arg = deref_result
    else
        __not_yet_impl__
    end if

    _puts "  push " & push_arg
end sub


sub _gen_call(fn_arg_names, lvar_names, stmt_rest)
    dim fn_name, fn_args
    fn_name = List.head(stmt_rest)
    fn_args = List.rest(stmt_rest)

    dim fn_args_r, fn_arg
    fn_args_r = List.reverse(fn_args)
    dim i as integer
    for i = 0 to List.size(fn_args_r) - 1
        fn_arg = List.get_(fn_args_r, i)
        _gen_call_push_fn_arg(fn_arg_names, lvar_names, fn_arg)
    next

    _gen_vm_comment "call  " & fn_name
    _puts "  call " & fn_name
    _puts "  add_sp " & List.size(fn_args)
end sub


sub _gen_call_set(fn_arg_names, lvar_names, stmt_rest)
    ' Utils.log2("-->> _gen_call_set()")
  
    dim lvar_name, fn_temp
    lvar_name = List.get_(stmt_rest, 0)
    fn_temp = List.get_(stmt_rest, 1)

    dim fn_name, fn_args
    fn_name = List.head(fn_temp)
    fn_args = List.rest(fn_temp)

    dim fn_args_r, fn_arg
    fn_args_r = List.reverse(fn_args)
    dim i as integer
    for i = 0 to List.size(fn_args_r) - 1
        fn_arg = List.get_(fn_args_r, i)
        _gen_call_push_fn_arg(fn_arg_names, lvar_names, fn_arg)
    next

    _gen_vm_comment "call_set  " & fn_name
    _puts "  call " & fn_name
    _puts "  add_sp " & List.size(fn_args)

    dim lvar_addr
    lvar_addr = _to_lvar_addr(lvar_names, lvar_name)
    _puts "  cp reg_a " & lvar_addr
end sub


function _extract_vram_param(str)
    _extract_vram_param = substring(str, 5, len(str) - 1)
end function


function _match_vram_addr(str) as boolean
    dim rv as boolean

    if left(str, 5) = "vram[" then
        dim param
        param = _extract_vram_param(str)
        rv = is_int(param)
    else
        rv = false
    end if

    _match_vram_addr = rv
end function


function _match_vram_ref(str) as boolean
    dim rv as boolean
  
    if left(str, 5) = "vram[" then
        rv = not _match_vram_addr(str)
    else
        rv = false
    end if
  
    _match_vram_ref = rv
end function


sub _gen_set(fn_arg_names, lvar_names, stmt_rest)
    ' Utils.log2("-->> _gen_set()")

    dim dest, expr, src_val
    dim vram_param as string

    dest = List.get_(stmt_rest, 0)
    expr = List.get_(stmt_rest, 1)

    dim temp
    temp = _simple_deref(fn_arg_names, lvar_names, expr)
    if not IsNull(temp) then
        src_val = temp
    else
        select case type_name_ex(expr)
            case "String"
                if _match_vram_addr(expr) then
                    vram_param = _extract_vram_param(expr)
                    _puts "  get_vram " & vram_param & " reg_a"
                    src_val = "reg_a"

                elseif _match_vram_ref(expr) then
                    vram_param = _extract_vram_param(expr)

                    dim vram_addr
                    vram_addr = _simple_deref(fn_arg_names, lvar_names, vram_param)
                    if not IsNull(vram_addr) then
                        _puts "  get_vram " & vram_addr & " reg_a"
                        src_val = "reg_a"
                    else
                        __not_yet_impl__
                    end if
                else
                    __not_yet_impl__
                end if

            case "List"
                _gen_expr(fn_arg_names, lvar_names, expr)
                src_val = "reg_a"
            case else
                __not_yet_impl__
        end select
    end if

    if List.include(lvar_names, dest) then
        dim lvar_addr
        lvar_addr = _to_lvar_addr(lvar_names, dest)
        _puts("  cp " & src_val & " " & lvar_addr)
    elseif _match_vram_addr(dest) then
        vram_param = _extract_vram_param(dest)
        if is_int(vram_param) then
            _puts "  set_vram " & vram_param & " " & src_val
        else
            __not_yet_impl__
        end if
    elseif _match_vram_ref(dest) then
        vram_param = _extract_vram_param(dest)
        temp = _simple_deref(fn_arg_names, lvar_names, vram_param)
        if not IsNull(temp) then
            _puts "  set_vram " & temp & " " & src_val
        else
            __not_yet_impl__
        end if
    else
        __not_yet_impl__
    end if
end sub


sub _gen_return(lvar_names, stmt_rest)
    dim retval
    retval = List.head(stmt_rest)

    dim vram_param

    dim temp
    temp = _simple_deref(List.new_(), lvar_names, retval)
    if not IsNull(temp) then
        _puts "  cp " & temp & " reg_a"
    else

        select case type_name_ex(retval)
            case "String"
                if _match_vram_ref(retval) then
                    vram_param = _extract_vram_param(retval)
                    temp = _simple_deref(List.new_(), lvar_names, vram_param)
                    if not IsNull(temp) then
                        _puts "  get_vram " & temp & " reg_a"
                    else
                        __not_yet_impl__
                    end if
                else
                    __not_yet_impl__
                end if
            case else
                __not_yet_impl__
        end select

    end if
end sub


sub _gen_vm_comment(comment)
    _puts "  _cmt " & replace_char(comment, " ", "~")
end sub


sub _gen_stmt(fn_arg_names, lvar_names, stmt)
    ' Utils.log("-->> _gen_stmt()")

    dim stmt_head, stmt_rest

    stmt_head = List.head(stmt)
    stmt_rest = List.rest(stmt)

    select case stmt_head
        case "call"
            _gen_call(fn_arg_names, lvar_names, stmt_rest)
        case "call_set"
            _gen_call_set(fn_arg_names, lvar_names, stmt_rest)
        case "set"
            _gen_set(fn_arg_names, lvar_names, stmt_rest)
        case "return"
            _gen_return(lvar_names, stmt_rest)
        case "case"
            _gen_case(fn_arg_names, lvar_names, stmt_rest)
        case "while"
            _gen_while(fn_arg_names, lvar_names, stmt_rest)
        case "_cmt"
            _gen_vm_comment(List.head(stmt_rest))
        case else
            __not_yet_impl__
    end select
end sub


sub _gen_stmts(fn_arg_names, lvar_names, stmts)
    dim i as integer
    dim stmt

    for i = 0 to List.size(stmts) - 1
        stmt = List.get_(stmts, i)
        _gen_stmt(fn_arg_names, lvar_names, stmt)
    next
end sub


sub gen_func_def(stmt_rest)
    dim fn_name as string
    dim fn_arg_names, body, lvar_names
    dim i as integer
    dim stmt

    fn_name      = List.get_(stmt_rest, 0)
    fn_arg_names = List.get_(stmt_rest, 1)
    body         = List.get_(stmt_rest, 2)

    Utils.logkv("-->> gen_func_def()", fn_name)

    _puts("")
    _puts("label " & fn_name)
    _puts("  push bp")
    _puts("  cp sp bp")
    _puts("")

    lvar_names = List.new_()

    for i = 0 to List.size(body) - 1
        stmt = List.get_(body, i)

        if List.get_(stmt, 0) = "var" then
            stmt_rest = List.rest(stmt)
            List.add(lvar_names, List.get_(stmt_rest, 0))
            _gen_var(fn_arg_names, lvar_names, stmt_rest)
        else
            _gen_stmt(fn_arg_names, lvar_names, stmt)
        end if
    next

    _puts("  cp bp sp")
    _puts("  pop bp")
    _puts("  ret")
end sub


sub gen_top_stmts(top_stmt_rest)
    Utils.log("-->> gen_top_stmts()")

    dim stmt, stmt_rest

    dim i as integer
    for i = 0 to List.size(top_stmt_rest) - 1
        stmt = List.get_(top_stmt_rest, i)
        stmt_rest = List.rest(stmt)
        gen_func_def(stmt_rest)
    next
end sub


function codegen as string
    dim _json as string
    dim tree

    g_asm_code = ""
    g_label_id = 0

    clear_cell_range(sh_tree, CI_OUT_TREE, 0, CI_OUT_TREE, 2000)
    clear_cell_range(sh_asm , 0, 0, 0, 2000)
    wait(100)

    _json = read_tree_json()
    tree = Json.parse(_json)

    _puts("  call main")
    _puts("  exit")

    gen_top_stmts(List.rest(tree))

    codegen = g_asm_code
End function

' --------------------------------

function read_tree_json
    read_tree_json = box_text_get("tree", "tree")
end function


sub _puts(aline)
    g_asm_code = g_asm_code & aline & lf()
end sub
