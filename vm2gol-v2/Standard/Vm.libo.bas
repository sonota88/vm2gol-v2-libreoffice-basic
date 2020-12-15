rem -*- mode: basic -*-

Option Explicit

' global variables
dim G_CI_REG_V as integer
dim G_CI_PC_POS as integer
dim G_CI_SP_POS as integer
dim G_CI_MEM_MAIN as integer
dim G_CI_MEM_STACK as integer
dim G_CI_MEM_VRAM as integer
dim G_RI_MEM_VIEW as integer
dim G_RI_MEM as integer

dim STACK_SIZE as integer

dim mem_cache

Sub Main
    setup()
    Utils.setup()

    reset()

    load_program()

    VmView.render()
    start()
End Sub

' --------------------------------

sub setup
    STACK_SIZE = 50

    dim ri_max
    ri_max = get_ri_max(Utils.get_sheet("vm"))
    dim xs(ri_max) as object
    mem_cache = xs

    alert_vm("") ' TODO refactor => clear err msg
end sub

sub reset()
    dim ri

    Utils.init_log()
    Utils.log "."

    ' 初期化
    env_set("pc", 0)
    env_set("reg_a", 0)
    env_set("reg_b", 0)
    env_set("sp", STACK_SIZE - 1)
    env_set("bp", STACK_SIZE - 1)
    env_set("zf", 0)

    env_set("step", 0)
    env_set("_exited", 0)

    dim ri_max as integer
    ri_max = get_ri_max(sh_vm)

    ' reset mem
    clear_cell_range( _
        sh_vm, _
        G_CI_MEM_MAIN, G_RI_MEM, _
        G_CI_MEM_MAIN + 1, ri_max, _
    )

    ' reset stack
    for ri = G_RI_MEM to (G_RI_MEM + STACK_SIZE - 1)
        cell_set(sh_vm, G_CI_MEM_STACK, ri, "")
    next
    wait(100)
    for ri = G_RI_MEM to (G_RI_MEM + STACK_SIZE - 1)
        cell_set(sh_vm, G_CI_MEM_STACK, ri, 0)
    next

    for ri = G_RI_MEM to (G_RI_MEM + 50) - 1
        cell_set(sh_vm, G_CI_MEM_VRAM, ri, 0)
    next

    alert_vm("")
end sub


sub load_program_v1
    dim ri, val, ci

    for ri = 0 to (32 - 1)
        ' オペレータ
        val = cell_get(sh_exe, 0, ri)
        cell_set(sh_vm, G_CI_MEM_MAIN + 0, G_RI_MEM + ri, val)
    next
end sub


sub load_program
    dim lines, line_

    lines = Utils.box_read_lines("exe", "exe")

    dim i
    for i = 0 to List.size(lines) - 1
        line_ = List.get_(lines, i)
        cell_set(sh_vm, G_CI_MEM_MAIN + 0, G_RI_MEM + i, line_)
    next
end sub

rem --------------------------------

' function strip(str)
'   dim retval
' 
'   if left(str, 2) = "  " then
'     retval = substring(str, 2)
'   else
'     retval = str
'   end if
' 
'   strip = retval
' end function


sub set_pc(pc as integer)
    env_set("pc", pc)
end sub


function get_pc() as integer
    get_pc = env_get("pc")
end function


sub set_reg_a(val as integer)
    env_set("reg_a", val)
end sub


function get_reg_a() as integer
    get_reg_a = env_get("reg_a")
end function


sub set_reg_b(val as integer)
    env_set("reg_b", val)
end sub


' function get_insn_v1(pc, i) as variant
'     dim rv
'     dim cell_val, val, xs
'     cell_val = cell_get(sh_vm, G_CI_MEM_MAIN, G_RI_MEM + pc)
'     xs = split(cell_val, " ")
'     val = xs(i)
' 
'     if is_int(val) then
'         rv = CInt(val)
'     else
'         rv = val
'     end if
' 
'     get_insn_v1 = rv
' end function


function _get_insn_parts(pc) as object
    dim rv

    dim cell_val, val, xs
    cell_val = cell_get(sh_vm, G_CI_MEM_MAIN, G_RI_MEM + pc)
    xs = split(cell_val, " ")

    dim parts
    parts = List.new_()

    dim i as integer
    for i = 0 to ubound(xs)
        val = xs(i)
        if is_int(val) then
            List.add(parts, CInt(val))
        else
            List.add(parts, val)
        end if
    next
    rv = parts

    _get_insn_parts = rv
end function


function get_insn(pc, i) as variant
    dim rv
    dim temp
    dim parts

    temp = mem_cache(pc)

    if IsNull(temp) then
        parts = _get_insn_parts(pc)
        mem_cache(pc) = parts
    else
        parts = temp
    end if
    rv = List.get_(parts, i)

    get_insn = rv
end function


sub set_sp(sp)
    if sp < 0 then
        print "stack overflow"
        ' __raise_stack_overflow__ rem TODO
    end if

    env_set("sp", sp)
end sub


function get_sp() as integer
    get_sp = env_get("sp")
end function


sub set_bp(bp as integer)
    env_set("bp", bp)
end sub


function get_bp() as integer
    get_bp = env_get("bp")
end function


sub set_vram(addr as integer, val as integer)
    cell_set(sh_vm, G_CI_MEM_VRAM, G_RI_MEM + addr, val)
end sub


function get_vram(addr as integer)
    on local error goto on_error__get_vram
    dim rv

    rv = cell_get(sh_vm, G_CI_MEM_VRAM, G_RI_MEM + addr)
    get_vram = rv

    exit function
on_error__get_vram:
    msgbox format_err_msg("get_vram", err, erl, error$)
end function


function _is_bp_rel(str as string) as boolean
    _is_bp_rel = (left(str, 3) = "[bp")
end function


function _bp_delta(str as string) as integer
    dim rv as integer

    dim sign as string
    sign = char_at(str, 3)

    dim factor as integer
    select case sign
        case "+"
            factor = 1
        case "-"
            factor = -1
        case else
            __invalid_sign__
    end select

    dim s as string
    s = substring(str, 4, len(str) - 1)

    rv = CInt(s) * factor

    _bp_delta = rv
end function

rem --------------------------------

function run_step() as boolean
    on local error goto err__run_step
    dim op
    dim param1, param2
    dim retval as boolean
    dim pc as integer

    if env_get("_exited") = 1 then
        exit function
    end if

    dim step_ as long
    step_ = env_get("step")
    env_set("step", step_ + 1)

    pc = env_get("pc")
    op = get_insn(pc, 0)

    select case op
        case "exit"
            env_set("_exited", 1)
            alert_vm("done")
            retval = true
            pc = pc + 1
            env_set("pc", pc)

        case "set_reg_a"
            insn_set_reg_a()

        case "set_reg_b"
            insn_set_reg_b()

        case "cp"
            insn_cp()

        case "add_ab"
            insn_add_ab()

        case "mult_ab"
            insn_mult_ab()

        case "add_sp"
            insn_add_sp()

        case "sub_sp"
            insn_sub_sp()

        case "compare"
            insn_compare()

        case "label"
            env_set("pc", pc + 1)

        case "jump"
            param1 = get_insn(pc, 1)
            ' logkv("param1", param1)
            pc = CInt(param1)
            env_set("pc", pc)

        case "jump_eq"
            insn_jump_eq()

        case "call"
            insn_call()

        case "ret"
            insn_ret()

        case "push"
            insn_push()
        case "pop"
            insn_pop()

        case "set_vram"
            insn_set_vram()
        case "get_vram"
            insn_get_vram()

        case "_cmt"
            insn__cmt()

        case else
            alert_vm("184 unknown op (" & op & ")")
    end select

    ' VmView.render()

    retval = false

    run_step = retval

    exit function
err__run_step:
    print format_err_msg("run_step", err, erl, error$)
end function


sub start()
    dim cnt as integer
    dim doexit

    cnt = 0
    do
        cnt = cnt + 1

        doexit = run_step()
        if doexit = true then
            exit do
        end if
    loop while cnt < 10
end sub

rem --------------------------------

' insn_exit


sub insn_set_reg_a()
    dim pc as integer
    pc = get_pc()

    dim val as integer
    val = get_insn(pc, 1)

    env_set("reg_a", val)
    env_set("pc", pc + 1)
end sub


sub insn_set_reg_b()
    dim pc as integer
    pc = get_pc()

    dim val as integer
    val = get_insn(pc, 1)

    env_set("reg_b", val)
    env_set("pc", pc + 1)
end sub


sub insn_cp()
    on local error goto err__insn_cp

    dim pc as integer
    dim param1, param2
    dim val
    pc = get_pc()

    param1 = get_insn(pc, 1)
    param2 = get_insn(pc, 2)

    dim bp_delta as integer

    if param1 = "reg_a" then
        val = get_reg_a()
    elseif param1 = "sp" then
        val = get_sp()
    elseif param1 = "bp" then
        val = get_bp()
    elseif _is_bp_rel(param1) then
        bp_delta = _bp_delta(param1)
        val = mem_stack_get(get_bp() + bp_delta)
    elseif match_int(param1) then
        val = CInt(param1)
    else
        __not_yet_impl__
    end if

    if param2 = "reg_a" then
        set_reg_a(val)
    elseif param2 = "reg_b" then
        set_reg_b(val)
    elseif param2 = "bp" then
        set_bp(val)
    elseif param2 = "sp" then
        set_sp(val)
    elseif _is_bp_rel(param2) then
        bp_delta = _bp_delta(param2)
        mem_stack_set(get_bp() + bp_delta, val)
    else
        Utils.logkv "param2", param2
        __not_yet_impl__
    end if

    set_pc(pc + 1)

    exit sub
err__insn_cp:
    print format_err_msg("insn_cp", err, erl, error$)
end sub


sub insn_add_ab
    dim pc as integer
    pc = get_pc()

    dim a as integer
    a = env_get("reg_a")

    dim b as integer
    b = env_get("reg_b")

    env_set("reg_a", a + b)

    env_set("pc", pc + 1)
end sub


sub insn_mult_ab
    dim pc as integer
    pc = get_pc()

    dim a as integer
    a = env_get("reg_a")

    dim b as integer
    b = env_get("reg_b")

    env_set("reg_a", a * b)

    env_set("pc", pc + 1)
end sub


sub insn_add_sp()
    dim pc as integer
    dim n as integer
    pc = get_pc()

    n = CInt(get_insn(get_pc(), 1))
    set_sp(get_sp() + n)

    set_pc(pc + 1)
end sub


sub insn_sub_sp()
    dim pc as integer
    dim n as integer
    pc = get_pc()

    n = CInt(get_insn(get_pc(), 1))
    set_sp(get_sp() - n)

    set_pc(pc + 1)
end sub


sub insn_compare()
    dim pc as integer
    dim a, b
    a = env_get("reg_a")
    b = env_get("reg_b")

    if a = b then
      env_set("zf", 1)
    else
      env_set("zf", 0)
    end if

    pc = env_get("pc")
    env_set("pc", pc + 1)
end sub

' label
' jump

sub insn_jump_eq()
    dim pc as integer
    pc = env_get("pc")

    if env_get("zf") = 1 then
        pc = CInt(get_insn(pc, 1))
    else
        pc = env_get("pc") + 1
    end if

    env_set("pc", pc)
end sub


sub insn_call()
    dim pc as integer
    dim sp, next_pc
    sp = env_get("sp") - 1
    pc = env_get("pc")

    set_sp(sp)
    mem_stack_set(sp, pc + 1)
    next_pc = CInt(get_insn(pc, 1))
    env_set("pc", next_pc)
end sub


sub insn_ret()
    dim ret_addr, sp
    sp = env_get("sp")

    ret_addr = mem_stack_get(sp)
    env_set("pc", ret_addr)
    set_sp(sp + 1)
end sub


sub insn_push()
    on local error goto err__insn_push

    dim pc as integer
    pc = env_get("pc")
    dim sp as integer
    sp = env_get("sp")

    dim param as variant
    param = get_insn(pc, 1)

    dim val_to_push as integer

    select case TypeName(param)
        case "Integer"
            val_to_push = param
        case "Double"
            val_to_push = CInt(param)
        case "String"
            if param = "reg_a" then
                val_to_push = get_reg_a()
            elseif param = "bp" then
                val_to_push = get_bp()
            elseif _is_bp_rel(param) then
                dim delta as integer
                delta = _bp_delta(param)
                val_to_push = mem_stack_get(get_bp() + delta)
            else
                __not_yet_impl__
            end if
        case else
          __ERR__
    end select

    set_sp(sp - 1)
    mem_stack_set(sp - 1, val_to_push)

    env_set("pc", pc + 1)

    exit sub
err__insn_push:
    print format_err_msg("insn_push", err, erl, error$)
end sub


sub insn_pop
    dim pc as integer
    dim sp, dest
    dim val as integer
    pc = env_get("pc")
    sp = env_get("sp")

    val = mem_stack_get(sp)

    dest = get_insn(pc, 1)
    if dest = "reg_a" then
        set_reg_a(val)
    elseif dest = "reg_b" then
        set_reg_b(val)
    elseif dest = "bp" then
        set_bp(val)
    else
        __not_yet_impl__
    end if

    set_sp(sp + 1)

    env_set("pc", pc + 1)
end sub


sub insn_set_vram
    dim pc as integer
    pc = env_get("pc")

    dim arg1, arg2
    arg1 = get_insn(pc, 1)
    arg2 = get_insn(pc, 2)

    dim bp_delta as integer

    dim vt as integer
    vt = VarType(arg2)

    dim val
    ' if type_name_ex(arg2) = "Integer" then
    if vt = TYPE_INTEGER then
        val = arg2
        ' elseif type_name_ex(arg2) = "String" then
    elseif vt = TYPE_STRING then
        if _is_bp_rel(arg2) then
            bp_delta = _bp_delta(arg2)
            val = mem_stack_get(get_bp() + bp_delta)
        else
            __not_yet_impl__
        end if
    else
        __not_yet_impl__
    end if

    vt = VarType(arg1)
    ' if type_name_ex(arg1) = "Integer" then
    if vt = TYPE_INTEGER then
        set_vram(arg1, val)
    ' elseif type_name_ex(arg1) = "String" then
    elseif vt = TYPE_STRING then
        if _is_bp_rel(arg1) then
            bp_delta = _bp_delta(arg1)
            dim vram_addr
            vram_addr = mem_stack_get(get_bp() + bp_delta)
            set_vram(vram_addr, val)
        else
            __not_yet_impl__
        end if
    else
        __not_yet_impl__
    end if

    env_set("pc", pc + 1)
end sub


sub insn_get_vram
    on local error goto on_error__insn_get_vram

    dim pc as integer
    pc = env_get("pc")

    dim arg1, arg2
    dim vram_addr as integer
    dim dest as string
    arg1 = get_insn(pc, 1)
    arg2 = get_insn(pc, 2)

    dim vt as integer
    vt = VarType(arg2)

    ' if type_name_ex(arg1) = "Integer" then
    if vt = TYPE_INTEGER then
        vram_addr = arg1
    elseif _is_bp_rel(arg1) then ' TODO string で分岐
        dim bp_delta
        bp_delta = _bp_delta(arg1)
        vram_addr = mem_stack_get(get_bp() + bp_delta)
    else
        __not_yet_impl__
    end if

    dim val
    if arg2 = "reg_a" then
        val = get_vram(vram_addr)
        set_reg_a(val)
    else
        __not_yet_impl__
    end if

    env_set("pc", pc + 1)

    exit sub
on_error__insn_get_vram:
    msgbox format_err_msg("insn_get_vram", err, erl, error$)
end sub


sub insn__cmt
    dim pc as integer
    pc = env_get("pc")

    env_set("pc", pc + 1)
end sub

rem --------------------------------
rem メモリ

function mem_stack_get(addr)
    mem_stack_get = cell_get(sh_vm, G_CI_MEM_STACK, G_RI_MEM + addr)
end function

sub mem_stack_set(addr, val)
    cell_set(sh_vm, G_CI_MEM_STACK, G_RI_MEM + addr, val)
end sub

rem --------------------------------

sub alert_vm(msg)
    Utils.log(msg)
    env_set("_alert_msg_vm", msg)
    VmView.render()
end sub
