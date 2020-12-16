rem -*- mode: basic -*-

Option Explicit

dim G_CI_BP_POS as integer

' --------------------------------

sub onclick_reset()
    Utils.setup()
    Vm.setup()

    Vm.reset()

    render()
end sub


sub onclick_load()
    Utils.setup()
    Vm.setup()

    Vm.load_program()
    render()
end sub


sub onclick_step()
    on local error goto on_error__onclick_step

    Utils.setup()
    Vm.setup()

    _lock()

    Vm.run_step()
    VmView.render()

    _unlock()

    exit sub
on_error__onclick_step:
    _unlock()
    print format_err_msg("onclick_step", err, erl, error$)
end sub


sub onclick_step_n
    Utils.setup()
    Vm.setup()
    Utils.init_log()

    _lock()

    dim t0_sec
    t0_sec = time_sec()

    dim i as integer
    dim num_steps as long
    num_steps = env_get("num_steps")
    for i = 0 to (num_steps - 1)
        Vm.run_step()

        if (i mod 200) = 0 then
            VmView.render()
            _unlock()
            _lock()
        end if
    next

    alert_vm(CStr(time_sec() - t0_sec) & " sec")

    _unlock()
end sub

' --------------------------------

sub _lock
    if not ThisComponent.hasControllersLocked() then
        ThisComponent.lockControllers()
    end if
end sub


sub _unlock
    if ThisComponent.hasControllersLocked() then
        ThisComponent.unlockControllers()
    end if
end sub


sub _lock_action
    if not ThisComponent.isActionLocked() then
        ThisComponent.addActionLock()
    end if
end sub


sub _unlock_action
    if ThisComponent.isActionLocked() then
        ThisComponent.removeActionLock()
    end if
end sub

' --------------------------------

sub cell_text_color(sh, ci, ri, color)
    dim cell
    cell = sh.getcellByposition(ci, ri)
    cell.CharColor = color
end sub


function _insn_color(insn)
    dim rv

    if left(insn, 4) = "jump" or left(insn, 5) = "call " or left(insn, 3) = "ret" or left(insn, 4) = "exit" then
        rv = rgb(230, 0, 0)
    elseif left(insn, 4) = "_cmt" then
        rv = rgb(120, 140, 120)
    else
        rv = rgb(0, 0, 0)
    end if

    _insn_color = rv
end function


sub _render_mem_main(sh_vm, pc as integer)
    dim addr_from as integer
    dim addr_to as integer

    addr_from = pc - 13
    addr_to = pc + 14

    dim addr as integer
    dim i as integer
    dim view_ri as integer
    dim insn as string
    i = 0
    for addr = addr_from to addr_to
        view_ri = G_RI_MEM_VIEW + i
        if 0 <= addr then
            insn = cell_get(sh_vm, G_CI_MEM_MAIN, G_RI_MEM + pc + i - 13)
            if left(insn, 5) = "label" then
                cell_set(sh_vm, G_CI_MEM_MAIN, view_ri, insn)
            else
                cell_set(sh_vm, G_CI_MEM_MAIN, view_ri, "  " + insn)
            end if
            cell_text_color(sh_vm, G_CI_MEM_MAIN, view_ri, _insn_color(insn))
        else
            cell_set(sh_vm, G_CI_MEM_MAIN, view_ri, "")
        end if

        if 0 <= addr then
            cell_set(sh_vm, G_CI_MEM_MAIN - 1, view_ri, addr)
        else
            cell_set(sh_vm, G_CI_MEM_MAIN - 1, view_ri, "")
        end if

        i = i + 1
    next
end sub


sub _render_mem_stack(sh_vm, bp as integer, sp as integer)
    dim addr_from as integer
    dim addr_to as integer

    const VIEW_SIZE = 16

    'addr_from = bp - 11
    'addr_to   = bp + 4
    addr_from = sp - 4
    addr_to   = sp + 11

    if addr_from < 0 then
        addr_from = 0
        addr_to = VIEW_SIZE - 1
    elseif STACK_SIZE <= addr_to then
        addr_from = STACK_SIZE - VIEW_SIZE
        addr_to = STACK_SIZE - 1
    end if

    dim addr as integer
    dim i as integer

    ' view
    dim stack_view_size
    stack_view_size = 16
    clear_cell_range(sh_vm, G_CI_BP_POS       , G_RI_MEM_VIEW,  G_CI_BP_POS       , G_RI_MEM_VIEW + stack_view_size - 1)
    clear_cell_range(sh_vm, G_CI_SP_POS       , G_RI_MEM_VIEW,  G_CI_SP_POS       , G_RI_MEM_VIEW + stack_view_size - 1)
    clear_cell_range(sh_vm, G_CI_MEM_STACK - 1, G_RI_MEM_VIEW,  G_CI_MEM_STACK - 1, G_RI_MEM_VIEW + stack_view_size - 1)
    clear_cell_range(sh_vm, G_CI_MEM_STACK    , G_RI_MEM_VIEW,  G_CI_MEM_STACK    , G_RI_MEM_VIEW + stack_view_size - 1)

    dim val, view_ri
    i = 0
    for addr = addr_from to addr_to
        view_ri = G_RI_MEM_VIEW + i
        if (0 <= addr) and (addr < STACK_SIZE) then
            cell_set(sh_vm, G_CI_MEM_STACK - 1, view_ri, addr)
            val = cell_get(sh_vm, G_CI_MEM_STACK, G_RI_MEM + addr)
            cell_set(sh_vm, G_CI_MEM_STACK    , view_ri, val)
        else
            cell_set(sh_vm, G_CI_MEM_STACK    , view_ri, "-")
        end if

        if bp = addr then
            cell_set(sh_vm, G_CI_BP_POS    , view_ri, "bp =>")
        end if

        if sp = addr then
            cell_set(sh_vm, G_CI_SP_POS    , view_ri, "sp =>")
        end if

        i = i + 1
    next
end sub


sub _render_mem_vram_common(sh_vm, addr_offset as integer)
    dim i as integer
    dim val as integer
    dim view_ci, view_ri
    dim x, y, str
    for i = addr_offset to addr_offset + (25 - 1)
        val = cell_get(sh_vm, G_CI_MEM_VRAM, G_RI_MEM + i)
        x = i mod 5
        y = i \ 5
        if val = 0 then
            str = "."
        else
            str = "■"
        end if
        cell_set(sh_vm, G_CI_MEM_VRAM + x, G_RI_MEM_VIEW + y, str)
    next
end sub


sub _render_mem_vram(sh_vm)
    _render_mem_vram_common(sh_vm, 0)
    _render_mem_vram_common(sh_vm, 25)
end sub


sub render
    on local error goto err__render
    dim pc, sp, bp

    pc = CInt(env_get("pc"))
    sp = env_get("sp")
    bp = env_get("bp")

    dim reg_ri0 as integer
    reg_ri0 = 7
    cell_set(sh_vm, G_CI_REG_V, reg_ri0 + 0, pc)
    cell_set(sh_vm, G_CI_REG_V, reg_ri0 + 1, env_get("reg_a"))
    cell_set(sh_vm, G_CI_REG_V, reg_ri0 + 2, env_get("reg_b"))
    cell_set(sh_vm, G_CI_REG_V, reg_ri0 + 3, env_get("sp"))
    cell_set(sh_vm, G_CI_REG_V, reg_ri0 + 4, env_get("bp"))
    cell_set(sh_vm, G_CI_REG_V, reg_ri0 + 5, env_get("zf"))

    cell_set(sh_vm, 2, 2, "step: " & env_get("step"))
    cell_set(sh_vm, 2, 3, env_get("_alert_msg_vm"))

    _render_mem_main(sh_vm, pc)
    _render_mem_stack(sh_vm, bp, sp)
    _render_mem_vram(sh_vm)

    _unlock()

    exit sub
err__render:
    print format_err_msg("render", err, erl, error$)
end sub
