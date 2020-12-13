rem -*- mode: basic -*-

Option Explicit

dim sh_tokens as object
dim sh_tree as object
dim sh_asm as object
dim sh_exe as object
dim sh_vm as object
dim sh_log as object
dim sh_env as object

dim TYPE_INTEGER as integer
dim TYPE_DOUBLE  as integer
dim TYPE_STRING  as integer

' env key => env key ri
dim env_key_map as object

' --------------------------------

' ここでは状態を変えないこと
sub setup()
    TYPE_INTEGER = 2
    TYPE_DOUBLE  = 5
    TYPE_STRING  = 8

    sh_tokens = Utils.get_sheet("tokens")
    sh_tree   = Utils.get_sheet("tree")
    sh_asm    = Utils.get_sheet("asm")
    sh_exe    = Utils.get_sheet("exe")
    sh_vm     = Utils.get_sheet("vm")
    ' sh_log    = Utils.get_sheet("log")
    sh_env    = Utils.get_sheet("env")

    G_CI_REG_V     = setup_get_ci("reg_v")
    G_CI_PC_POS    = setup_get_ci("pc_pos")
    G_CI_SP_POS    = setup_get_ci("sp_pos")
    G_CI_BP_POS    = setup_get_ci("bp_pos")
    G_CI_MEM_MAIN  = setup_get_ci("mem_main")
    G_CI_MEM_STACK = setup_get_ci("mem_stack")
    G_CI_MEM_VRAM  = setup_get_ci("mem_vram")
    G_RI_MEM_VIEW  = setup_get_ri("mem_view")
    G_RI_MEM       = setup_get_ri("mem")

    env_key_map = _init_env_key_map()
end sub


' --------------------------------

function cell_get(sheet, ci, ri)
    on local error goto err__cell_get
    dim cell as object
    dim rv as variant

    cell = sheet.getCellByPosition(ci, ri)

    select case cell.getType()
        case com.sun.star.table.CellContentType.EMPTY
            rv = ""
        case com.sun.star.table.CellContentType.VALUE
            rv = cell.Value
        case com.sun.star.table.CellContentType.TEXT
            rv = cell.String
        case com.sun.star.table.CellContentType.FORMULA
            rv = cell.Formula
        case else
            alert_vm("cell_get: must not happen")
    end select

    cell_get = rv

    exit function
err__cell_get:
    print "cell_get"
end function


sub cell_set(sheet, ci, ri, val as variant)
    dim cell
    cell = sheet.getCellByPosition(ci, ri)

    select case TypeName(val)
    case "Integer", "Double"
        cell.Value = val
    case "String"
        cell.String = val
    case else
        cell.String = "289 unsupported type: " & TypeName(val)
    end select
end sub


sub cell_set_formula(sheet, ci, ri, val as variant)
    dim cell
    cell = sheet.getCellByPosition(ci, ri)

    cell.Formula = val
end sub


sub clear_cell_range(sheet, c0, r0, c1, r1)
    dim range, flags

    range = sheet.getCellRangeByPosition(c0, r0, c1, r1)
    flags = _
        com.sun.star.sheet.CellFlags.STRING + _
        com.sun.star.sheet.CellFlags.VALUE + _
        com.sun.star.sheet.CellFlags.FORMULA
    range.clearContents(flags)
end sub

rem --------------------------------

function _env_find_ri_v1(key)
    dim ri, _key, rv
    rv = -1

    ri = -1
    do while ri < 20
        ri = ri + 1
        _key = cell_get(sh_env, 0, ri)
        if _key = key then
            rv = ri
            exit do
        end if
    loop

    _env_find_ri_v1 = rv
end function


' 高速化のため env_key_map を使用したもの
function _env_find_ri(key) as integer
    _env_find_ri = Map.get_(env_key_map, key)
end function


sub env_set(key as string, val)
    dim ri as integer

    ri = _env_find_ri(key)
    if ri = -1 then
        print "env: key not found: " & key
    else
        cell_set(sh_env, 1, ri, val)
    end if
end sub


function env_get(key as string)
    dim ri as integer
    dim rv

    ri = _env_find_ri(key)
    if ri = -1 then
        print "env: key not found: " & key
    else
        rv = cell_get(sh_env, 1, ri)
    end if

    env_get = rv
end function

rem --------------------------------

sub log(msg)
    log2(msg)
    exit sub ' TODO

    dim cell, log_ri
    log_ri = env_get("_log_ri")

    cell = sh_log.getCellByPosition(0, log_ri)
    cell.formula = msg

    env_set("_log_ri", log_ri + 1)

end sub


sub logkv(k, v)
    logkv2(k, v)
    exit sub ' TODO

    dim cell, log_ri
    log_ri = env_get("_log_ri")

    cell = sh_log.getCellByPosition(0, log_ri)
    'cell.formula = k
    cell = sh_log.getCellByPosition(1, log_ri)
    'cell.value = v
    cell = sh_log.getCellByPosition(2, log_ri)
    'cell.string = v
    cell = sh_log.getCellByPosition(3, log_ri)
    'cell.formula = v

    env_set("_log_ri", log_ri + 1)
end sub

rem --------------------------------

function setup_get_ci(key) as integer
    dim rv
    dim ci, _key

    rv = -1
    ci = -1
    do while ci < 50
        ci = ci + 1
        _key = cell_get(sh_vm, ci, 0)
        if _key = key then
            rv = ci
            exit do
        end if
    loop

    setup_get_ci = rv
end function


function setup_get_ri(key) as integer
    dim rv
    dim ri, _key

    rv = -1
    ri = -1
    do while ri < 50
        ri = ri + 1
        _key = cell_get(sh_vm, 0, ri)
        if _key = key then
            rv = ri
            exit do
        end if
    loop

    setup_get_ri = rv
end function


function _init_env_key_map() as object
    dim rv as object
    rv = Map.new_()

    dim ri as integer
    dim key as string
    dim val

    for ri = 0 to 99
        key = cell_get(sh_env, 0, ri)
        if key = "" then
            exit for
        end if

        Map.set_(rv, key, ri)
    next

    _init_env_key_map = rv
end function

rem --------------------------------

function lf() as string
    lf = chr(10)
end function


function dq() as string
    dq = chr(34)
end function


function bs() as string
    bs = chr(92)
end function


function substring(str as string, index as integer, optional end_index as integer) as string
    dim rv as string

    if (index < 0) then
        __ERROR_invalid_index__
    else
        rv = right(str, len(str) - index)
        if not IsMissing(end_index) then
            rv = left(rv, end_index - index)
        end if
    end if

    substring = rv
end function


function char_at(str as string, index as integer) as string
    dim rv as string

    if (index < 0 or len(str) <= index) then
        rv = "invalid_index"
    else
        rv = right(left(str, index + 1), 1)
    end if

    char_at = rv
end function


function str_include(str as string, target as string) as boolean
    str_include = (instr(str, target) <> 0 )
end function


function is_int(str as string) as boolean
    dim rv as boolean
    dim i as integer
    dim c as string

    if str = "-" then
        rv = false
        exit function
    end if

    rv = true
    for i = 0 to len(str) - 1
        c = char_at(str, i)
        if not ( is_numeric(c) or c = "-" ) then
            rv = false
        end if
    next

    is_int = rv
end function


function lstrip(str as string) as string
    dim i as integer
    i = 0

    do while char_at(str, i) = " "
        i = i + 1
    loop

    lstrip = substring(str, i)
end function


function replace_char(text_ as string, c1 as string, c2 as string) as string
    dim replaced as string
    replaced = ""

    dim i as integer
    dim c as string

    for i = 0 to len(text_) - 1
        c = char_at(text_, i)
        if c = c1 then
            replaced = replaced & c2
        else
            replaced = replaced & c
        end if
    next

    replace_char = replaced
end function

' --------------------------------

function inspect(val) as string
    dim rv

    if TypeName(val) = "String" then
        rv = inspect_str(val)
    elseif TypeName(val) = "Integer" then
        rv = "" & val
    elseif TypeName(val) = "Double" then
        rv = "" & val
    elseif TypeName(val) = "Object" then
        if obj_typename(val) = "List" then
            rv = List_inspect(val)
        elseif obj_typename(val) = "Map" then
            rv = Map_inspect(val)
        elseif obj_typename(val) = "Token" then
            rv = Token_inspect(val)
        else
            rv = "<unknown_obj>"
        endif
    else
        rv = "<UNKNOWN> " & TypeName(val)
    end if

    inspect = rv
end function


function inspect_str(str) as string
    dim rv as string
    dim i as integer
    dim s as string
    dim c as string
    s = ""

    dim DQ_ as string : DQ_ = dq()
    dim LF_ as string : LF_ = lf()
    dim BS_ as string : BS_ = bs()

    for i = 0 to len(str) - 1
        c = char_at(str, i)
        if c = LF_ then
            s = s & LF_
        elseif c = DQ_ then
            s = s & BS_ & DQ_
        else
            s = s & c
        end if
    next

    rv = DQ_ & s & DQ_
    inspect_str = rv
end function


function type_name_ex(val)
    dim rv

    select case TypeName(val)
        case "Integer"
            rv = TypeName(val)
        case "String"
            rv = TypeName(val)
        case else
            rv = obj_typename(val)
    end select

    type_name_ex = rv
end function


function obj_typename(obj)
    on local error goto on_error

    obj_typename = obj.type_
    exit function

on_error:
    if err = 423 then
        rem type_ プロパティが存在しない場合
        obj_typename = "Object"
    else
        msgbox format_err_msg("obj_typename", err, erl, error$)
        __ERROR__
    end if
end function


function get_sheet(name_)
    get_sheet = ThisComponent.Sheets.getByName(name_)
end function


function get_active_sheet()
    get_active_sheet = ThisComponent.CurrentController.ActiveSheet
end function

rem --------------------------------

function _get_shape_by_name(name_)
    dim sh
    set sh = get_active_sheet()
    _get_shape_by_name = _get_shape_by_name_v2(sh, name_)
end function


function _get_shape_by_name_v2(sh, name_)
    dim dp, count, i, shape
    dp = sh.Drawpage
    count = dp.Count

    dim target_i
    For i = 0 to count - 1
        shape = dp.getByIndex(i)
        If shape.Name = name_ then
            target_i = i
        end If
    next i

    _get_shape_by_name_v2 = dp.getByIndex(target_i)
end function


sub clear_log_box()
    dim box
    box = _get_shape_by_name("log")
    box.string = ""
end sub


' TODO Use clear_log_box()
sub init_log2()
    dim box
    box = _get_shape_by_name("log")
    box.string = ""
end sub


sub log2(text_)
    dim box
    box = _get_shape_by_name("log")
    box.string = box.string & text_ & chr(13)
end sub


sub logkv2(k, v)
    log2(k & " (" & inspect(v) & ")")
end sub

' --------------------------------

sub box_text_set(sheet_name, box_name, text_)
    dim rv
    dim sh, box
    sh = get_sheet(sheet_name)
    box = _get_shape_by_name_v2(sh, box_name)
    box.string = "" & text_
end sub


function box_text_get(sheet_name, box_name) as string
    dim rv as string
    dim sh, box
    sh = get_sheet(sheet_name)
    box = _get_shape_by_name_v2(sh, box_name)
    rv = box.string

    box_text_get = rv
end function


function box_read_lines(sheet_name as string, box_name as string)
    dim src as string
    src = box_text_get(sheet_name, box_name)

    dim raw_lines
    raw_lines = split(src, lf())

    dim lines
    lines = List.new_()

    dim i as integer
    dim line_
    for i = 0 to ubound(raw_lines)
        line_ = raw_lines(i)
        if line_ <> "" then
            List.add(lines, line_)
        end if
    next

    box_read_lines = lines
end function

' --------------------------------

function is_numeric(c)
    is_numeric = str_include("0123456789", c)
end function


function get_ri_max(sheet)
    dim range
    range = sheet.getCellRangeByName("A1")
    dim cursor
    cursor = sheet.createCursorByRange(range)
    cursor.gotoEndOfUsedArea(True)
    get_ri_max = cursor.Rows.Count - 1
end function


function time_sec() as long
    time_sec = (Now() - 25569) * 86400
end function


function format_err_msg(text_, err, erl_, error_) as string
  format_err_msg = text_ & "(line " & erl_ & ") " & err_ & " " & error_
end Function
