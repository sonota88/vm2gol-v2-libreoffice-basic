rem -*- mode: basic -*-

Option Explicit

sub onclick_clear()
    Utils.init_log()
    _clear_output()
end sub


sub onclick_tokenize()
    dim src, sh_src, sh_tok

    Utils.setup()
    Utils.init_log()

    sh_src = Utils.get_sheet("src")
    sh_tok = Utils.get_sheet("tokens")

    src = box_text_get("src", "src")

    _clear_output()
    wait(100)

    tokenize(src, sh_src, 3, 0)
    _copy_to_tokens_sheet(sh_src, sh_tok)

    Utils.log("done")
end sub


sub _clear_output()
    dim sh_src, base_ci
    sh_src = Utils.get_sheet("src")

    dim ri_max as Integer
    ri_max = get_ri_max(sh_src)

    base_ci = 3
    clear_cell_range(sh_src, base_ci, 0, base_ci + 1, ri_max)

    dim sh_next
    sh_next = Utils.get_sheet("tokens")
    base_ci = 0
    clear_cell_range(sh_next, base_ci, 0, base_ci + 1, ri_max)
end sub

	
sub _copy_to_tokens_sheet(sh_src, sh_tok)
    dim src_ci, dest_ci, ri, val
    src_ci = 3
    dest_ci = 0

    clear_cell_range(sh_tok, dest_ci, 0, dest_ci + 1, 10)

    dim ri_max as Integer
    ri_max = get_ri_max(sh_src)

    for ri = 0 to ri_max
        val = cell_get(sh_src, src_ci, ri)
        cell_set(sh_tok, dest_ci, ri, val)
        val = cell_get(sh_src, src_ci + 1, ri)
        cell_set(sh_tok, dest_ci + 1, ri, val)
    next
end sub

' --------------------------------

Sub tokenize(src, sh_dest, ci0, ri0)
    Utils.log("-->> tokenize()")

    dim pos, rest_, head, out_i
    dim token, sym, str, size

    pos = 0
    out_i = 0
    do while pos < len(src)
        if (pos mod 200 = 0) then
            Utils.log("... " & pos)
        end if

        rest_ = substring(src, pos)

        token = tok_sub(rest_)
        if not IsNull(token) then
            sym  = token(0)
            str  = token(1)
            size = token(2)
            if sym <> "comment" then
                output_token(sh_dest, ci0, ri0 + out_i, sym, str)
                out_i = out_i + 1
            end if
            pos = pos + size
        else
            pos = pos + 1
        end if
    loop

    Utils.log("... " & pos)
End Sub


sub output_token(sheet, ci, ri, sym, str)
    cell_set(sheet, ci    , ri, sym)
    cell_set(sheet, ci + 1, ri, str)
end sub


function tok_sub(rest_)
    dim rv, size, str
    rv = null

    if left(rest_, 1) = " " then
        rv = null
        tok_sub = rv
        exit function
    end if

    size = _match_comment(rest_)
    if 0 < size then
        rv = Array("comment", null, size)
        tok_sub = rv
        exit function
    end if

    size = _match_str(rest_)
    if 0 < size then
        str = substring(rest_, 1, size - 1)
        rv = Array("str", str, size)
        tok_sub = rv
        exit function
    end if

    size = _match_kw(rest_)
    if 0 < size then
        str = left(rest_, size)
        rv = Array("kw", str, size)
        tok_sub = rv
        exit function
    end if

    size = _match_int(rest_)
    if 0 < size then
        str = left(rest_, size)
        rv = Array("int", str, size)
        tok_sub = rv
        exit function
    end if

    size = _match_sym(rest_)
    if 0 < size then
        str = left(rest_, size)
        rv = Array("sym", str, size)
        tok_sub = rv
        exit function
    end if

    size = _match_ident(rest_)
    if 0 < size then
        str = left(rest_, size)
        rv = Array("ident", str, size)
        tok_sub = rv
        exit function
    end if

    tok_sub = rv
end function


function _match_comment(rest_) as integer
    dim rv

    if left(rest_, 2) <> "//" then
        _match_comment = 0
        exit function
    end if

    dim i, c
    i = 2
    do while i < len(rest_)
    c = char_at(rest_, i)
        if c = lf() then
            exit do
        end if
        i = i + 1
    loop
    rv = i + 1

    _match_comment = rv
end function


function _match_str(rest_) as integer
    dim rv

    if left(rest_, 1) <> dq() then
        _match_str = 0
        exit function
    end if

    dim i, c
    i = 1
    do while i < len(rest_)
    c = char_at(rest_, i)
        if c = dq() then
            exit do
        end if
        i = i + 1
    loop
    rv = i + 1

    _match_str = rv
end function


function _match_kw(rest_) as integer
    dim head, next_c, size

    size = 8
    head = left(rest_, size)
    next_c = char_at(rest_, size)
    if ( _
      ( _
           (head = "call_set") _
      )_
      and not is_ident_char(next_c) _
    ) then
        _match_kw = size
        exit function
    end if

    size = 6
    head = left(rest_, size)
    next_c = char_at(rest_, size)
    if ( _
      ( _
           (head = "return") _
      )_
      and not is_ident_char(next_c) _
    ) then
        _match_kw = size
        exit function
    end if

    size = 5
    head = left(rest_, size)
    next_c = char_at(rest_, size)
    if ( _
      ( _
           (head = "while") _
      )_
      and not is_ident_char(next_c) _
    ) then
        _match_kw = size
        exit function
    end if

    size = 4
    head = left(rest_, size)
    next_c = char_at(rest_, size)
    if ( _
      ( _
           (head = "func") _
        or (head = "call") _
        or (head = "case") _
      )_
      and not is_ident_char(next_c) _
    ) then
        _match_kw = size
        exit function
    end if

    size = 3
    head = left(rest_, size)
    next_c = char_at(rest_, size)
    if ( _
           head = "var" _
        or head = "set" _
      ) and (not is_ident_char(next_c) _
    ) then
        _match_kw = size
        exit function
    end if

    _match_kw = 0
end function


function _match_int(rest_) as integer
    dim pos, c

    pos = 0
    do while pos < len(rest_)
        c = char_at(rest_, pos)
        if not str_include("-0123456789", c) then
            exit do
        end if
        pos = pos + 1
    loop

    _match_int = pos
end function


function _match_sym(rest_) as integer
    dim rv

    if left(rest_, 2) = "==" or left(rest_, 2) = "!=" then
        rv = 2
    elseif instr(";,+*=(){}", left(rest_, 1)) <> 0 then
        rv = 1
    else
        rv = 0
    end if

    _match_sym = rv
end function


function _match_ident(rest_) as integer
    dim pos

    pos = 0
    do while pos < len(rest_)
        if not is_ident_char(char_at(rest_, pos)) then
            exit do
        end if
        pos = pos + 1
    loop

    _match_ident = pos
end function


function is_ident_char(c) as boolean
    dim i
    i = Instr("0123456789abcdefghijklmnopqrstuvwxyz_[]", c)
    is_ident_char = i <> 0
end function
