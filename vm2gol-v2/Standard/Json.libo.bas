rem -*- mode: basic -*-

Option Explicit

' --------------------------------

function parse(_json as string)
    dim parse_result
    parse_result = _parse_json(_json)
    parse = parse_result(0)
end function


function _parse_json(_json as string)
    dim pos as integer
    dim retval, c, rest
    dim str
    dim DQ_, LF_
    DQ_ = Utils.dq()
    LF_ = Utils.lf()

    dim xs
    xs = List.new_(4)

    pos = 1
    do while pos < len(_json)
        rest = substring(_json, pos)
        c = char_at(rest, 0)

        if c = "[" then
            dim list_elem
            dim size
            retval = _parse_json(rest)
            list_elem = retval(0)
            size = retval(1)
            List.add(xs, list_elem)
            pos = pos + size
        elseif c = "]" then
            pos = pos + 1
            exit do
        elseif c = " " or c = "," or c = LF_ then
            pos = pos + 1
        elseif c = DQ_ then
            str = consume_str(rest)
            List.add(xs, str)
            pos = pos + len(str) + 2
        elseif Utils.is_numeric(c) or c = "-" then
            str = consume_int(rest)
            List.add(xs, CInt(str))
            pos = pos + len(str)
        else
            pos = pos + 1
        end if

    loop

    _parse_json = Array(xs, pos)
end function


function consume_int(rest as string) as string
    dim pos, c

    pos = 0
    do while pos < len(rest)
        c = char_at(rest, pos)
        if not (Utils.is_numeric(c) or c = "-") then
            exit do
        end if
        pos = pos + 1
    loop

    consume_int = left(rest, pos)
end function


function consume_str(rest as string) as string
    dim DQ_ as string
    DQ_ = Utils.dq()

    dim pos as integer
    pos = 1

    dim c as string

    do while pos < len(rest)
        c = char_at(rest, pos)
        if c = DQ_ then
            exit do
        end if
        pos = pos + 1
    loop

    dim s1
    s1 = left(rest, pos)
    consume_str = substring(s1, 1)
end function


function indent(n as integer) as string
    indent = Space(2 * n)
end function


function _list_to_json_oneline(xs) as string
    dim s, retval as string
    dim i as integer
    dim val
    dim DQ_ as string
    DQ_ = Utils.dq()

    s = "["

    i = -1
    do while i + 1 < xs.len
        i = i + 1
        val = List.get_(xs, i)
        s = s

        if IsNull(val) then
            __ERR__ rem null は来ない前提
        elseif List.is_list(val) then
            s = s & _list_to_json_oneline(val)
        else
            select case TypeName(val)
            case "Integer", "Double"
                s = s & val
            case "String"
                s = s & DQ_ & val & DQ_
            case else
                s = s & "unsupported type: " & TypeName(val)
            end select
        end if

        if i < xs.len - 1 then
            s = s & ", "
        end if
    loop

    s = s & "]"

    _list_to_json_oneline = s
end function


function _list_to_json(xs, lv as integer) as string
    dim s, retval as string
    dim i as integer
    dim val
    dim list_s as string
    dim DQ_, LF_
    DQ_ = Utils.dq()
    LF_ = Utils.lf()

    s = indent(lv) & "["
    s = s & LF

    i = -1
    do while i + 1 < xs.len
        i = i + 1
        val = List.get_(xs, i)
        s = s

        if IsNull(val) then
            __ERR__ rem null は来ない前提
        elseif List.is_list(val) then
            list_s = _list_to_json_oneline(val)
            if len(list_s) <= 20 then
                s = s & indent(lv + 1) & list_s
            else
                s = s & _list_to_json(val, lv + 1)
            end if
        else
            select case TypeName(val)
            case "Integer", "Double"
                s = s & indent(lv + 1) & val
            case "String"
                s = s & indent(lv + 1) & DQ_ & val & DQ_
            case else
                s = s & indent(lv) & "unsupported type: " & TypeName(val)
            end select
        end if

        if i < xs.len - 1 then
            s = s & ", "
        end if

        s = s & LF_
    loop

    s = s & indent(lv) & "]"

    _list_to_json = s
end function


function to_json(xs) as string
    to_json = _list_to_json(xs, 0)
end function
