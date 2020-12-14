rem -*- mode: basic -*-

Option Explicit

const RI_MAX = 1999
const CI_OUT_ASM = 3

dim g_exe_code as string

' --------------------------------

Sub onclick_assemble
    Utils.setup()
    Utils.init_log()

    dim exe_code as string
    exe_code = assemble()

    box_text_set("asm", "exe", exe_code)
    box_text_set("exe", "exe", exe_code)

    Utils.log "done"
End Sub


sub onclick_clear()
    Utils.init_log()
    _clear_output()
end sub


sub _clear_output
    box_text_set("asm", "exe", "")
    box_text_set("exe", "exe", "")
end sub

' --------------------------------

function is_label_line(_line)
    is_label_line = (left(_line, 5) = "label")
end function


function _make_label_addr_map(lines)
    dim label_addr_map
    dim _line
    dim v1
    dim ri

    label_addr_map = Map.new_()

    for ri = 0 to (lines.len - 1)
      _line = List.get_(lines, ri)

      if is_label_line(_line) then
        v1 = split(_line, " ")(1)
        Map.set_(label_addr_map, v1, ri)
      end if
    next
  
    _make_label_addr_map = label_addr_map
end function


sub _puts(line_)
    g_exe_code = g_exe_code & line_ & lf()
end sub


sub _assemble(lines, label_addr_map)
    dim ri as integer
    dim _line, xs
    dim v0, v1

    for ri = 0 to (lines.len - 1)
        _line = List.get_(lines, ri)

        if not is_label_line(_line) then
          _line = substring(_line, 2)
        end if

        xs = List.from_array(split(_line, " "))
        v0 = List.get_(xs, 0)

        if ( _
             (v0 = "jump") _
          or (v0 = "jump_eq") _
          or (v0 = "call") _
        ) then
            v1 = List.get_(xs, 1)
            _puts(v0 & " " & Map.get_(label_addr_map, v1))
        elseif v0 = "" then
            ' Utils.log("skip")
        else
            dim s
            s = v0
            if 2 <= List.size(xs) then
              s = s + " " + List.get_(xs, 1)
            end if
            if 3 <= List.size(xs) then
              s = s + " " + List.get_(xs, 2)
            end if
            _puts(s)
        end if
    next
end sub


function _filter_lines(raw_lines)
    dim lines
    lines = List.create()

    dim i as integer
    dim line_ as string
    for i = 0 to List.size(raw_lines)
      line_ = List.get_(raw_lines, i)
      if char_at(lstrip(line_), 0) = "#" then
        ' skip
      elseif line_ = "" then
        ' skip
      else
        List.add(lines, line_)
      end if
    next

    _filter_lines = lines
end function


function assemble() as string
    dim label_addr_map

    g_exe_code = ""

    Utils.log("ラベル→アドレスのマッピングを作成")

    dim raw_lines
    raw_lines = box_read_lines("asm", "asm")

    dim lines
    lines = _filter_lines(raw_lines)

    label_addr_map = _make_label_addr_map(lines)

    rem map_dump(label_addr_map)

    Utils.log("機械語コードに変換")

    clear_cell_range(sh_asm, CI_OUT_ASM, 0, CI_OUT_ASM, RI_MAX)
    clear_cell_range(sh_exe, 0, 0, 0, RI_MAX)
    wait(100)

    _assemble(lines, label_addr_map)

    assemble = g_exe_code
end function
