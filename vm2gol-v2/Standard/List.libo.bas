rem -*- mode: basic -*-

type List
    _xs as Array
    type_ as String
    len as integer
    cap as integer ' capacity
end type


function new_
    dim cap
    cap = 1

    dim _xs(cap - 1) as variant
    dim xs as New List

    xs._xs = _xs
    xs.type_ = "List"
    xs.len = 0
    xs.cap = cap
    new_ = xs
end function


function create
    dim cap
    cap = 1

    dim _xs(cap - 1) as variant
    dim xs as New List

    xs._xs = _xs
    xs.type_ = "List"
    xs.len = 0
    xs.cap = cap
    create = xs
end function


function _extend(self)
    dim newcap as integer
    newcap = self.cap * 2

    dim newxs
    newxs = self._xs
    redim preserve newxs(newcap - 1)

    self._xs = newxs
    self.cap = newcap
    
    _extend = self
end function


sub add(self, val)
    if self.cap <= self.len then
        self = _extend(self)
    end if

    self._xs(self.len) = val
    self.len = self.len + 1
end sub


sub add_els(self, els)
    dim i as integer
    dim el
    for i = 0 to List.size(els) - 1
        el = List.get_(els, i)
        List.add(self, el)
    next
end sub


function get_(self as List, i)
    get_ = self._xs(i)
end function


function is_list(val)
    dim retval as boolean
    retval = false

    if IsNull(val) then
        retval = false
    elseif isArray(val) = true then
        retval = false
    elseif TypeName(val) = "String" then
        retval = false
    elseif TypeName(val) = "Object" then
        dim xs as List
        xs = val
        if xs.type_ = "List" then
            retval = true
        end if
    end if

    is_list = retval
end function


function head(self)
    head = get_(self, 0)
end function


function rest(self)
    dim newlist, i
    newlist = new_()

    if 2 <= self.len then
        for i = 1 to (self.len - 1)
            List.add(newlist, List.get_(self, i))
        next
    end if

    rest = newlist
end function


function index(self, elem)
    dim rv
    dim i, found

    i = 0
    found = false
    do while i < self.len
        if List.get_(self, i) = elem then
            found = true
            exit do
        end if
        i = i + 1
    loop

    if found then
        rv = i
    else
        rv = null
    end if

    index = rv
end function


function include(self, elem)
    dim rv
    dim i

    i = index(self, elem)
    rv = not IsNull(i)

    include = rv
end function


function List_inspect(self) as string
    dim rv
    dim i

    rv = "["
    for i = 0 to self.len - 1
        if 1 <= i then
            rv = rv & ", "
        end if
        rv = rv & inspect(List.get_(self, i))
    next
    rv = rv & "]"

    List_inspect = rv
end function


function size(self)
      size = self.len
end function


function reverse(self)
    dim newlist
    newlist = List.new_()

    dim i as integer
    dim el
    i = List.size(self) - 1
    do while 0 <= i
        el = List.get_(self, i)
        List.add(newlist, el)
        i = i - 1
    loop

    reverse = newlist
end function


function from_array(xs)
    dim rv

    rv = List.new_()

    dim i as integer
    for i = 0 to ubound(xs)
        List.add(rv, xs(i))
    next

    from_array = rv
end function
