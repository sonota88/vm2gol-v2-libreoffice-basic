rem -*- mode: basic -*-

Option Explicit

type Map
    data as object ' com.sun.star.container.XMap
    type_ as string
end type


function new_()
    dim rv as New Map
    
    rv.data = com.sun.star.container.EnumerableMap.create("string", "any")
    rv.type_ = class_name

    new_ = rv
end function


function class_name() as string
    class_name = "Map"
end function


sub set_(self as Map, k as string, v)
    self.data.put(k, v)
end sub


function get_(self as Map, k as string)
    dim rv

    if self.data.containsKey(k) then
        rv = self.data.get(k)
    else
        rv = null
    end if

    get_ = rv
end function
