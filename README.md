## OCaml bindings to OS X FSEvents

This package relies on
[osx-cf](https://github.com/dsheets/ocaml-osx-cf) for CoreFoundation
support. These bindings use
[ctypes](https://github.com/ocamllabs/ocaml-ctypes) for type-safe stub
generation.

### FSEvents Quirks

`chmod` will result in an `ItemChangeOwner` event.
