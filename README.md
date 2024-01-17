`ogaml` is an OCaml game of life. It needs to be optimized but is working properly, written in a functional style.

```console
opam install graphics
ocamlfind ocamlopt -o ogaml -linkpkg -package graphics oga.ml
./ogaml
```

```ocaml
(* or using utop *)
#require "graphics";;
#use "oga.ml";;
big_bang world;;
```
