`ogaml` is an OCaml game of life.

```console
opam install graphics
```

```console
# compile in native code:
ocamlfind ocamlopt -o ogaml -linkpkg -package graphics oga.ml
./ogaml
```

```ocaml
(* Using utop *)
#require "graphics";;
#use "oga.ml";;
big_bang world;;
```
