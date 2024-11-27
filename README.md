`ogaml` is just an OCaml [game of life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life), made for learning.
It is now really fast.

```sh
$ opam install graphics
$ ocamlfind ocamlopt -o ogaml -linkpkg -package graphics oga.ml
$ ./ogaml
```

| Key      | Functionality                                     |
|----------|---------------------------------------------------|
| `<Space>`| Start/pause                                       |
| `n`      | Display next generation incrementally             |
| `<Esc>`  | Close                                             |
