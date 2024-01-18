`ogaml` is just an OCaml [game of life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life), made for learning. It needs some optimization but it is working properly.

```sh
$ opam install graphics
$ ocamlfind ocamlopt -o ogaml -linkpkg -package graphics oga.ml
$ ./ogaml
```

| Key      | Functionality           |
|----------|-------------------------|
| `r`      | Load a new random world |
| `<Space>`| Play/pause the game     |
| `<Esc>`  | Close the game          |
