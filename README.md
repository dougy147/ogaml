`ogaml` is just an OCaml [game of life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life), made for learning. It needs some optimization but it is working properly.

```sh
$ opam install graphics
$ ocamlfind ocamlopt -o ogaml -linkpkg -package graphics oga.ml
$ ./ogaml
```

| Key      | Functionality                              |
|----------|--------------------------------------------|
| `r`      | Load and start a new random world          |
| `n`      | New empty world (left click to draw on it) |
| `s`      | Start current state of the world           |
| `<Space>`| Play/pause                                 |
| `<Esc>`  | Close                                      |
