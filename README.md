`ogaml` is just an OCaml [game of life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life), made for learning. It needs some optimization but it is working properly.

```sh
$ opam install graphics
$ ocamlfind ocamlopt -o ogaml -linkpkg -package graphics oga.ml
$ ./ogaml
```

| Key      | Functionality                                     |
|----------|---------------------------------------------------|
| `r`      | Load and start a new random world                 |
| `c`      | New empty world (left click to draw on it)        |
| `n`      | Display next generation incrementally             |
| `s`      | Save currently drawn world (press space to start) |
| `<Space>`| Start/pause                                       |
| `<Esc>`  | Close                                             |
