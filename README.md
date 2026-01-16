# dwarf-evaluator

This is an evaluator for DWARF expressions implemented in OCaml.  It
aims to be concise and lightweight.  It can be used by tool developers
to learn and understand DWARF by examining the precise definitions of
DWARF operators and by running examples.  The evaluator follows the
"locations on the stack" semantics that is defined by DWARF 6.

There exist a [web playground](https://intel.github.io/dwarf-evaluator/).
Concrete examples can be shared easily via playground links,
[like this](https://intel.github.io/dwarf-evaluator/?context=%28%29&input=DW_OP_lit10%0ADW_OP_lit4%0ADW_OP_plus%0ADW_OP_lit3%0ADW_OP_mul%0A).

## Getting Started

For Ubuntu/Debian you can get started by installing `opam` (the OCaml package
manager) using the `apt` package manager, and then creating a "switch" (virtual
environment) for the project:

```
$ sudo apt install opam # or any other recommended means for your OS
$ opam init # follow the prompts here; accepting defaults changes ~/.profile
            # which means you won't have to remember to `eval $(opam env)` in
            # every new shell to access your switches
$ cd path/to/dwarf-evaluator
$ opam switch create . 5.3.0 # creates a hermetic environment used for this dir
                             # with the 5.3.0 OCaml compiler
```

At this point, whenever your shell is in the project directory you can run the
code with `ocaml`:

```
$ ocaml dwarf_evaluator.ml
...
```

For an interactive REPL, you can install `utop` and `#use` the source:

```
$ opam install utop
$ utop
...
utop # #use "dwarf_evaluator.ml";;
...
```

### Building the playground

To transpile to JavaScript and generate a "playground" HTML file, install the
dependencies and build via `dune`:

```
$ opam install --deps-only . # this pulls from *.opam files in the current dir
$ opam install dune
$ dune build
```

Then point your browser at `_build/default/js/dwarf_evaluator.html`.

This builds with the `dev` profile by default, which leaves the `js` source
external to the HTML file and includes sourcemaps for easier debugging. To
package it all together and minimize it you can instead build with the
`release` profile:

```
$ dune build --profile=release
```

At which point the resulting `html` is entirely self-contained.
