#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  Pkg.describe "libudev" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/libudev";
    Pkg.doc "examples/monitoring.ml";
    Pkg.doc "examples/searching.ml"; ]
