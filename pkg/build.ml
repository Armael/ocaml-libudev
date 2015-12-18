#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  Pkg.describe "libudev" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/libudev";
    Pkg.stublibs ~exts:Exts.c_dll_library "src/dlludev_stubs";
    Pkg.doc "examples/monitoring.ml";
    Pkg.doc "examples/searching.ml"; ]
