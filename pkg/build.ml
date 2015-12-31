#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;
#use "config.ml";;

let () =
  Vars.subst ~skip:Config.subst_skip ~vars:Config.vars ~dir:"." >>& fun () ->
  Pkg.describe "libudev" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/udev";
    Pkg.stublibs "src/dlludev_stubs.so";
    Pkg.doc "examples/monitoring.ml";
    Pkg.doc "examples/searching.ml"; ]
