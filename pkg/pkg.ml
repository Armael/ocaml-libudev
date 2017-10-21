#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "libudev"
    ~opams:[Pkg.opam_file "opam" ~lint_deps_excluding:(Some ["ctypes-foreign"])]
    ~licenses:[]
    ~change_logs:[]
  @@ fun _ ->
  Ok [
    Pkg.mllib "src/udev.mllib";
    Pkg.stublibs "src/dlludev_stubs.so";
    Pkg.doc "examples/monitoring.ml";
    Pkg.doc "examples/searching.ml";
  ]
