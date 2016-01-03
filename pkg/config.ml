#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

module Config = struct
  include Config_default

  let vars =
    [ "NAME", "libudev";
      "VERSION", "0.2";
      "MAINTAINER", "Armael <armael@isomorphis.me>"; ]
end
