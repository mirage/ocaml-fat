#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let opam =
  Pkg.opam_file ~lint_deps_excluding:(Some ["ppx_tools"]) "opam"

let () =
  Pkg.describe ~opams:[opam] "fat-filesystem" @@ fun c ->
  Ok [ Pkg.mllib ~api:["Fat"] "src/fat.mllib";
       Pkg.bin   ~dst:"fat" "bin/main";
       Pkg.test  "test/test";
       Pkg.test ~run:false "bin/shell"; ]
