rotthdir := (justfile_directory() + "/rotth-src")

default:
  @just --list

build FILE:
    cargo run -- --compile -- {{rotthdir}}/{{FILE}}.rh
    nasm -g -F dwarf -f elf64 {{rotthdir}}/{{FILE}}.asm -o {{rotthdir}}/{{FILE}}.o
    ld -o {{rotthdir}}/{{FILE}} {{rotthdir}}/{{FILE}}.o

run FILE: (build FILE)
    {{rotthdir}}/{{FILE}}

clean:
    find {{rotthdir}} -type f  ! -name "*.rh"  -delete