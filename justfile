root := justfile_directory()
rotthdir := (root + "/rotth-src")

default:
  @just --list

build FILE:
    cargo run -- --compile -- {{rotthdir}}/{{FILE}}.rh
    nasm -g -F dwarf -f elf64 {{root}}/print.asm -o {{root}}/print.o
    nasm -g -F dwarf -f elf64 {{rotthdir}}/{{FILE}}.asm -o {{rotthdir}}/{{FILE}}.o
    ld -o {{rotthdir}}/{{FILE}} {{rotthdir}}/{{FILE}}.o {{root}}/print.o

run FILE: (build FILE)
    {{rotthdir}}/{{FILE}}

clean-run FILE: clean (run FILE)

clean:
    find {{rotthdir}} -type f  ! -name "*.rh"  -delete