#!/bin/sh
GCC=riscv64-linux-musl-gcc
CPU=rv64i
ABI=lp64

$GCC -mabi=$ABI -march=$CPU "$@"
