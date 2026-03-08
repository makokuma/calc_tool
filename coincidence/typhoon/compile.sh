#!/usr/bin/env bash
set -euo pipefail

FC=${FC:-ifx}          # 環境変数FCがあればそれを使う。なければifx
FFLAGS=${FFLAGS:--O3}  # 環境変数FFLAGSがあればそれを使う

EXE=judgeTD_tool
SRC="inout.f90 main.f90"

rm -f *.o *.mod "$EXE"

$FC $FFLAGS -c inout.f90
$FC $FFLAGS -c main.f90
$FC $FFLAGS -o "$EXE" inout.o main.o

echo "Built: $EXE  (FC=$FC)"
