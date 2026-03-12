#!/usr/bin/env bash
set -euo pipefail

FC=${FC:-ifx}          # 環境変数FCがあればそれを使う。なければifx
FFLAGS=${FFLAGS:--O3}  # 環境変数FFLAGSがあればそれを使う

EXE=judgeTD_tool
SRC="inout.f90 TD_bst.f90 search.f90  calc_distance.f90 calc_CE1.f90 calc_CE2.f90 main.f90"

rm -f *.o *.mod "$EXE"

$FC $FFLAGS -c inout.f90
$FC $FFLAGS -c TD_bst.f90
$FC $FFLAGS -c search.f90
$FC $FFLAGS -c calc_distance.f90
$FC $FFLAGS -c calc_CE1.f90
$FC $FFLAGS -c calc_CE2.f90
$FC $FFLAGS -c main.f90
$FC $FFLAGS -o "$EXE" inout.o TD_bst.f90 search.o calc_distance.o calc_CE1.o calc_CE2.o main.o

echo "Built: $EXE  (FC=$FC)"
