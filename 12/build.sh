#!/bin/bash
ocamlopt $1
./a.out

rm *.cmi
rm *.cmx
rm *.o
rm *.out