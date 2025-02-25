#!/bin/bash

# $1: Input NetCDF file
# $2: Grid description ascii filename

cdo griddes $1 > $2
