#!/bin/bash
TEX_FILE="eda-repositorios-artigo"

xelatex "$TEX_FILE"
xelatex "$TEX_FILE"
biber referencias
xelatex "$TEX_FILE"
makeglossaries "$TEX_FILE"
xelatex "$TEX_FILE"
