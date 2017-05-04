#/bin/bash
# inlines referenced files into a markdown file

INPUT=README.md.in
OUTPUT=README.md

perl -ne 's/^\\input\{(.+)\}$/`cat $1`/e;print' ${INPUT} > ${OUTPUT}
