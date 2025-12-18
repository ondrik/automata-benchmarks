#!/bin/bash

if [ $# -ne 2 ]; then
    echo "Usage: $0 <file_with_formula_paths> <tool_command>"
    exit 1
fi

PATHS_FILE="$1"
TOOL="$2"

if [ ! -f "$PATHS_FILE" ]; then
    echo "Error: file '$PATHS_FILE' not found"
    exit 1
fi

suffix="$(date +%Y%m%d_%H%M%S)"
classifications="classifications_$suffix"
OUTDIR="outputs_$suffix"
mkdir -p "$OUTDIR"

echo "name;empty;deterministic;inherently weak;semi deterministic;terminal;unambiguous;weak;very weak;elevator;one-state;" > "$classifications"
i=1

while IFS= read -r INPUT_FILE; do
    [ -z "$INPUT_FILE" ] && continue

    if [ ! -f "$INPUT_FILE" ]; then
        echo "Warning: formula file '$INPUT_FILE' not found, skipping."
        continue
    fi
    

    while IFS= read -r formula; do
        [ -z "$formula" ] && continue
        formula="${formula%$'\n'}"
        echo "Processing formula $i from file $INPUT_FILE: $formula"

        "$TOOL" -f "$formula" | awk '/^HOA: v1/ {flag=1} flag' > "$OUTDIR/out_$i.hoa"
        ./classify_ba.sh --csv "$OUTDIR/out_$i.hoa" >> "$classifications"

        i=$((i+1))
    done < "$INPUT_FILE"


done < "$PATHS_FILE"

python3 gen_pdf.py "$classifications"
python3 gen_st.py "$OUTDIR"