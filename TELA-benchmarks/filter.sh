#!/bin/bash

# Check arguments
if [ $# -ne 1 ]; then
    echo "Usage: $0 <csv file>"
    exit 1
fi

PATH_FILE="$1"

cat $PATH_FILE \
| awk -F';' 'NR==1 || $2!="1"' \
| awk -F';' 'NR==1 || $3!="1"' \
| awk -F';' 'NR==1 || $4!="1"' \
| awk -F';' 'NR==1 || ( $2+$3+$4+$5+$6+$7+$8+$9+$10+$11 > 0 )' \
> filtered.csv

mkdir -p filtered

tail -n +2 filtered.csv | cut -d';' -f1 | while read -r fname; do
    cp "$fname" filtered/ 2>/dev/null
done