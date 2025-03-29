#!/bin/bash  

# ./publish.sh FILE
# FILE = filename

FILE=$1

echo ""
echo "Publishing $FILE to quarto-pub"
echo ""
quarto publish quarto-pub "$FILE" --no-browser
rm _quarto.yml
rm -r *_files
rm *.html
