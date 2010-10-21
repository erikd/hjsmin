#!/bin/sh

# Script to minify all the files in ./unminified/, results with same file name in ./minified

for i in `(cd ./unminified;ls *.js)`
do
  #echo "./unminified/"$i "./minified/"$i
  ./jsminplus/run.php "./unminified/"$i >  "./minified/"$i
done

for i in `(cd ./parsingonly;ls *.js)`
#for i in `(cd ./parsingonly;ls 20_*.js)`
do
  #echo "./unminified/"$i "./minified/"$i
  ./jsminplus/run.php "./parsingonly/"$i >  "./pminified/"$i
done