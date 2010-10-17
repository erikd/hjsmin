#!/bin/sh

# Script to minify all the files in ./unminified/, results with same file name in ./minified

#find ./unminified -iname "*.js" 

#for i in ./unminified/*.js
for i in `(cd ./unminified;ls *.js)`
do
  #echo "./unminified/"$i "./minified/"$i
  ./jsminplus/run.php "./unminified/"$i >  "./minified/"$i
done