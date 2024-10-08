#!/bin/bash

echo "merging source and sinks"

apis=`cat iccta/res/SourcesAndSinks_tmp.txt`

#echo `echo $apis | wc -l` apis

cp iccta/res/susi.txt iccta/res/SourcesAndSinks.txt

IFS=$'\n'

for api in $apis
do

v=`grep -F '$api' iccta/res/susi.txt`

if [ $? -ne 0 ]
then

echo $api  >> iccta/res/SourcesAndSinks.txt

fi

done

echo "done merging source and sinks"
