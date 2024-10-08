#!/bin/bash

# produce permission pair
file=$1

filename=$(basename $file)
filename=`echo $filename | awk -F ".apk" '{print $1}'`

content=`grep -v "PATH" $file | awk -F "FLOW: " '{print $2}' | grep -v "@parameter" | sort | uniq` 

IFS=$'\n'
echo "" > PermissionPair_temp.txt
for line in $content
do
source=`echo $line | awk -F "==>" '{print $1}'`
    sink=`echo $line | awk -F "==>" '{print $2}'`
	
source_perm=`grep -F $source mapping.txt | awk -F ";" '{print $2}'`
sink_perm=`grep -F $sink mapping.txt | awk -F ";" '{print $2}'`


for src_perm in $source_perm
do
for sn_perm in $sink_perm
do
    		echo $filename";"$src_perm";"$sn_perm >> PermissionPair_temp.txt

done
	done
 done 


sort PermissionPair_temp.txt | uniq >> output/PermPairTest.txt
rm PermissionPair_temp.txt


