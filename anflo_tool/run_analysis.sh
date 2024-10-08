#!/bin/bash

# dir where this tool resides
curr=/Users/MrX/InforLeakDir

# modifiy this accordingly
PLATFORMSPATH=~/Library/Android/sdk/platforms


cd $curr

mkdir $curr/output

app=$1

# this is an optimization. instead of providing all the 32k api from pscout
# we limit to the ones that are actually present in the app
# this significantly improved speed
rm $curr/iccta/res/SourcesAndSinks.txt

echo "running source and sink extraction"
java -jar extractAPI.jar $app ${PLATFORMSPATH} $curr/iccta/res/api_mappings.txt iccta/res/SourcesAndSinks_tmp.txt

# create a new sourcesandsinks file
$curr/iccta/res/merge_source_sink.sh

cd $curr	

echo "running ic3 on $app"
# detect icc links in the app using ic3
timeout 10m $curr/iccProvider/ic3/runIC3.sh $app
	
# were we able to run/timed-out the analysis? if not report and exit
if [ $? -ne 0 ]
then	
	cd $curr
	# log unsuccessful analysis
	echo $(basename $app) >> $curr/output/failed_on_ic3.txt
fi

# run iccta anyways. we will miss ICC but intra-compoent should be possible
cd $curr/iccta

echo "running iccta on $app"

# run taint analysis on the app
timeout 30m /usr/bin/java -Xmx5g -Xss2g -jar $curr/iccta/iccta.jar  $app ${PLATFORMSPATH} $curr/output/$(basename $app).txt

# failed or timed-out
if [ $? -ne 0 ]
then
	cd $curr	
	echo $(basename $app) >> $curr/output/failed_on_iccta.txt
fi

./permpair.sh $curr/output/$(basename $app).txt

