#! /bin/sh

appPath=$1
forceAndroidJar=/Users/biniam/Library/Android/sdk/platforms

rm -rf testspace
mkdir testspace

appName=`basename $appPath .apk`
retargetedPath=testspace/$appName.apk/retargeted/retargeted/$appName

rm -rf output/ic3/$appName.txt

/usr/bin/java -Xmx8192m -jar RetargetedApp.jar $forceAndroidJar $appPath $retargetedPath
/usr/bin/java -Xmx8192m -jar ic3-0.1.0-full.jar -apkormanifest $appPath -input $retargetedPath -cp $forceAndroidJar -db cc.properties

rm -rf testspace
rm -rf sootOutput
