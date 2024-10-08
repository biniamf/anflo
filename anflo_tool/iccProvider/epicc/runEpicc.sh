#!/bin/bash

HOST=127.0.0.1
DB=cc
USERNAME=root
PASSWORD=mysql

ANDROID_JARS=/Users/biniam/Library/Android/sdk/platforms
APP=$1

rm -rf testspace/*

java -Xmx8192m -jar Epicc.jar $ANDROID_JARS $APP $HOST $DB $USERNAME $PASSWORD

rm -rf testspace/*
