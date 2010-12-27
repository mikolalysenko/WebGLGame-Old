#!/bin/sh
mkdir logs
google-chrome 127.0.0.1:8080/test_ajax.html &
yaws -i --conf yaws.conf  -mnesia dir db
