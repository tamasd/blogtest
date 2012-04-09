#!/bin/sh
cd deps

rm -rf jquery bootstrap &>/dev/null

# jquery
mkdir -p jquery
curl -s -o jquery/jquery.min.js http://code.jquery.com/jquery-1.7.1.min.js

# bootstrap
curl -s -O http://twitter.github.com/bootstrap/assets/bootstrap.zip
unzip -qq bootstrap.zip
rm bootstrap.zip
