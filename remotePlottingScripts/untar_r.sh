# This is a simple function to help with unpacking the verification suite data. 
# Starting from a root directory it descends into each of the directories listed
# in the loop below, unpacking all the .tar files it finds within.

# Modify these directories as needed based on what you calculate with the suite

# D MacLeod, University of Oxford 24 March 2016

#! /bin/bash

set -xv

for directory in "acc" "anom" "clim" "rel"
do
cd $directory

for file in *tar
do 
	tar xvf $file
done 

cd ..

done
