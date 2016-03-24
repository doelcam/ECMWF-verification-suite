#!/bin/bash
#SBATCH --workdir=$SCRATCH
#SBATCH --output=copydata.out
#SBATCH --qos=normal
#SBATCH --job-name=CopyECFSdata
#SBATCH --mail-type=ALL
#SBATCH --export=ALL


# Copying data produced by the verification suite from ECFS to a remote directory

# Path definitions
remoteDirectory="macleod@gateway.atm.ox.ac.uk:/home/chaos/pred/macleod"

ecmwfUserID="ukdm"
rootStoreDirectory="${SCRATCH}/CopyToRemote/"
if [ ! -d $rootStoreDirectory ]; then mkdir $rootStoreDirectory ;fi
runName="exampleVerificationRun"
rm -rf $rootStoreDirectory$runName
mkdir $rootStoreDirectory$runName



hindcastPeriod="1981-1984"

echo "Copying only hindcast period "$hindcastPeriod

for exID in "0001" ; do # modify this for multiple experiments

 type="uk" # presuming you are a UK user - change if not! 

 if [ "$exID" = "0001" ]; then # running with the operational hindcast means a diff type
  type="od"
 fi
 
 rootReadDirectory="ec:/"$ecmwfUserID"/verify/data/ecmf/"$type"/"$exID"/s004/m001/"
 mkdir $rootStoreDirectory$runName"/"$exID

 for directory in "" "clim" "anom" "acc" "rel" ; do 
  readdir=$rootReadDirectory$directory"/*"
  copydir=$rootStoreDirectory$runName"/"$exID$"/"$directory
  mkdir $copydir
  ecp $readdir'*'$hindcastPeriod'*' $copydir
  echo "Copied from "$readdir" to "$copydir
 done

done

echo "Finished copying from ecfs to ecgate (scratch). Now to remote directory...if password is necessary then enter it now."
scp -r $rootStoreDirectory"/"$runName $remoteDirectory
