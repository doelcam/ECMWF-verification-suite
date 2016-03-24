echo "-----------------------------------"
echo "This is the beginning of def_exp3.h"
echo "-----------------------------------"
#
# Lists and variables to be able to take into account the multi-model
#
set -A EXP_LIST $EXPVER
set -A TYP_LIST $TYPFOR
set -A CLA_LIST $CLASS
set -A STR_LIST $STREAM
set -A MET_LIST $METHOD
set -A SYS_LIST $SYSTEM
set -A ORI_LIST $ORIGIN
set -A NEN_LIST $NENS
NUMMOD=0
in=0
for EXP in ${EXP_LIST[*]} ; do
    NUMMOD=$((NUMMOD+1))
    in=$((in+1))
done # EXP loop
if [[ $MULTI -eq 1 ]] ; then
   NUMMOD=$((NUMMOD-1))
fi
echo "Total number of experiments ${NUMMOD}"
#
in=0
NEN=0
EXPlist=""
NENSlist=""
while [[ $in -lt $NUMMOD ]] ; do
      NEN=$((NEN+NEN_LIST[$in]))
      EXPlist="${EXPlist},'${EXP_LIST[$in]}'"
      NENSlist="${NENSlist},${NEN_LIST[$in]}"
      in=$((in+1))
done
while [[ $in -lt 10 ]] ; do
      EXPlist="${EXPlist},'xxxx'"
      NENSlist="${NENSlist},999"
      in=$((in+1))
done
EXPlist=${EXPlist#,}
NENSlist=${NENSlist#,}
echo "List of experiments ${EXPlist}"
echo "Ensemble sizes of experiments ${NENSlist}"
echo "Total ensemble size ${NEN}"
#
if [[ $MULTI -eq 1 ]] ; then
   export EXPID=${EXP_LIST[$NUMMOD]}
   export NENS=$NEN
   EXPID_LONG=E${EXPID}
   ECDISK_LONG="${ECDISK}/data/${EXPID}"
   emkdir -p ec:/$ECDISK_LONG || echo "ECFS directory already exists"
fi
#
# Multi-model description
#
if [[ $MULTI -eq 1 ]] ; then
   \rm -f ${EXPID}_description
   touch ${EXPID}_description
   ir=0
   while [[ $ir -le $((NUMMOD-1)) ]] ; do
         echo C${CLA_LIST[$ir]}O${ORI_LIST[$ir]}E${EXP_LIST[$ir]}S${SYS_LIST[$ir]}M${MET_LIST[$ir]}N${NEN_LIST[$ir]} >> ${EXPID}_description
         ir=$((ir+1))
   done
fi
#
echo "-----------------------------"
echo "This is the end of def_exp3.h"
echo "-----------------------------"
