echo "-----------------------------------"
echo "This is the beginning of def_exp1.h"
echo "-----------------------------------"
#
# This file contains basic names and directories of the experiment
#
typeset -Z3 SYSIDdef METIDdef
#
SYSIDdef=$SYSID
METIDdef=$METID
ORIGIN=$ORIID
SYSTEM=$SYSID
if [[ $CLAID = rd || $CLAID = dm ]] ; then
   SYSTEM=off
   ORIGIN=off
fi
if [[ $CLAID = od && $STREAM = sfmm ]] ; then
   ORIGIN=off
fi
if [[ $CLAID = rd ]] ; then
   ORIIDdef=ecmf
else
   ORIIDdef=$ORIID
fi
#

if [[ $TYPID -eq 0 ]] ; then
   EXPID_LONG=C${CLAID}O${ORIIDdef}E${EXPID}S${SYSIDdef}M${METIDdef}
   ECDISK_LONG="${ECDISK}/data/${ORIIDdef}/${CLAID}/${EXPID}/s${SYSIDdef}/m${METIDdef}"
fi
if [[ $TYPID -eq 1 ]] ; then

   EXPID_LONG=E${EXPID}
   ECDISK_LONG="${ECDISK}/data/${EXPID}"
fi
emkdir -p ec:$ECDISK_LONG || echo "ECFS directory already exists"
#
echo "-----------------------------"
echo "This is the end of def_exp1.h"
echo "-----------------------------"
