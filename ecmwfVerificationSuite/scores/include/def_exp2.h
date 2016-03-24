echo "-----------------------------------"
echo "This is the beginning of def_exp2.h"
echo "-----------------------------------"
#
# This file contains basic lists for the experiment
#
typeset -Z2 CROS
#
# Start dates
#
set -A ALL_MON $MM1
set -A ALL_DAY $DD1
set -A ALL_TIM $TT1
NUMSEA=0
MM_LIST=""
DD_LIST=""
TT_LIST=""
MMlist=""
DDlist=""
TTlist=""
for MM in ${ALL_MON[*]} ; do
    MM_LIST="${MM_LIST} ${MM}"
    DD_LIST="${DD_LIST} ${ALL_DAY[$NUMSEA]}"
    TT_LIST="${TT_LIST} ${ALL_TIM[$NUMSEA]}"
    MMlist="${MMlist},${MM}"
    DDlist="${DDlist},${ALL_DAY[$NUMSEA]}"
    TTlist="${TTlist},${ALL_TIM[$NUMSEA]}"
    NUMSEA=$((NUMSEA+1))
done
echo "Number of start dates per year "$NUMSEA
MM_LIST=${MM_LIST# }
DD_LIST=${DD_LIST# }
TT_LIST=${TT_LIST# }
MMlist=${MMlist#,}
DDlist=${DDlist#,}
TTlist=${TTlist#,}
echo $MM_LIST
set -A MM_LIST $MM_LIST
echo $DD_LIST
set -A DD_LIST $DD_LIST
echo $TT_LIST
set -A TT_LIST $TT_LIST
echo $MMlist
echo $DDlist
echo $TTlist
#
# Diagnostic dates
#
set -A ALL_IAFCM $SDIA
set -A ALL_IEFCM $EDIA
SDlist=""
EDlist=""
NLDT=0
for FC in ${ALL_IAFCM[*]} ; do
    SDlist="${SDlist},${FC}"
    NLDT=$((NLDT+1))
done
TOT_ALL_DIA=$((NLDT-1))
for FC in ${ALL_IEFCM[*]} ; do
    EDlist="${EDlist},${FC}"
done
SDlist=${SDlist#,}
EDlist=${EDlist#,}
#
# Regions of interest
#
set -A ALL_REG $RDIA
#REG_LST=""
#n=0
#while [[ $n -lt $TOT_REG ]] ; do
#      REG_LST="${REG_LST} 0"
#      n=$((n+1))
#done
#set -A REG_LST $REG_LST
#TOT_ALL_REG=0
#for REG in ${ALL_REG[*]} ; do
#    TOT_ALL_REG=$((TOT_ALL_REG+1))
#    n=0
#    while [[ $n -lt $TOT_REG ]] ; do
#          if [[ ${REG_NAM[$n]} == $REG ]] ; then
#             REG_LST[$n]=1
#          fi
#          n=$((n+1))
#    done
#done
#rcal=""
#n=0
#while [[ $n -lt $TOT_REG ]] ; do
#      rcal="${rcal} ${REG_LST[$n]},"
#      n=$((n+1))
#done
#cat >> reglist_std << EOF
#rcal=${rcal}
# /
#EOF
#cat >> reglist_139 << EOF
#rcal=${rcal}
# /
#EOF
#
# Time series to diagnose
#
set -A ALL_TS $TSDIA
TS_LST=""
n=0
while [[ $n -lt $TOT_TS ]] ; do
      TS_LST="${TS_LST} 0"
      n=$((n+1))
done
set -A TS_LST $TS_LST
TOT_ALL_TS=0
for TS in ${ALL_TS[*]} ; do
    TOT_ALL_TS=$((TOT_ALL_TS+1))
    n=0
    while [[ $n -lt $TOT_TS ]] ; do
          if [[ ${TS_NAM[$n]} == $TS ]] ; then
             TS_LST[$n]=1
          fi
          n=$((n+1))
    done
done
tscal=""
n=0
while [[ $n -lt $TOT_TS ]] ; do
      tscal="${tscal} ${TS_LST[$n]},"
      n=$((n+1))
done
cat >> tslist << EOF
tscal=${tscal}
 /
EOF
#
echo "-----------------------------"
echo "This is the end of def_exp2.h"
echo "-----------------------------"
