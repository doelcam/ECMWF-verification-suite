echo "---------------------------------"
echo "This is the beginning of config.h"
echo "---------------------------------"
#
typeset -Z2 MM DD TT
typeset -Z3 PAR LEV
typeset -Z3 IAFCM IEFCM
typeset -Z2 IAFCY IEFCY
#
# Compilation options
#
PATHSO="/home/rd/nep/sms/verify/scores/bin" # Source of the fortran files
# For rs6000 (with xlf90)
COMPOP1="-qfixed -qsigtrap -qarch=pwr3"
COMPOP2="-g -C -qextchk -O3 -qflttrap=overflow:zerodivide:invalid:enable"
COMPOP3="-g -C -qextchk"
COMPOP4="-O3"
# For linux machines (with pgf90)
COMPOP1="-O3"
COMPOP2="-pg -g -C -Mbounds -Ktrap=fp" # For debugging
COMPOP2="-g -C -Mbounds -Ktrap=fp" # For debugging
# Libraries
LINKLI1="$EMOSLIB"
LINKLI2="$MAGLIB"
LINKLI3="$NAGLIB"
#
# The files are supposed to be interpolated into 2.5 degree grid from
# 87.5 North to 87.5 South
#
NX=144
NY=71
north=87.5
east=0.
south=-87.5
west=357.5
#
# Special dates
#
#YY1_228=1959
#YY1_228=1979
#
# Definitions for variables
# TOT_ALL_PAR: Total number of raw variables (the count starts from zero)
# TOT_ANO_PAR: Total number of variables used to compute anomalies
# TOT_REG_PAR: Total number of variables used to compute regional scores
# ALL_PAR: GRIB code of the raw variables
# ALL_LEV: Level
# ALL_LTY: Level type
# ALL_TYP: Type of the variable for the reference
# ANO_PAR: GRIB code of the variables for which anomalies are computed
# ANO_LEV: Level
# The last four sets are defined using 0/1 tags from the ALL_* sets
#
set -A ALL_PAR_LST 039 040 041 042 151 139 167 168 228 164 165 166 169 175 176 177 178 179 201 202 129 129 129 129 130 130 130 130 131 131 131 131 132 132 132 133 133 133 155
set -A ALL_LEV_LST 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 850 500 200 050 850 500 200 050 850 500 200 050 850 500 200 850 500 200 200
set -A ALL_LTY_LST sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc sfc  pl  pl  pl  pl  pl  pl  pl  pl  pl  pl  pl  pl  pl  pl  pl  pl  pl  pl  pl
set -A ALL_TYP_LST  an  an  an  an  an  an  an  an  fc  an  an  an  fc  fc  fc  fc  fc  fc  an  an  an  an  an  an  an  an  an  an  an  an  an  an  an  an  an  an  an  an  an
#
TOT_ALL_PAR_LST=0
for PAR in ${ALL_PAR_LST[*]} ; do
    TOT_ALL_PAR_LST=$((TOT_ALL_PAR_LST+1))
done
TOT_ALL_PAR_LST=$((TOT_ALL_PAR_LST-1))
#
set -A ANO_PAR $PLST
set -A ANO_LEV $LLST
set -A ANO_TAB $TLST
set -A ANO_FAB $FLST
set -A ANO_FRQ $MLST
TOT_ANO_PAR=0
for PAR in ${ANO_PAR[*]} ; do
    TOT_ANO_PAR=$((TOT_ANO_PAR+1))
done
TOT_ANO_PAR=$((TOT_ANO_PAR-1))
#
TOT_ALL_PAR=0
m=0
ALL_PAR=""
ALL_LEV=""
ALL_LTY=""
ALL_TYP=""
ALL_TAB=""
ALL_FAB=""
ALL_FRQ=""
set +xv
while [[ $m -le $TOT_ANO_PAR ]] ; do
      n=0
      while [[ $n -le $TOT_ALL_PAR_LST ]] ; do
            if [[ ${ANO_PAR[${m}]} -eq ${ALL_PAR_LST[${n}]} ]] ; then
               if [[ ${ANO_LEV[${m}]} -eq ${ALL_LEV_LST[${n}]} ]] ; then
#                 ALL_PAR[${m}]=${ALL_PAR_LST[${n}]}
#                 ALL_LEV[${m}]=${ALL_LEV_LST[${n}]}
#                 ALL_LTY[${m}]=${ALL_LTY_LST[${n}]}
#                 ALL_TYP[${m}]=${ALL_TYP_LST[${n}]}
                  ALL_PAR="${ALL_PAR} ${ALL_PAR_LST[${n}]}"
                  ALL_LEV="${ALL_LEV} ${ALL_LEV_LST[${n}]}"
                  ALL_LTY="${ALL_LTY} ${ALL_LTY_LST[${n}]}"
                  ALL_TYP="${ALL_TYP} ${ALL_TYP_LST[${n}]}"
                  ALL_TAB="${ALL_TAB} ${ANO_TAB[${m}]}"
                  ALL_FAB="${ALL_FAB} ${ANO_FAB[${m}]}"
                  ALL_FRQ="${ALL_FRQ} ${ANO_FRQ[${m}]}"
                  TOT_ALL_PAR=$((TOT_ALL_PAR+1))
               fi
            fi
            n=$((n+1))
      done
      m=$((m+1))
done
set -xv
TOT_ALL_PAR=$((TOT_ALL_PAR-1))
set -A ALL_PAR $ALL_PAR
set -A ALL_LEV $ALL_LEV
set -A ALL_LTY $ALL_LTY
set -A ALL_TYP $ALL_TYP
set -A ALL_TAB $ALL_TAB
set -A ALL_FAB $ALL_FAB
set -A ALL_FRQ $ALL_FRQ
#
# Regions
#
# The total number of regions should match the value nreg_max in the fortran programmes
# namr: name of the regions
# limn: limits/boundaries of the regions (North)
# lims: limits/boundaries of the regions (South)
# limw: limits/boundaries of the regions (West)
# lime: limits/boundaries of the regions (East)
#       The regions available are:
#        1. Global                             (GLOB)  87.5,-87.5,   0.0, 360.0   0
#        2. Northern Extratropics              (NHEX)  87.5, 30.0,   0.0, 360.0   0
#        3. Tropics                            (TROP)  20.0,-20.0,   0.0, 360.0   0
#        4. Southern Hemisphere                (SHEX) -30.0,-87.5,   0.0, 360.0   0
#        5. Global land                        (GLOL)  87.5,-87.5,   0.0, 360.0   1
#        6. Northern Extratropics land         (NHEL)  87.5, 30.0,   0.0, 360.0   1
#        7. Tropics land                       (TROL)  20.0,-20.0,   0.0, 360.0   1
#        8. Southern Hemisphere land           (SHEL) -30.0,-87.5,   0.0, 360.0   1
#        9. Global ocean                       (GLOO)  87.5,-87.5,   0.0, 360.0  -1
#       10. Northern Extratropics ocean        (NHEO)  87.5, 30.0,   0.0, 360.0  -1
#       11. Tropics ocean                      (TROO)  20.0,-20.0,   0.0, 360.0  -1
#       12. Southern Hemisphere ocean          (SHEO) -30.0,-87.5,   0.0, 360.0  -1
#       13. Europe                             (EURO)  75.0, 35.0, -12.5,  42.5   0
#       14. North America                      (NAME)  70.0, 30.0,-130.0, -60.0   0
#       15. Indian Ocean 1                     (IND1)  10.0,-10.0,  50.0,  70.0  -1
#       16. Indian Ocean 2                     (IND2)   0.0,-10.0,  90.0, 110.0  -1
#       17. Tropical Indian                    (TRIN)  20.0,-20.0,  50.0, 120.0  -1
#       18. Tropical Atlantic                  (TRAT)  30.0,-20.0, -80.0,  20.0  -1
#       19. Nino3 SST                          (NI03)   5.0, -5.0,-150.0, -90.0  -1
#       20. Nino4 SST                          (NI04)   5.0, -5.0, 160.0,-150.0  -1
#       21. Nino1+2 SST                        (NI12)   0.0,-10.0, -90.0, -80.0  -1
#       22. Nino3.4 SST                        (NI34)   5.0, -5.0,-170.0,-120.0  -1  
#   Giorgi and Francisco regions (land only)
#       23. Australia                          (AUSG) -11.0,-45.0, 110.0, 155.0   1
#       24. Amazon                             (AMZG)  12.0,-20.0, -82.5, -35.0   1
#       25. Southern South America             (SSAG) -20.0,-55.0, -75.0, -40.0   1
#       26. Central America                    (CAMG)  30.0, 10.0,-115.0, -82.5   1
#       27. Western North America              (WNAG)  60.0, 30.0,-130.0,-102.5   1
#       28. Central North America              (CNAG)  50.0, 30.0,-102.5, -85.0   1
#       29. Eastern North America              (ENAG)  50.0, 25.0, -85.0, -60.0   1
#       30. Alaska                             (ALAG)  72.5, 60.0,-170.0,-102.5   1
#       31. Greenland                          (GRLG)  85.0, 50.0,-102.5, -10.0   1
#       32. Mediterranean basin                (MEDG)  47.5, 30.0, -10.0,  40.0   1
#       33. Northern Europe                    (NEUG)  75.0, 47.5, -10.0,  40.0   1
#       34. Western Africa                     (WAFG)  17.5,-12.5, -20.0,  22.5   1
#       35. Eastern Africa                     (EAFG)  17.5,-12.5,  22.5,  52.5   1
#       36. Southern Africa                    (SAFG) -12.5,-35.0, -10.0,  52.5   1
#       37. Sahara                             (SAHG)  30.0, 17.5, -20.0,  65.0   1
#       38. Southeast Asia                     (SEAG)  20.0,-10.0,  95.0, 155.0   1
#       39. East Asia                          (EASG)  50.0, 20.0, 100.0, 145.0   1
#       40. South Asia                         (SASG)  30.0,  5.0,  65.0, 100.0   1
#       41. Central Asia                       (CASG)  50.0, 30.0,  40.0,  75.0   1
#       42. Tibet                              (TIBG)  50.0, 30.0,  75.0, 100.0   1
#       43. North Asia                         (NASG)  70.0, 50.0,  40.0, 180.0   1
#  some more
#       44. Africa (land only)                 (AFRL)  35.0,-35.0, -20.0,  52.5   1
#       45. Europe (land only)                 (EURL)  75.0, 30.0, -10.0,  40.0   1
#       46. North Atlantic (sea points only)   (NATL)  87.5, 30.0, -80.0, -10.0  -1
#       47. North Pacific  (sea points only)   (NPAC)  65.0, 30.0, 160.0,-140.0  -1
#
# ilsm: use of land sea mask
#       1 = land points
#       0 = no land-sea mask applied
#      -1 = sea points
#
TOT_REG=48 # Total number of regions
set -A REG_NAM GLOB NHEX TROP SHEX GLOL NHEL TROL SHEL GLOO NHEO TROO SHEO EURO NAME IND1 IND2 TRIN TRAT NI03 NI04 NI12 NI34 AUSG AMZG SSAG CAMG WNAG CNAG ENAG ALAG GRLG MEDG NEUG WAFG EAFG SAFG SAHG SEAG EASG SASG CASG TIBG NASG AFRL EURL NATL NPAC NONE
namr=""
n=0
while [[ $n -lt $TOT_REG ]] ; do
      namr="${namr}'${REG_NAM[$n]}',"
      n=$((n+1))
done
cat > reglist_std << EOF
 &region
  namr=${namr}
  limn=  87.5,  87.5,  20.0, -30.0,  87.5,  87.5,  20.0, -30.0,  87.5,  87.5,  20.0, -30.0,  75.0,  70.0,  10.0,   0.0,  20.0,  30.0,   5.0,   5.0,   0.0,   5.0, -11.0,  12.0, -20.0,  30.0,  60.0,  50.0,  50.0,  72.5,  85.0,  47.5,  75.0,  17.5,  17.5, -12.5,  30.0,  20.0,  50.0,  30.0,  50.0,  50.0,  70.0,  35.0,  75.0,  87.5,  65.0, 999.9,
  lims= -87.5,  30.0, -20.0, -87.5, -87.5,  30.0, -20.0, -87.5, -87.5,  30.0, -20.0, -87.5,  35.0,  30.0, -10.0, -10.0, -20.0, -20.0,  -5.0,  -5.0, -10.0,  -5.0, -45.0, -20.0, -55.0,  10.0,  30.0,  30.0,  25.0,  60.0,  50.0,  30.0,  47.5, -12.5, -12.5, -35.0,  17.5, -10.0,  20.0,   5.0,  30.0,  30.0,  50.0, -35.0,  30.0,  30.0,  30.0, 999.9,
  limw=   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0, -12.5,-130.0,  50.0,  90.0,  50.0, -80.0,-150.0, 160.0, -90.0,-170.0, 110.0, -82.5, -75.0,-115.0,-130.0,-102.5, -85.0,-170.0,-102.5, -10.0, -10.0, -20.0,  22.5, -10.0, -20.0,  95.0, 100.0,  65.0,  40.0,  75.0,  40.0, -20.0, -10.0, -80.0, 160.0, 999.9,
  lime= 360.0, 360.0, 360.0, 360.0, 360.0, 360.0, 360.0, 360.0, 360.0, 360.0, 360.0, 360.0,  42.5, -60.0,  70.0, 110.0, 120.0,  20.0, -90.0,-150.0, -80.0,-120.0, 155.0, -35.0, -40.0, -82.5,-102.5, -85.0, -60.0,-102.5, -10.0,  40.0,  40.0,  22.5,  52.5,  52.5,  65.0, 155.0, 145.0, 100.0,  75.0, 100.0, 180.0,  52.5,  40.0, -10.0,-140.0, 999.9,
  ilsm=     0,     0,     0,     0,     1,     1,     1,     1,    -1,    -1,    -1,    -1,     0,     0,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,   1,   1,  -1,  -1,   999,
EOF
#
cat > reglist_139 << EOF
 &region
  namr=${namr}
  limn=  87.5,  87.5,  20.0, -30.0,  87.5,  87.5,  20.0, -30.0,  87.5,  87.5,  20.0, -30.0,  75.0,  70.0,  10.0,   0.0,  20.0,  30.0,   5.0,   5.0,   0.0,   5.0, -11.0,  12.0, -20.0,  30.0,  60.0,  50.0,  50.0,  72.5,  85.0,  47.5,  75.0,  17.5,  17.5, -12.5,  30.0,  20.0,  50.0,  30.0,  50.0,  50.0,  70.0,  35.0,  75.0,  87.5,  65.0, 999.9,
  lims= -87.5,  30.0, -20.0, -87.5, -87.5,  30.0, -20.0, -87.5, -87.5,  30.0, -20.0, -87.5,  35.0,  30.0, -10.0, -10.0, -20.0, -20.0,  -5.0,  -5.0, -10.0,  -5.0, -45.0, -20.0, -55.0,  10.0,  30.0,  30.0,  25.0,  60.0,  50.0,  30.0,  47.5, -12.5, -12.5, -35.0,  17.5, -10.0,  20.0,   5.0,  30.0,  30.0,  50.0, -35.0,  30.0,  30.0,  30.0,  999.9,
  limw=   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0, -12.5,-130.0,  50.0,  90.0,  50.0, -80.0,-150.0, 160.0, -90.0,-170.0, 110.0, -82.5, -75.0,-115.0,-130.0,-102.5, -85.0,-170.0,-102.5, -10.0, -10.0, -20.0,  22.5, -10.0, -20.0,  95.0, 100.0,  65.0,  40.0,  75.0,  40.0,  -20.0, -10.0, -80.0, 160.0, 999.9,
  lime= 360.0, 360.0, 360.0, 360.0, 360.0, 360.0, 360.0, 360.0, 360.0, 360.0, 360.0, 360.0,  42.5, -60.0,  70.0, 110.0, 120.0,  20.0, -90.0,-150.0, -80.0,-120.0, 155.0, -35.0, -40.0, -82.5,-102.5, -85.0, -60.0,-102.5, -10.0,  40.0,  40.0,  22.5,  52.5,  52.5,  65.0, 155.0, 145.0, 100.0,  75.0, 100.0, 180.0,  52.5,  40.0, -10.0,-140.0,  999.9,
  ilsm=     0,     0,     0,     0,     1,     1,     1,     1,    -1,    -1,    -1,    -1,     0,     0,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,   1,   1,  -1,  -1,   999,
EOF
#
# Time series
#
# Regional averages and simple indices
#
# nind:  number of indices defined
# ipar:  variable to be analysed
# ilei:  initial level
# ilel:  end level
# ilsm:  use of land sea mask
#        1 = land points
#        0 = no land-sea mask applied
#       -1 = sea points
# icdf:  flag for shape of region
#        0 = rectangular area
#       >0 = non rectangular area (# of river catchment area)
# xdif:  compute differences or sums between levels or two different regions
#       -1.0 = computes differences
#        0.5 = does averages
#        0.0 = takes the value of the last computed level
# stnd:  standardization of the variables before computing the index (as in SOI)
#        1 = performs standardization
#        0 = does not
# namr:  name of the regions
# limn:  limits/boundaries of the regions (North)
# lims:  limits/boundaries of the regions (South)
# limw:  limits/boundaries of the regions (West)
# lime:  limits/boundaries of the regions (East)
#        As the regional area for level 1 and level 2 might be different
#        (or to allow defining different areas for the same level) the
#        region has to be defined for each level separately, with i standing
#        for first level, l for the second one
#
#       The regions available are:
#        1. U-shear index (USHEAR)
#        2. V-shear indexa(VSHEAR)
#        3. All Indian rainfall (RRAIND)
#        4. Central tropical Pacific precipitation (RRCTPA)
#        5. Indonesian precipitation (RRINDO)
#        6. Sahel precipitation (RRSAHE)
#        7. Guinea coast precipitation (RRGUIC)
#        8. East Africa precipitation (RREAFR)
#        9. South Africa precipitation (RRSAFR)
#       10. Sahel temperature (T2SAHE)
#       11. East Africa 2m-temperature (T2EAFR)
#       12. South Africa 2m-temperature (T2SAFR)
#       13. North America precipitation (RRNAME)
#       14. North America 2m-temperature (T2NAME)
#       15. Europe precipitation (RREURO)
#       16. Northern Europe precipitation (RRNEUR)
#       17. Southern Europe precipitation (RRSEUR)
#       18. Europe 2m-temperature (T2EURO)
#       19. Northern Europe 2m-temperature (T2NEUR)
#       20. Southern Europe 2m-temperature (T2SEUR)
#       21. Ukraine 2m-temperature (T2UKRA)
#       22. Central North America precipitation (RRCENA)
#       23. North West Pacific coast precipitation (RRNWNA)
#       24. North East North America precipitation (RRNENA)
#       25. Gulf coast of North America precipitation (RRGCNA)
#       26. Central North America 2m-temperature (T2CENA)
#       27. North West Pacific coast 2m-temperature (T2NWNA)
#       28. North East North America 2m-temperature (T2NENA)
#       29. Gulf coast of North America 2m-temperature (T2GCNA)
#       30. Florida 2m-temperature (T2FLOR)
#       31. Caribbean/Amazon precipitation (RRCAAM)
#       32. NE Brazil precipitation (RRNEBR)
#       33. South American Atlantic coast precipitation (RRATSA)
#       34. South American Pacific coast precipitation (RRPASA)
#       35. Caribian/Amazon 2m-temperature (T2CAAM)
#       36. NE Brazil 2m-temperature (T2NEBR)
#       37. South American Atlantic coast 2m-temperature (T2ATSA)
#       38. South American Pacific coast 2m-temperature (T2PASA)
#       39. Central Asia precipitation (RRCASI)
#       40. Japan/Korea precipitation (RRJAKO)
#       41. China precipitation (RRCHIN)
#       42. Middle East precipitation (RRMEAS)
#       43. South East Asia precipitation (RRSEAS)
#       44. Philippines precipitation (RRPHIL)
#       45. Central Asia 2m-temperature (T2CASI)
#       46. Japan/Korea 2m-temperature (T2JAKO)
#       47. China 2m-temperature (T2CHIN)
#       48. Middle East 2m-temperature (T2MEAS)
#       49. Australia precipitation (RRAUST)
#       50. North East Australia precipitation (RRNEAU)
#       51. South East Australia precipitation (RRSEAU)
#       52. Australia 2m-temperature (T2AUST)
#       53. North East Australia 2m-temperature (T2NEAU)
#       54. South East Australia 2m-temperature (T2SEAU)
#       55. Nino3 SST (STNIN3)
#       56. Nino4 SST (STNIN4)
#       57. Nino1+2 SST (STNI12)
#       58. Nino3.4 SST (STNI34)
#       59. Tropical Atlantic SST (STTATL)
#       60. Indian ocean dipole of SST (STIODI)
#       61. Atlantic3 SST (STATL3)
#       62. Tropical Atlantic meridional mode SST (STTAMM)
#       63. SOI (East Pacific - West Pacific) based on SLP (SLPSOI)
#       64. North Atlantic SST (STNATL)
#       65. Northern Hemisphere T2m (land points) (T2NHLP)
#       67. Atlantic multi-decadal oscillation index (STAMOI)
#       68. Amazon catchment area (CAAMAZ)
#       69. Nile catchment area (CANILE)
#       70. Zambezi catchment area (CAZAMB)
#       71. Jiang catchment area (CAJIAN)
#       72. Ganges catchment area (CAGANG)
#       73. Global T2m (T2GAVE)
#       74. Global T2m (land points) (T2GALP)
#       75. Global T2m (ocean points) (T2GAOP)
#
TOT_TS=75 # Total number of time series
set -A TS_NAM USHEAR VSHEAR RRAIND RRCTPA RRINDO RRSAHE RRGUIC RREAFR RRSAFR T2SAHE T2EAFR T2SAFR \
              RRNAME T2NAME RREURO RRNEUR RRSEUR T2EURO T2NEUR T2SEUR T2UKRA \
              RRCENA RRNWNA RRNENA RRGCNA T2CENA T2NWNA T2NENA T2GCNA T2FLOR \
              RRCAAM RRNEBR RRATSA RRPASA T2CAAM T2NEBR T2ATSA T2PASA \
              RRCASI RRJAKO RRCHIN RRMEAS RRSEAS RRPHIL T2CASI T2JAKO T2CHIN T2MEAS \
              RRAUST RRNEAU RRSEAU T2AUST T2NEAU T2SEAU \
              STNIN3 STNIN4 STNI12 STNI34 STTATL STIODI STATL3 STTAMM SLPSOI \
              STNATL T2NHLP STAMOI \
              CAAMAZ CANILE CAZAMB CAJIAN CAGANG \
              T2GAVE T2GALP T2GAOP   NONE
set -A TS_PAR    131    132    228    228    228    228    228    228    228    167    167    167 \
                 228    167    228    228    228    167    167    167    167 \
                 228    228    228    228    167    167    167    167    167 \
                 228    228    228    228    167    167    167    167 \
                 228    228    228    228    228    228    167    167    167    167 \
                 228    228    228    167    167    167 \
                 139    139    139    139    139    139    139    139    151 \
                 139    167    139 \
                 228    228    228    228    228 \
                 167    167    167    999
set -A TS_LVI    200    200      0      0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      0 \
                   0      0      0 \
                   0      0      0      0      0 \
                   0      0      0    999
set -A TS_LVL    850    850      0      0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0  \
                   0      0      0      0      0      0      0      0      0      0  \
                   0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      0 \
                   0      0      0 \
                   0      0      0      0      0 \
                   0      0      0    999
set -A TS_LSM      0      0      1      0      0      1      1      1      1      1      1      1 \
                   1      1      1      1      1      1      1      1      1 \
                   1      1      1      1      1      1      1      1      1 \
                   1      1      1      1      1      1      1      1 \
                   1      1      1      1      1      1      1      1      1      1 \
                   1      1      1      1      1      1 \
                  -1     -1     -1     -1     -1     -1     -1     -1      0 \
                  -1      1     -1 \
                   1      1      1      1      1 \
                   0      1     -1    999
set -A TS_CDF      0      0      0      0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      0 \
                   0      0      0 \
                   1      2     11     13     15 \
                   0      0      0    999
      TS_DIF="   -1.    -1.     0.     0.     0.     0.     0.     0.     0.     0.     0.     0. \
                  0.     0.     0.     0.     0.     0.     0.     0.     0. \
                  0.     0.     0.     0.     0.     0.     0.     0.     0. \
                  0.     0.     0.     0.     0.     0.     0.     0. \
                  0.     0.     0.     0.     0.     0.     0.     0.     0.     0. \
                  0.     0.     0.     0.     0.     0. \
                  0.     0.     0.     0.     0.    -1.     0.    -1.    -1. \
                  0.     0.    -1. \
                  0.     0.     0.     0.     0. \
                  0.     0.     0.  999.9"
set -A TS_STD      0      0      0      0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      0      0 \
                   0      0      0      0      0      0 \
                   0      0      0      0      0      0      0      0      1 \
                   0      0      0 \
                   0      0      0      0      0 \
                   0      0      0    999
      TS_LNI="  20.0   25.0   30.0   10.0    5.0   20.0   10.0    7.5  -15.0   20.0    7.5  -15.0 \
                70.0   70.0   65.0   65.0   50.0   65.0   65.0   50.0   52.5 \
                60.0   52.5   47.5   35.0   60.0   52.5   47.5   35.0   32.5 \
                10.0    0.0  -25.0  -30.0   10.0    0.0  -25.0  -30.0 \
                65.0   45.0   35.0   35.0   25.0   20.0   65.0   45.0   35.0   35.0 \
               -25.0  -25.0  -12.5  -25.0  -12.5  -25.0 \
                 5.0    5.0    0.0    5.0   30.0   10.0    3.0   25.0    5.0 \
                65.0   75.0   60.0 \
               999.0  999.0  999.0  999.0  999.0 \
                87.5   87.5   87.5  999.9"
      TS_LNL="  20.0   25.0   30.0   10.0    5.0   20.0   10.0    7.5  -15.0   20.0    7.5  -15.0 \
                70.0   70.0   65.0   65.0   50.0   65.0   65.0   50.0   52.5 \
                60.0   52.5   47.5   35.0   60.0   52.5   47.5   35.0   32.5 \
                10.0    0.0  -25.0  -30.0   10.0    0.0  -25.0  -30.0 \
                65.0   45.0   35.0   35.0   25.0   20.0   65.0   45.0   35.0   35.0 \
               -25.0  -25.0  -12.5  -25.0  -12.5  -25.0 \
                 5.0    5.0    0.0    5.0   30.0    0.0    3.0    0.0    5.0 \
                65.0   75.0  -40.0 \
               999.0  999.0  999.0  999.0  999.0 \
                87.5   87.5   87.5  999.9"
      TS_LSI="   0.0  -15.0    5.0  -10.0  -10.0   10.0    0.0  -12.5  -35.0   10.0  -12.5  -35.0 \
                30.0   30.0   35.0   50.0   35.0   35.0   50.0   35.0   45.0 \
                35.0   37.5   37.5   25.0   35.0   37.5   37.5   25.0   25.0 \
               -12.5  -17.5  -45.0  -55.0  -12.5  -17.5  -45.0  -55.0 \
                45.0   30.0   25.0   20.0    5.0    5.0   45.0   30.0   25.0   20.0 \
               -40.0  -40.0  -25.0  -40.0  -25.0  -40.0 \
                -5.0   -5.0  -10.0   -5.0  -20.0  -10.0   -3.0    5.0   -5.0 \
                10.0   10.0   40.0 \
               999.0  999.0  999.0  999.0  999.0 \
               -87.5  -87.5  -87.5  999.9"
      TS_LSL="   0.0  -15.0    5.0  -10.0  -10.0   10.0    0.0  -12.5  -35.0   10.0  -12.5  -35.0 \
                30.0   30.0   35.0   50.0   35.0   35.0   50.0   35.0   45.0 \
                35.0   37.5   37.5   25.0   35.0   37.5   37.5   25.0   25.0 \
               -12.5  -17.5  -45.0  -55.0  -12.5  -17.5  -45.0  -55.0 \
                45.0   30.0   25.0   20.0    5.0    5.0   45.0   30.0   25.0   20.0 \
               -40.0  -40.0  -25.0  -40.0  -25.0  -40.0 \
                -5.0   -5.0  -10.0   -5.0  -20.0  -10.0   -3.0  -20.0   -5.0 \
                10.0   10.0  -60.0 \
               999.0  999.0  999.0  999.0  999.0 \
               -87.5  -87.5  -87.5  999.9"
      TS_LWI="  40.0   40.0   70.0  170.0   95.0  -10.0  -17.5   27.5   15.0  -10.0   27.5   15.0 \
              -130.0 -130.0  -10.0  -10.0  -10.0  -10.0  -10.0  -10.0   22.5 \
              -110.0 -130.0  -75.0 -100.0 -110.0 -130.0  -75.0 -100.0  -82.5 \
               -75.0  -50.0  -65.0  -80.0  -75.0  -50.0  -65.0  -80.0 \
                40.0  125.0   95.0   30.0   95.0  110.0   40.0  125.0   95.0   30.0 \
               142.5  142.5  140.0  140.0  140.0  140.0 \
              -150.0  160.0  -90.0 -170.0  -80.0   50.0  -20.0  -55.0 -130.0 \
               -60.0    0.0  -60.0 \
               999.0  999.0  999.0  999.0  999.0 \
                 0.0    0.0    0.0  999.9"
      TS_LWL="  40.0   40.0   70.0  170.0   95.0  -10.0  -17.5   27.5   15.0  -10.0   27.5   15.0 \
              -130.0 -130.0  -10.0  -10.0  -10.0  -10.0  -10.0  -10.0   22.5 \
              -110.0 -130.0  -75.0 -100.0 -110.0 -130.0  -75.0 -100.0  -82.5 \
               -75.0  -50.0  -65.0  -80.0  -75.0  -50.0  -65.0  -80.0 \
                40.0  125.0   95.0   30.0   95.0  110.0   40.0  125.0   95.0   30.0 \
               142.5  142.5  140.0  140.0  140.0  140.0 \
              -150.0  160.0  -90.0 -170.0  -80.0   90.0  -20.0  -30.0   90.0 \
               -60.0    0.0  -50.0 \
               999.0  999.0  999.0  999.0  999.0 \
                 0.0    0.0    0.0  999.9"
      TS_LEI=" 110.0  140.0   90.0 -150.0  145.0   30.0   25.0   42.5   40.0   30.0   42.5   40.0 \
               -60.0  -60.0   30.0   30.0   30.0   30.0   30.0   30.0   40.0 \
               -75.0 -115.0  -65.0  -80.0  -75.0 -115.0  -65.0  -80.0  -80.0 \
               -50.0  -35.0  -45.0  -65.0  -50.0  -35.0  -45.0  -65.0 \
                90.0  145.0  122.5   65.0  110.0  140.0   90.0  145.0  122.5   65.0 \
               152.5  152.5  155.0  155.0  155.0  155.0 \
               -90.0 -150.0  -80.0 -120.0   20.0   70.0    0.0  -15.0  -80.0 \
               -10.0  357.5  -10.0 \
               999.0  999.0  999.0  999.0  999.0 \
               357.5  357.5  357.5  999.9"
      TS_LEL=" 110.0  140.0   90.0 -150.0  145.0   30.0   25.0   42.5   40.0   30.0   42.5   40.0 \
               -60.0  -60.0   30.0   30.0   30.0   30.0   30.0   30.0   40.0 \
               -75.0 -115.0  -65.0  -80.0  -75.0 -115.0  -65.0  -80.0  -80.0 \
               -50.0  -35.0  -45.0  -65.0  -50.0  -35.0  -45.0  -65.0 \
                90.0  145.0  122.5   65.0  110.0  140.0   90.0  145.0  122.5   65.0 \
               152.5  152.5  155.0  155.0  155.0  155.0 \
               -90.0 -150.0  -80.0 -120.0   20.0  110.0    0.0   10.0  140.0 \
               -10.0  357.5    0.0 \
               999.0  999.0  999.0  999.0  999.0 \
               357.5  357.5  357.5  999.9"
#
set +xv
namts=""
part=""
leit=""
lelt=""
lsmt=""
cdft=""
dift=""
stdt=""
lini=""
linl=""
lisi=""
lisl=""
liwi=""
liwl=""
liei=""
liel=""
n=0
while [[ $n -lt $TOT_TS ]] ; do
      namts="${namts}'${TS_NAM[$n]}',"
      part="${part}${TS_PAR[$n]},"
      leit="${leit}${TS_LVI[$n]},"
      lelt="${lelt}${TS_LVL[$n]},"
      lsmt="${lsmt}${TS_LSM[$n]},"
      cdft="${cdft}${TS_CDF[$n]},"
      stdt="${stdt}${TS_STD[$n]},"
      n=$((n+1))
done
for DIF in $TS_DIF ; do
    dift="${dift}${DIF},"
done
for COOR in $TS_LNI ; do
    lini="${lini}${COOR},"
done
for COOR in $TS_LNL ; do
    linl="${linl}${COOR},"
done
for COOR in $TS_LSI ; do
    lisi="${lisi}${COOR},"
done
for COOR in $TS_LSL ; do
    lisl="${lisl}${COOR},"
done
for COOR in $TS_LWI ; do
    liwi="${liwi}${COOR},"
done
for COOR in $TS_LWL ; do
    liwl="${liwl}${COOR},"
done
for COOR in $TS_LEI ; do
    liei="${liei}${COOR},"
done
for COOR in $TS_LEL ; do
    liel="${liel}${COOR},"
done
n=$((TOT_TS))
while [[ $n -lt 80 ]] ; do
      namts="${namts}'NONE',"
      part="${part}999,"
      leit="${leit}999,"
      lelt="${lelt}999,"
      lsmt="${lsmt}999,"
      cdft="${cdft}999,"
      dift="${dift}999.9,"
      stdt="${stdt}999,"
      lini="${lini}999.9,"
      linl="${linl}999.9,"
      lisi="${lisi}999.9,"
      lisl="${lisl}999.9,"
      liwi="${liwi}999.9,"
      liwl="${liwl}999.9,"
      liei="${liei}999.9,"
      liel="${liel}999.9,"
      n=$((n+1))
done
set -xv
cat > tslist_std << EOF
 &timeseries
  namt=${namts}
  ipar=${part}
  ilei=${leit}
  ilel=${lelt}
  ilsm=${lsmt}
  icdf=${cdft}
  xdif=${dift}
  stnd=${stdt}
  lini=${lini}
  linl=${linl}
  lisi=${lisi}
  lisl=${lisl}
  liwi=${liwi}
  liwl=${liwl}
  liei=${liei}
  liel=${liel}
EOF
#
# Projections on spatial patterns (usually EOFs)
#
# namp:   name of the projection
#         1. Projection onto the NAO: NAOP
#         2. Projection onto the PNA: PNAP
#         3. Projection onto the Northern Hemisphere AO: AONP
# par:    variable to be analysed
# lev:    level
# ilsm:   use of land sea mask
#         1 = land points
#         0 = no land-sea mask applied
#        -1 = sea points
# lim:    boundaries of the corresponding region
#         order: North - South - West - East
#
TOT_PRO=3 # Total number of projections
set -A PRO_NAM Z5PNAP Z5NAOP SLAONP
set -A PRO_PAR    129    129    151
set -A PRO_LEV    500    500    000
set -A PRO_LSM      0      0      0
     PRO_LIN="   87.5   87.5   87.5"
     PRO_LIS="   20.0   20.0   20.0"
     PRO_LIW="  110.0  -90.0    0.0"
     PRO_LIE="  -60.0   60.0  357.5"
set +xv
namp=""
parp=""
levp=""
lsmp=""
n=0
while [[ $n -lt $TOT_PRO ]] ; do
      namp="${namp}'${PRO_NAM[$n]}',"
      parp="${parp}${PRO_PAR[$n]},"
      levp="${levp}${PRO_LEV[$n]},"
      lsmp="${lsmp}${PRO_LSM[$n]},"
      n=$((n+1))
done
limn=""
lims=""
limw=""
lime=""
for COOR in $PRO_LIN ; do
    limn="${limn}${COOR},"
done
for COOR in $PRO_LIS ; do
    lims="${lims}${COOR},"
done
for COOR in $PRO_LIW ; do
    limw="${limw}${COOR},"
done
for COOR in $PRO_LIE ; do
    lime="${lime}${COOR},"
done
while [[ $n -lt 10 ]] ; do
      namp="${namp}'NONE',"
      parp="${parp}999,"
      levp="${levp}999,"
      lsmp="${lsmp}999,"
      limn="${limn}999.,"
      lims="${lims}999.,"
      limw="${limw}999.,"
      lime="${lime}999.,"
      n=$((n+1))
done
set -xv
cat > prolist_std << EOF
 &timeseries
   namp=${namp}
    par=${parp}
    lev=${levp}
   ilsm=${lsmp}
   limn=${limn}
   lims=${lims}
   limw=${limw}
   lime=${lime}
EOF
#
# EOFs
#
# name:   name of the EOF analysis
#         1. North Atlantic area with Z500: NAZ?
#         2. North Pacific area with Z500: NPZ?
#         3. Northern Hemisphere with MSLP: NPZ?
#         where ? stands for the number of the EOF
# nume:   number of EOFs to retain
# par:    variable to be analysed
# lev:    level
# ilsm:   use of land sea mask
#         1 = land points
#         0 = no land-sea mask applied
#        -1 = sea points
# lim:    boundaries of the corresponding region
#         order: North - South - West - East
#
TOT_EOF=3 # Total number of projections
set -A EOF_NAM  NAZ  NPZ  NHP
set -A EOF_NUM    1    2    1
set -A EOF_PAR  129  129  151
set -A EOF_LEV  500  500  000
name=""
nume=""
pare=""
leve=""
n=0
while [[ $n -lt $TOT_EOF ]] ; do
      name="${name}'${EOF_NAM[$n]}',"
      nume="${nume}'${EOF_NUM[$n]}',"
      pare="${pare}${EOF_PAR[$n]},"
      leve="${leve}${EOF_LEV[$n]},"
      n=$((n+1))
done
while [[ $n -lt 10 ]] ; do
      name="${name}'NONE',"
      nume="${nume}'NONE',"
      pare="${pare}999,"
      leve="${leve}999,"
      n=$((n+1))
done
cat > eoflist_std << EOF
 &timeseries
   name=${name}
   nume=${nume}
    par=${pare}
    lev=${leve}
   ilsm=    0,    0,    0,  999,  999,  999,  999,  999,  999,  999,
   limn= 87.5, 87.5, 87.5, 999., 999., 999., 999., 999., 999., 999.,
   lims=  20.,  20.,  20., 999., 999., 999., 999., 999., 999., 999.,
   limw= 110., -90.,   0., 999., 999., 999., 999., 999., 999., 999.,
   lime= -60.,  60.,357.5, 999., 999., 999., 999., 999., 999., 999.,
EOF











#
# Events for probability scores
#
set -A PEVT_LIST $BRV_EVT
BRV_NEVT=0
BRV_PEVT=""
for PEVT in ${PEVT_LIST[*]} ; do
    BRV_NEVT=$((BRV_NEVT+1))
    BRV_PEVT="${BRV_PEVT},${PEVT}"
done
echo "Number of events is $BRV_NEVT"
ie=$((BRV_NEVT+1))
while [[ $ie -le 10 ]] ; do
      BRV_PEVT="${BRV_PEVT},999"
      ie=$((ie+1))
done
BRV_PEVT=${BRV_PEVT#,}
#
echo "---------------------------"
echo "This is the end of config.h"
echo "---------------------------"
