--------------
- 28/02/2008 -
--------------

Some MM files with CRU data from 1951 to 2000 are in 
/vol/demeter/verify/data/scru
The original data are in ec:/DEMETER/verify/data/cru
The scripts used in DEMETER are in
/home/rd/nep/sms/verify/automat/include/develop
The land-sea mask is created with
/home/rd/nep/sms/verify/automat/include/manual/create_lsm.job
which uses a normal land-sea mask file and then looks for 
the missing data in all the monthly mean files to maks them too.
The land-sea mask obtained for DEMETER (using data from 1958
to 1986) is in /vol/demeter/verify/data/const/LSM_scru_fin

I downloaded the most recent version of the CRU dataset from
http://www.cru.uea.ac.uk/cru/data/hrg/cru_ts_2.10/data_dec/
The information of the dataset is in
http://www.cru.uea.ac.uk/~timm/grid/CRU_TS_2_1.html
The data are available until 2002. The tar file is stored in
ec:/ENSEMBLES/verify/data/cru
The land-sea mask is created with
/home/ne/nep/sms/verify/scores/include/cru/create_lsm
The scripts to write the GRIB files have been modified and
are available from /home/ne/nep/sms/verify/scores/include/cru

--------------
- 03/03/2008 -
--------------

The script make_climatologies creates seasonal climatologies
for the three datasets. cont_prec shows nice contours in metview.

