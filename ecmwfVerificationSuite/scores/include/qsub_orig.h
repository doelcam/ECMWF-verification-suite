:
# QSUB -eo
# QSUB -ro
# QSUB -nr
# QSUB -s /bin/ksh
# QSUB -o %LOGDIR%%SMSNAME%.%SMSTRYNO%
# QSUB -q %QUEUE%
# QSUB -lT %CPUTIME%
# QSUB -lt %CPUTIME%
# QSUB -lM %MEM%Mb
# QSUB -lPv %NPES%
