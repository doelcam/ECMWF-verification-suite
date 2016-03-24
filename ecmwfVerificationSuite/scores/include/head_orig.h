#!/bin/ksh
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
# QSUB -r %TASK%

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

# Defines the three variables that are needed for any
# communication with SMS

export SMS_PROG=%SMS_PROG%  # SMS Remote Procedure Call number
export SMSNODE=%SMSNODE%    # The name sms that issued this task
export SMSNAME=%SMSNAME%    # The name of this current task
export SMSPASS=%SMSPASS%    # A unique password
export SMSTRYNO=%SMSTRYNO%  # Current try number of the task
export SMSHOME=%SMSHOME%    # home directory for sms


LOADL_STEP_ID=${LOADL_STEP_ID:=NOT_SET}
QSUB_REQID=${QSUB_REQID:=NOT_SET}
PBS_JOBID=${PBS_JOBID:=NOT_SET}
SGE_JOB_SPOOL_DIR=${SGE_JOB_SPOOL_DIR:=NOT_SET}

if [[ $LOADL_STEP_ID != NOT_SET ]] ; then
  SMSRID=$(echo $LOADL_STEP_ID | cut -f2 -d.)
  JOB_ID=$LOADL_STEP_ID
elif [[ $QSUB_REQID != NOT_SET ]] ; then
  SMSRID=$(echo $QSUB_REQID | cut -f1 -d.)
  JOB_ID=$QSUB_REQID
elif [[ $PBS_JOBID != NOT_SET ]] ; then
  SMSRID=$(echo $PBS_JOBID | cut -f1 -d.)
  JOB_ID=$PBS_JOBID
elif [[ $SGE_JOB_SPOOL_DIR != NOT_SET ]] ; then
  SMSRID=$(basename $SGE_JOB_SPOOL_DIR .1)
  JOB_ID=$SMSRID
else
  SMSRID=$$
  JOB_ID=$SMSRID
fi

# Tell SMS we have stated
# The SMS variable SMSRID will be set to parameter of smsinit
# Here we give the current PID.

smsinit $SMSRID

# Defined a error hanlder

ERROR() {
	set +e        # Clear -e flag, so we don't fail
	smsabort      # Notify SMS that something went wrong
	trap 0        # Remove the trap
	exit 0        # End the script
}

# Trap any calls to exit and errors caught by the -e flag

trap ERROR 0

# Trap any signal that may cause the script to fail

trap '{ echo "Killed by a signal"; ERROR ; }' 1 2 3 4 5 6 7 8 10 12 13 15

