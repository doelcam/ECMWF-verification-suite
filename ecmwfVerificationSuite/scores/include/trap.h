banner trap.h 
#==========================================================================
#  Trap handling and SMS initialisation
#
#  Write into the ERROR function what to do in case of any error.
#==========================================================================

typeset -l ARCH 

if [[ "$ARCH" = hpia64 ]] ; then
  rcp /home/ma/emos/data/dummy.output emos@%SMSNODE%:%SMSJOBOUT% || true 
fi

set -a
LOADL_STEP_ID=${LOADL_STEP_ID:=NOT_SET}
QSUB_REQID=${QSUB_REQID:=NOT_SET}
PBS_JOBID=${PBS_JOBID:=NOT_SET}

if [[ $LOADL_STEP_ID != NOT_SET ]] ; then
  SMSRID=$(echo $LOADL_STEP_ID | cut -f1 -d.)
  SMSRID=$(substring $SMSRID 6 8  )
  SMSRID=$(echo $LOADL_STEP_ID | cut -f2 -d.)${SMSRID}
  # SMSRID=$(echo $LOADL_STEP_ID | cut -f2 -d.)
  JOB_ID=$LOADL_STEP_ID
elif [[ $QSUB_REQID != NOT_SET ]] ; then
  SMSRID=$(echo $QSUB_REQID | cut -f1 -d.)
  JOB_ID=$QSUB_REQID
elif [[ $PBS_JOBID != NOT_SET ]] ; then
  SMSRID=$(echo $PBS_JOBID | cut -f1 -d.)
  JOB_ID=$PBS_JOBID

  TOPATH=%TOPATH:/tmp/output%
  link_name=`~emos/bin/subs_path.pl -f %SMSOUT% -t $TOPATH -n %SMSJOBOUT%`
  mkdir -p `dirname $link_name` || : 
  ln -sf /var/spool/PBS/spool/${PBS_JOBID}.OU $link_name || :

  rm -f %SMSJOBOUT% || :
  ln -sf /var/spool/PBS/spool/${PBS_JOBID}.OU %SMSJOBOUT%

else
  SMSRID=$$
  JOB_ID=$SMSRID
fi

#SMSPASS=%SMSPASS%
#SMSNODE=%SMSNODE%
SMSNAME=%SMSNAME%
SMSTRYNO=%SMSTRYNO%
SMSHOSTFILE=$HOME/.smshostfile
SMS_PROG=%SMS_PROG%
SMSJOBOUT=%SMSJOBOUT%

set +a

ARCHWDIR=${ARCHWDIR:=""}

SMSCLEAN() {
  set -x
  set +e

  STREAM=${STREAM:=""}
  if [[ "$STREAM" = @(SEAS|MOFC|OCEA|mnfc|mnfh) ]] ; then
    if [[ "%TASK%" = logfiles ]] ; then
      n=0
      while [[ -d $WDIR ]] ; do
        # Forked processes could still be around, and 'rm -rf' fails
        rm -rf $WDIR || true
        ls -l $WDIR && sleep 2 || true
        n=$((n+1))
        [[ $n -gt 5 ]] && break
      done || true
    fi
  fi
  FSFAMILY=${FSFAMILY:=""}
  if [[ "$FSFAMILY" = /mars ]] ; then
    cd $TMPDIR
    n=0
    while [[ -d ${WDIR}$SMSNAME ]] ; do
      # Forked processes could still be around, and 'rm -rf' fails
      rm -rf ${WDIR}$SMSNAME || true
      ls -l ${WDIR}$SMSNAME && sleep 2 || true
      n=$((n+1))
      [[ $n -gt 5 ]] && break
    done || true
  fi

  if [[ $ARCH = linux ]] ; then
    . /usr/local/share/ecmwf/share/.epilog
  fi
}
typeset -fx SMSCLEAN

export SMS_SIGNAL_LIST='1 2 3 4 5 6 7 8 10 12 13 15 24 30'

if [[ $ARCH = linux ]] ; then
   export SMS_SIGNAL_LIST='1 2 3 4 5 6 7 8 10 13 15 24 31'

   if [[ $CPU_TYPE = amd64 ]] ; then # linux cluster
     export SMS_SIGNAL_LIST='1 2 3 4 5 6 8 7 13 15 24 31'
     trap '' USR1 
     trap '{ echo "Killed by a signal"; ERROR ; }' USR2
   fi
fi

trap ERROR 0
trap '{ echo "Killed by a signal"; ERROR ; }' $SMS_SIGNAL_LIST

set -exu

[[ -d $TMPDIR ]] && cd $TMPDIR

smsinit $SMSRID &

date

