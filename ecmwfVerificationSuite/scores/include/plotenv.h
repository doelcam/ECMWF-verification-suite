echo "----------------------------------"
echo "This is the beginning of plotenv.h"
echo "----------------------------------"
#
# Basic IDL and plot related variables
#
use idl
#
export COL="/home/rd/ocx/idl/col"
export IDL_PATH="/home/rd/ocx/idl/lib:/home/rd/ocx/oc_pp/plot:<IDL_DEFAULT>"
#
export DISPLAY=${DISP}:0.0
DEBUG=1
PATHSO_IDL="/home/rd/nep/sms/verify/scores/idl" # Source of the idl files
export IDL_STARTUP="/home/rd/dij/idl/idlstartup.pro" 
#
# Basic actions before plotting
#
export CHART_BASE=/vol/demeter/verify/catalogue/charts
export MY_DATE=20020602

TO_PLOT_DIR() {

    set -x
    set -e

	chart=$CHART_BASE/${SUITE}_${TASK}/${EXPVER}/${MY_DATE}
    _pic_n=${_pic_n:-0}
    for n in $@; do
        _pic_n=$((_pic_n+1))
        ln -sf $n $TMPDIR/output.$_pic_n.ps
#       cp $n $TMPDIR/output.$_pic_n.ps
#       chmod 777 $TMPDIR/output.$_pic_n.ps
#       ls -l $TMPDIR
        /usr/local/lib/metops/vmr/maxcp $TMPDIR/output.$_pic_n.ps $chart
    done
}

TO_PLOT_DIR_RH() {

    set -x
    set -e

	chart=$CHART_BASE/${SUITE}_${TASK}/${EXPVER}/${SEASONAL_DATE}
    _pic_n=${_pic_n:-0}
    for n in $@; do
        _pic_n=$((_pic_n+1))
        ln -sf $n $TMPDIR/output.$_pic_n.ps
#       cp $n $TMPDIR/output.$_pic_n.ps
#       chmod 777 $TMPDIR/output.$_pic_n.ps
#       ls -l $TMPDIR
        /usr/local/lib/metops/vmr/maxcp $TMPDIR/output.$_pic_n.ps $chart
    done
}


PLOT_NOTIFY() {

    set -e
    set -x

	define=/plots/data/web/define/demeter
	CHART_DEFINE=${CHART_DEFINE:-$define}
    [[ -d $CHART_DEFINE ]] || mkdir -m g+w -p $CHART_DEFINE
    conf=/vol/demeter/verify/catalogue/$TASK
    conf=${conf}.vmr
	filebase=${TASK}_${EXPVER}
    outpath=$define

    if [[ -f $conf ]]; then
        /usr/local/share/perl /usr/local/lib/metops/vmr/vmr_create.pl $conf $filebase
        mv $filebase.pdb $filebase.p || true
        mv $filebase.pdi $filebase.i
        /usr/local/lib/metops/vmr/maxcp $filebase.i $outpath
        /usr/local/lib/metops/vmr/maxcp $filebase.p $outpath || true
    fi
}

typeset -fx TO_PLOT_DIR PLOT_NOTIFY

export CLASS=demeter
export PLOT_STREAM=seasonal
export SUITE=%SUITE%
export TASK=%TASK%
#export EXPVER=%EXPVER%

export BASETIME=%YYYY%%MM%%DD%
#export PLOT_DATE=$BASETIME
export PLOT_DATE=$MY_DATE

export CHART_ROOT=images/demeter

echo "----------------------------"
echo "This is the end of plotenv.h"
echo "----------------------------"
#
