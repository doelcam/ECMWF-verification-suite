; This is an ad-hoc function to read in the ascii data for the deterministic score
; diagram plus boot data. There are two functions, since there ACC/ACP/MSS and RSR/RSP scores have different ascii format.
; Dave MacLeod University of Oxford, 26th April 2013

;---------------------------------------------------
; this function is for the ACC/ACP and MSS
undef("read_det_scores_ascii")
function read_det_scores_ascii(filename,StartYear,EndYear,nBins,\
nBoot,nMembers,SigLvl)

; Filename must include full path

begin
;-----------------------------

bootfilename =filename+"_S0"+sprinti("%0.4i",nBoot)+".ran"

;-----------------------------
; Reading in the ascii data
fld = asciiread(filename,-1,"float")
boot_fld = asciiread(bootfilename,-1,"float")

nYears = (EndYear-StartYear+1)


boot_data = new((/nBoot/),float) ; size, x, y
det_scores = new((/3/),float) ; first one for data, then upper and lower CIs

readinpoint = 1+(nMembers+2)*(nYears+1)

det_scores(0) = fld(readinpoint)


do iBoot=0,nBoot-1
	readinpointboot=(iBoot+1)*(nMembers+2)
	boot_data(iBoot) = boot_fld(readinpointboot)
end do

; Calculating the thresholds for the required SigLvl level
tmp = dim_pqsort_n(boot_data,2,0)
delete(tmp)

low_thresh = (100-SigLvl)/2
high_thresh = 100-low_thresh
low_index = floattoint(nBoot*low_thresh/100)
high_index = floattoint(nBoot*high_thresh/100)

det_scores(1) = boot_data(low_index)
det_scores(2) = boot_data(high_index)
delete(boot_data)
return(det_scores) 

end

;---------------------------------------------------
; this function is for the ensemble characteristic scores, i.e. spread/error ratios
undef("read_det_ens_scores_ascii")
function read_det_ens_scores_ascii(filename,StartYear,EndYear,nBins,\
nBoot,nMembers,SigLvl)

; Filename must include full path

begin
;-----------------------------

bootfilename =filename+"_S0"+sprinti("%0.4i",nBoot)+".ran"

;-----------------------------
; Reading in the ascii data
fld = asciiread(filename,-1,"float")
boot_fld = asciiread(bootfilename,-1,"float")

nYears = (EndYear-StartYear+1)


boot_data = new((/nBoot/),float) ; size, x, y
det_scores = new((/3/),float) ; first one for data, then upper and lower CIs

readinpoint = 3+(nYears*2)

det_scores(0) = fld(readinpoint)

do iBoot=0,nBoot-1
	readinpointboot=2+2*iBoot
	boot_data(iBoot) = boot_fld(readinpointboot)
end do

; Calculating the thresholds for the required SigLvl level
tmp = dim_pqsort_n(boot_data,2,0)
delete(tmp)

low_thresh = (100-SigLvl)/2
high_thresh = 100-low_thresh
low_index = floattoint(nBoot*low_thresh/100)
high_index = floattoint(nBoot*high_thresh/100)

det_scores(1) = boot_data(low_index)
det_scores(2) = boot_data(high_index)
delete(boot_data)

return(det_scores)

end
