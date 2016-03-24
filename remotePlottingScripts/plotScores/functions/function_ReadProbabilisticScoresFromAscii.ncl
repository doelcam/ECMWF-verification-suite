; This is an ad-hoc function to read in the ascii data for the probabilistic score
; diagram plus boot data.
; Dave MacLeod University of Oxford, 26th April 2013

undef("read_prob_scores_ascii")
function read_prob_scores_ascii(filename,StartYear,EndYear,nBins,\
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


boot_data = new((/nBoot,15/),float) ; size, x, y
prob_scores = new((/3,15/),float) ; first one for data, then upper and lower CIs

readinpoint = 2+(15*nYears)+nBins*5

prob_scores(0,:) = fld(readinpoint:readinpoint+14)

do iBoot=0,nBoot-1
	readinpointboot=1+iBoot*((nMembers+1)*4+16)+iBoot*nBins*5+nBins*5
	boot_data(iBoot,:) = boot_fld(readinpointboot:readinpointboot+14)
end do


; Calculating the thresholds for the required SigLvl level
tmp = dim_pqsort_n(boot_data,2,0)
delete(tmp)

low_thresh = (100-SigLvl)/2
high_thresh = 100-low_thresh
low_index = floattoint(nBoot*low_thresh/100)
high_index = floattoint(nBoot*high_thresh/100)

prob_scores(1,:) = boot_data(low_index,:)
prob_scores(2,:) = boot_data(high_index,:)
delete(boot_data)

return(prob_scores) 

end
