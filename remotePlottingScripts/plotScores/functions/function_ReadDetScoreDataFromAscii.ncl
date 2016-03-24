; This is an ad-hoc function to read in the ascii data for the deterministic score
; diagram plus boot data, giving out a table the size of nBins * 7, where
; the 7 dimensions are... (confirm!, but 4 & 5 are x/y points, 6 & 7 are low & high 
; error bounds...1-3 all look like they could be bin population size).

; Dave MacLeod University of Oxford, 22nd April 2013

undef("read_deterministic_scores_ascii")
function read_deterministic_scores_ascii(filename,StartYear,EndYear,nBins,\
nBoot,nMembers,SigLvl)

; Filename must include full path

begin
;-----------------------------
; Location of the files
;filename = "/home/cirrus/pred/macleod/InitialPlotData/fg79/rel/BRV_CrdOecmfEfg79S001M001_1989-2008_050100_CV01_I0_228_000_T67_M1_B10_002-004_TROL"
;StartYear=1989
;EndYear =2008
;nBins = 10 ; # of bins used
;nBoot = 1000; # of bootstrap samples
;nMembers =25; # important for locating the data within the ascii file
;SigLvl = 95

bootfilename =filename+"_S0"+nBoot+".ran"

;-----------------------------
; Reading in the ascii data
fld = asciiread(filename,-1,"float")
boot_fld = asciiread(bootfilename,-1,"float")

nYears = (EndYear-StartYear+1)

readinpoint = 3+nMembers-1+((nMembers+2)*nYears); 25 members, 26th is the average I think - > it's not!

print(fld(0:30))
;exit

print(dim_avg_Wrap(fld(4:28)))
exit

do iBoot=0,nBoot-1

	readinpointboot=1+iBoot*((nMembers+1)*4+15)+iBoot*nBins*5

	do iBin=0,nBins-1

		boot_data(iBin,iBoot) = boot_fld(5*iBin+readinpointboot+4)
		reliability_data(iBin,0:4) =\
		fld(5*iBin+readinpoint:5*iBin+readinpoint+4)
	
	end do
end do

tmp = dim_pqsort(boot_data,2)
delete(tmp)

errors = new((/2,nBins/),float)

; Calculating the thresholds for the required SigLvl level
low_thresh = (100-SigLvl)/2
high_thresh = 100-low_thresh
low_index = floattoint(nBoot*low_thresh/100)
high_index = floattoint(nBoot*high_thresh/100)

reliability_data(:,5) = boot_data(:,low_index)
reliability_data(:,6) = boot_data(:,high_index)
delete(boot_data)

return(reliability_data) 


end
