; This is an ad-hoc function to read in the ascii data for the deterministic score
; diagram plus boot data, giving out a table the size of nBins * 7, where
; the 7 dimensions are... (confirm!, but 4 & 5 are x/y points, 6 & 7 are low & high 
; error bounds...1-3 all look like they could be bin population size). 
; Dave MacLeod University of Oxford, 22nd April 2013

; 16/08/13 DM: Added functionality for when there is no boot data (because of the absence of NAG) - just set nBoot to 0 when calling the routine

undef("read_reliability_ascii")
function read_reliability_ascii(filename,StartYear,EndYear,nBins,\
nBoot,nMembers,SigLvl)

; Filename must include full path

begin
;-----------------------------

bootfilename =filename+"_S0"+sprinti("%0.4i",nBoot)+".ran"

;-----------------------------
; Reading in the ascii data
fld = asciiread(filename,-1,"float")
nYears = (EndYear-StartYear+1)


readinpoint = 2+(15*nYears); for the reliability table ; changed 14 to 15 after addition of ignorance

reliability_data = new((/nBins,12/),float,-9999.) 
; first 4 for reliability data from file,
; next two for the error bars
; final six for regression line data - c_0, c_1 for reg line, then for lower
; and upper confidence intervals (in the first bin)

; Going through for reliability data
do iBin=0,nBins-1
	reliability_data(iBin,0:3) =\
	fld(5*iBin+readinpoint+1:5*iBin+readinpoint+4)
end do

reliability_data(0,6:7) = lspoly(reliability_data(:,2),\
reliability_data(:,3)\
,0.00000000001+reliability_data(:,0),2) ; added this fraction to the weights, as lspoly playing up when zero weight present

; Gooing through for boot data

if (nBoot.ne.0) then ; read boot data, but if there isn't any, set boot data to 0, don't look for 
	boot_fld = asciiread(bootfilename,-1,"float") ; boot data in this file
	boot_data = new((/nBins,nBoot,3/),float,-9999.) ; size, x, y
	boot_regression_intercepts = new((/nBoot/),float,-9999.) ; intercept and gradient
	boot_regression_gradients = new((/nBoot/),float,-9999.) ; intercept and gradient
	
	; Going through for boot data
	do iBoot=0,nBoot-1
		readinpointboot=1+iBoot*((nMembers+1)*4+16)+iBoot*nBins*5 ; changed from 15 to 16 after addition of ignorance

		do iBin=0,nBins-1
			if (nBoot.ne.0) then
				boot_data(iBin,iBoot,0) = boot_fld(5*iBin+readinpointboot+1)
				boot_data(iBin,iBoot,1) = boot_fld(5*iBin+readinpointboot+3)
				boot_data(iBin,iBoot,2) = boot_fld(5*iBin+readinpointboot+4)
			end if
		end do
		tmp_coeffs = lspoly(\
		boot_data(:,iBoot,1),boot_data(:,iBoot,2),0.00000000001+boot_data(:,iBoot,0),2)
		boot_regression_intercepts(iBoot) = tmp_coeffs(0)
		boot_regression_gradients(iBoot) = tmp_coeffs(1)
		delete(tmp_coeffs)
	end do
	; Calculating the thresholds for the required SigLvl level
	tmp = dim_pqsort_n(boot_data,2,1)
	delete(tmp)

	errors = new((/2,nBins/),float)

	low_thresh = (100-SigLvl)/2
	high_thresh = 100-low_thresh
	

	low_index = floattoint(nBoot*low_thresh/100)
	high_index = floattoint(nBoot*high_thresh/100)

	reliability_data(:,4) = boot_data(:,low_index,2)
	reliability_data(:,5) = boot_data(:,high_index,2)
	delete(boot_data)


	; Now calculating the thresholds for the confidence regression lines

	; Sorting both coefficients, and taking largest (smallest) gradient (intercept)
	; to get each confidence line...so intercept and gradient don't necessarily come from ; the same bootstrap sample. Is this ok? Otherwise, you could sort by either.
	; It makes a difference in at least the 2nd d.p. of the intercept.

	; I have now changed it to keep them together
	tmp = dim_pqsort(boot_regression_gradients,2)
	low_intercept_index = tmp(low_index)
	high_intercept_index = tmp(high_index)
	delete(tmp)

	reliability_data(0,8) = boot_regression_intercepts(low_intercept_index)
	reliability_data(0,9) = boot_regression_gradients(low_index)
	reliability_data(0,10) = boot_regression_intercepts(high_intercept_index)
	reliability_data(0,11) = boot_regression_gradients(high_index) 

	delete(boot_regression_intercepts)
	delete(boot_regression_gradients)
else
	reliability_data(0,8) = -2
	reliability_data(0,9) = 0
	reliability_data(0,10) =-2
	reliability_data(0,11) =0


end if
; if nBoot is zero, we don't do this loop - reliability data stays filled with missing values

return(reliability_data) 


end
