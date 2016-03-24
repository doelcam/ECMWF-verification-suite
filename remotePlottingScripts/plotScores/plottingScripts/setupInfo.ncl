load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"


load "/home/jupiter/cpdn/macleod/Scripts/vSuite_scripts/plotScores/functions/function_CalculateReliabilityCategory.ncl"
load "/home/jupiter/cpdn/macleod/Scripts/vSuite_scripts/plotScores/functions/function_ReadDeterministicScoresFromAscii.ncl"
load "/home/jupiter/cpdn/macleod/Scripts/vSuite_scripts/plotScores/functions/function_ReadDetScoreDataFromAscii.ncl"
load "/home/jupiter/cpdn/macleod/Scripts/vSuite_scripts/plotScores/functions/function_ReadProbabilisticScoresFromAscii.ncl"
load "/home/jupiter/cpdn/macleod/Scripts/vSuite_scripts/plotScores/functions/function_ReadReliabilityDataFromAscii.ncl"
load "/home/jupiter/cpdn/macleod/Scripts/vSuite_scripts/plotScores/functions/function_AssignGiorgiDomain.ncl"


;_________________________
; Variable definitions & lists

runName="exampleVerificationRun"
dataRootDir="/home/chaos/pred/macleod/"

figDir=dataRootDir+"/Figures/"+runName
system ("if [ ! -d "+figDir+" ]; then mkdir "+figDir+";fi")
; Miscellaneous switches
plotOceanSST=2 ; 0 mask ocean, 1 plotting SST (only if var is soil temp/SST), 2 for no masking (except soil moisture)
plotType = "ps"
verbose = True ; True will print out every plot filename on script execution


;_________________________
; Experiment, var info info

DataExtension 		= ".grb"
Sys			="004"


; List the 4-digit experiment id, the type and then an identifier as separate elements - add extra dimensions to the array
; to run verificaiton for additional experiments
expInfo 		= (/\
				(/"0001","od","System 4"/),\
				(/"0001","od","System 4"/)\
			  /)
			  
; Array holding info for all variables (variable code, vertical height code, name, unit, reference data) 			  
varInfo			= (/\
				(/"228","000","Total ;precipitation","[mm/day]","GPCP"/),\
				(/"167","000","2m air temperature","[~F34~0~F~C]","ERA-I reanalysis"/)\
;				(/"039","000","Soil moisture lvl 1","","ERA Land"/),\
;				(/"139","000","Soil temperature lvl 1","[~F34~0~F~C]","ERA Land"/),\
;				(/"146","000","Surface sensible heat flux","[W/m~S~2~N~]","ERA-I reanalysis"/),\
;				(/"147","000","Surface latent heat flux","[W/m~S~2~N~]","ERA-I reanalysis"/),\
;				(/"151","000","Mean sea level pressure","[hPa]","ERA-I reanalysis"/),\
;				(/"130","850","Temperature at 850hPa","[~F34~0~F~C]","ERA-I reanalysis"/),\
;				(/"129","500","500hPa geopotential height","[dm]","ERA-I reanalysis"/)\	
			   /)
			   		   
			   
;_________________________
; Reliabilty function info  - the reliability function relies on this being correct, so check if you change (e.g.) ensemble members
nBins 			= 10
nBoot 			= 1000
nMembers 		= 51
SigLvl 			= 95


;_________________________
; Time

yIni			= 1981
yFin			= 1984
hindcastPeriod 		= ""+yIni+"-"+yFin
initialisationMonthList = (/"02","05","08","11"/)

; info about the leadtime verificaiton considered - must be consistent with leadtimeNames below
leadtimeInfo		= (/\
				(/0,0/),\  ; month 1
				(/1,3/)\ 
			  /)
			 		  
; lead time names - code, then the name for each of the start dates listed in initializationMonthList			  
leadtimeNames 		= (/\
				(/"001-001","February","May","August","November"/),\
				(/"002-004","MAM","JJA","SON","DJF"/)\
;				(/"002-002","March","June","September","December"/),\
;				(/"003-003","April","July","October","January"/),\
;				(/"004-004","May","August","November","February"/),\
			   /)
				

; for verification produced by cal_bias, this is the scores code name, real name and NCL colortable used
climPlotInfo 		= (/\
				(/"BIAS","Bias","BlueWhiteOrangeRed"/),\
				(/"CLIM","Climatology","BlAqGrYeOrRe"/),\
				(/"SDRT","Model/ref standard deviation ratio","BlueWhiteOrangeRed"/),\
				(/"STDV","Standard deviation","perc2_9lev"/)\
			  /)
;_________________________
; Contour levels: for each variable, bias/clim/sdrt/stdv - each should be size 12. Might need to play with these to get nice plots!			
climContourLevels 		= (/\
			 (/(/-20,-10,-5,-2.5,-1,-0.5,0.5,1,2.5,5,10,20/)/2.,\
			ispan(1,12,1)/2.0,\			; Precipitation 
			(/0,0.2,0.4,0.6,0.8,0.9,1.1,1.2,1.5,2,4,8/),\
			fspan(0.2,3.5,12)/),\
			(/(/-5,-4,-3,-2,-1,-0.5,0.5,1,2,3,4,5/),\ 
			ispan(255,310,5),\		; 167 2mT
			(/0,0.2,0.4,0.6,0.8,0.9,1.1,1.2,1.5,2,4,8/),\
			fspan(0.2,3.5,12)/)\
			/)
;			\
;(/(/-0.5,-0.4,-0.3,-0.2,-0.1,-0.05,0.05,0.1,0.2,0.3,0.4,0.5/)/2,\
;			fspan(0.1,0.65,12),\		; 039 Lvl 1 soil moisture
;			(/0,0.2,0.4,0.6,0.8,0.9,1.1,1.2,1.5,2,4,8/),\
;			fspan(0.01,0.065,12)/),\
;			\
;			(/(/-5,-4,-3,-2,-1,-0.5,0.5,1,2,3,4,5/),\ 
;			ispan(255,310,5),\		; 139 Lvl 1 soil temperature
;			(/0,0.2,0.4,0.6,0.8,0.9,1.1,1.2,1.5,2,4,8/),\
;			fspan(0.2,3.5,12)/),\
;			\
;			(/(/-5,-4,-3,-2,-1,-0.5,0.5,1,2,3,4,5/)*4,\ 
;			ispan(-55,55,10),\		; sshf
;			(/0,0.2,0.4,0.6,0.8,0.9,1.1,1.2,1.5,2,4,8/),\
;			fspan(1,12,12)/),\
;			\ 
;			(/(/-5,-4,-3,-2,-1,-0.5,0.5,1,2,3,4,5/)*4,\ 
;			ispan(-110,0,10),\		; slhf
;			(/0,0.2,0.4,0.6,0.8,0.9,1.1,1.2,1.5,2,4,8/),\
;			fspan(1,12,12)/),\
;			\
;			(/(/-5,-4,-3,-2,-1,-0.5,0.5,1,2,3,4,5/),\ 
;			ispan(998,1020,2),\	       ; mslp
;			(/0,0.2,0.4,0.6,0.8,0.9,1.1,1.2,1.5,2,4,8/),\
;			fspan(0.2,3.5,12)/),\
;			\
;			(/(/-5,-4,-3,-2,-1,-0.5,0.5,1,2,3,4,5/),\ 
;			ispan(255,310,5),\	       ; T 850
;			(/0,0.2,0.4,0.6,0.8,0.9,1.1,1.2,1.5,2,4,8/),\
;			fspan(0.2,3.5,12)/),\
;			\
;			(/(/-12,-10,-8,-6,-4,-2,2,4,6,8,10,12/),\
;			ispan(495,605,10),\	       ; 129 Z500
;			(/0,0.2,0.4,0.6,0.8,0.9,1.1,1.2,1.5,2,4,8/),\
;		       fspan(0.8,8.5,12)/)\


; for each variable listed above, this gives the scaling of the contours in the comparison plots (used in PlotClimatologyMapComparison.ncl)
climCompContourLevels		= (/-3.2,-1.6,-0.8,-0.4,-0.2,-0.1,0.1,0.2,0.4,0.8,1.6,3.2/) 
climCompContourLevelScaling	= (/2,1,0.05,1,1,2,1,5,5,10,1,5/)
			

; as climPlotInfo for the different deterministic scores			
detPlotInfo 		= (/\
				(/"ACC","Anomaly correlation coefficient","BlueWhiteOrangeRed"/),\
				(/"ACP","Anomaly correlation coefficient (perfect model)","BlueWhiteOrangeRed"/),\
				(/"MSS","Mean square skill score","BlueWhiteOrangeRed"/),\
				(/"RSR","Ratio spread(sd)/RMSE","BlueWhiteOrangeRed"/),\
				(/"SPR","Normalised spread (wrt ref sd)","BlueWhiteOrangeRed"/)\
			  /)
detContourLevels 		=\
		(/(/-1,-0.9,-0.8,-0.7,-0.6,-0.4,-0.2,0.2,0.4,0.6,0.7,0.8,0.9,1/),\
		(/-1,-0.9,-0.8,-0.7,-0.6,-0.4,-0.2,0.2,0.4,0.6,0.7,0.8,0.9,1/),\
		(/-40,-4,-2,-1,-0.8,-0.5,-0.2,0.2,0.4,0.6,0.7,0.8,0.9,1/),\
		(/0,0.3,0.5,0.6,0.7,0.8,0.9,1.1,1.2,1.3,1.4,1.6,1.8,2/),\
		(/0,0.3,0.5,0.6,0.7,0.8,0.9,1.1,1.2,1.3,1.4,1.6,1.8,2/)/)\
		
; as climPlotInfo for the different probabilistic scores
probPlotInfo 		= (/\
				(/"BSS","Brier Score","precip3_16lev"/),\
				(/"BSI","Brier Score (infinite ensemble)","precip3_16lev"/),\
				(/"REL","Reliability skill score","precip3_16lev"/),\
				(/"RES","Resolution skill score","precip3_16lev"/),\
				(/"ROC","ROC skill score","precip3_16lev"/),\
				(/"SHA","Sharpness","precip3_16lev"/),\
				(/"IGN","Ignorance","precip3_16lev"/)\
			  /)
probContourLevels 		= (/(/0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/),\ ;
			(/0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/),\ ;
			(/0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/),\ ;
			(/0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/),\ ;
			(/0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/),\ ;
			(/0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1/),\ ;
			(/0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/)/)

			
; Events
eventInfo 		= (/\
				(/"lower tercile","33"/),\
				(/"upper tercile","67"/)\
;				(/"lower decile","10"/),\
;				(/"lower quintile","20"/),\
;				(/"lower quartile","25"/),\
;				(/"above median","50"/)\
;				(/"upper quintile","80"/),\
;				(/"upper decile","90"/),\
			  /)
eventPercentileList	= (/.33,.33,.1,.2,.25,.5,.2,.1/) ; these should be the proportion of the events out of total (not the percentile)

; this is used in PlotProbabilisticScores, so it selects the relevant score from the list written out by the suite
MetricIndexList 	= (/0,1,2,4,6,12,10,11/)


; One 3-dimension element for each region studied
regionData		=(/\
			(/"TROP","Tropics (TROP)"/),\
			(/"TROP","Tropics (TROP)"/)\
			/)

;regionData		=(/(/"AUSG","Australia (AUS)"/),\
;			(/"AMZG","Amazon (AMZ)"/),\
;			(/"SSAG","Southern South America (SSA)"/),\
;			(/"CAMG","Central America (CAM)"/),\
;			(/"WNAG","Western North America (WNA)"/),\
;			(/"CNAG","Central North America (CNA)"/),\
;			(/"ENAG","Eastern North America (ENA)"/),\
;			(/"ALAG","Alaska (ALA)"/),\
;			(/"GRLG","Greenland (GRL)"/),\
;			(/"MEDG","Mediterranean basin (MED)"/),\
;			(/"NEUG","Northen Europe (NEU)"/),\
;			(/"WAFG","Western Africa (WAF)"/),\
;			(/"EAFG","Eastern Africa (EAF)"/),\
;			(/"SAFG","Southern Africa (SAF)"/),\
;			(/"SAHG","Sahara (SAH)"/),\
;			(/"SEAG","Southeast Asia (SEA)"/),\
;			(/"EASG","East Asia (EAS)"/),\
;			(/"SASG","South Asia (SAS)"/),\
;			(/"CASG","Central Asia (CAS)"/),\
;			(/"TIBG","Tibet (TIB)"/),\
;			(/"GLOL","Global Land (GLOL)"/),\
;			(/"GLOB","Global (GLOB)"/),\
;			(/"TROP","Tropics (TROP)"/),\
;			(/"TROL","Tropical Land (TROL)"/),\
;			(/"NASG","North Asia (NAS)"/)/)
			
;_________________________
; Subset regions

; this will make map plots for user-defined regions
RegionNames	=(/"","_Europe","_USA","_Russia"/) ; suffix on plot name
LatLons		=(/(/-90.,90.,-180.,180./),\ ; global
		(/25.,75.,-15.,60./),\ ; Europe
		(/25.,50.,-125.,-65./),\ ; USA
		(/40.,70.,15.,55./)/) ; Russia

;_________________________
; Loop limits
nExp 			= dimsizes(expInfo(:,0))
nClimPlotTypes 		= dimsizes(climPlotInfo(:,0))
nDetPlotTypes 		= dimsizes(detPlotInfo(:,0))
nProbPlotTypes 		= dimsizes(probPlotInfo(:,0))
nVars	 		= dimsizes(varInfo(:,0))
nTargets		= dimsizes(leadtimeInfo(:,0))
;nTargets		= 1

nEvents			= dimsizes(eventInfo(:,0))
nStarts			= dimsizes(initialisationMonthList)
nPlotRegions		= dimsizes(RegionNames)
nRegions		= dimsizes(regionData(:,0))














;---------- Defining some functions ---------;

undef("getFile")
function getFile(filename)
begin
 f = addfile(filename,"r")
 vNames = getfilevarnames (f) 
 fld = f->$vNames(0)$
 delete(vNames)
 return fld
end

undef("setAxes")
function setAxes(fld)
begin
 if (isMonotonic(fld&lat).eq.-1) then
  fld=fld(::-1,:)
 end if
 if any(fld&lon.ge.180) then
  fld=lonFlip(fld)
 end if
 
 return fld
end



undef("scaleVar")
function scaleVar(fld,varNumber,plotTypeName)
begin
 ; Converting Z500 to decameters
 if (varNumber.eq."129".and.plotTypeName.ne."SDRT") then
  fld=fld/100
 end if

 ; Converting mslp from Pa to hPa
 if (varNumber.eq."151".and.plotTypeName.ne."SDRT") then
  fld=fld/100
 end if

 return(fld)
end


undef("maskField")
function maskField(fld,varNumber,plotOceanSST)
begin
 a=addfile("$NCARG_ROOT/lib/ncarg/data/cdf/landsea.nc","r")
 lsdata = a->LSMASK
 lsm  = landsea_mask(lsdata,fld&lat,fld&lon)

 if (varNumber.eq."139".and.plotOceanSST.eq.1) then
  fld = mask(fld,lsm.eq.1,False)
   varName = "Sea surface temperature"
   varReference = "ERA-Interim"
 else

  if (plotOceanSST.eq.0) then
  fld = mask(fld,lsm.eq.0,False)
  end if
 end if
 
 if (varNumber.eq."039") then
  fld = mask(fld,lsm.eq.0,False)
 end if

 return fld
end


undef("loadBootstrap")
function loadBootstrap(filenameroot,bootstrap)
begin
 if (bootstrap) then
  bootdata 			= filenameroot+"_S01000"+DataExtension
  f 			= addfile(bootdata,"r")
  bootvNames 		= getfilevarnames (f) 
  fld_boot 			= f->$bootvNames(0)$
  fld_boot!0 		= "lat"
  fld_boot!1 		= "lon"
  return fld_boot
 else
  return 0
 end if
  
end
