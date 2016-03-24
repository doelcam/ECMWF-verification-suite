; This script plots climatologies, biases, model/reference standard deviation 	
; ratios and standard deviations.					
;										
; Initially written to work with output from ECMWF CDP verification scripts 	
; simulates and calculates scores upstream. Though as long as the scores follow 
; the same naming convention then the script should work.			
;										
; 			Dave MacLeod, University of Oxford, 15th April 2013	
;
; Overhauled to separate repeated information into setupInfo.ncl file
; 			DM 26 Mar 2015

;

load "setupInfo.ncl"
begin
figDir =figDir+"/ClimatologyMaps/"
system ("if [ ! -d "+figDir+\
" ]; then mkdir "+figDir+";fi")

do iExp 	=0,nExp-1 ; looping over experiments
 expID 			= expInfo(iExp,0)
 expType 		= expInfo(iExp,1)
 expLongName 		= expInfo(iExp,2)
 print("Plotting all climatology maps, experiment: "+expID)

 do iStart	= 1,1;nStarts-1 ; loopng over forecast start month
  do iPlotType  = 0,nClimPlotTypes-1; loopng over plot type
   do iVar	= 1,nVars-1 ; looping over variables

    ;----------------- setting up values with loop iterators -------------------
    initialisationMonth=initialisationMonthList(iStart)

    expIDLong = "C"+expType+"OecmfE"+expID+"S"+Sys+"M001_"\
    +hindcastPeriod+"_"+initialisationMonth+"0100_"

    ReadDataRootDirectory =dataRootDir+runName+"/"+expID+"/clim/"
    
    plotTypeName 	= climPlotInfo(iPlotType,0)
    plotTypeLongName 	= climPlotInfo(iPlotType,1)
    colorTable 		= climPlotInfo(iPlotType,2)
    
    varNumber		= varInfo(iVar,0)
    varLevel		= varInfo(iVar,1)
    varName		= varInfo(iVar,2)
    varUnit		= varInfo(iVar,3)
    varReference	= varInfo(iVar,4)
    
    ;------------------------- reading in file ------------------------
        filename  		=  ReadDataRootDirectory\
    +plotTypeName+"_"+expIDLong+\
    varNumber+"_"+varLevel+DataExtension

    if (.not.isfilepresent(filename)) then
     print("File missing, breaking loop. File is "+filename)
     continue
    end if

    fld			= getFile(filename)
    
    fld!0 = "time"
    fld!1 = "lat"
    fld!2 = "lon"
    fld			= scaleVar(fld,varNumber,plotTypeName)
    fld			= maskField(fld,varNumber,plotOceanSST)


     do iTarget = 0,0;nTargets-1 ; looping over forecast target
      targetStart = leadtimeInfo(iTarget,0)
      targetEnd = leadtimeInfo(iTarget,1)
      targetCode = leadtimeNames(iTarget,0)
      targetName = leadtimeNames(iTarget,iStart+1)
      if (expID.eq."ga9e") then
       fld_target=fld
      else
       fld_target = dim_avg_n_Wrap\
       (fld(targetStart:targetEnd,:,:),0)
      end if
      
      ;------------------------- Plotting -------------------------
      
      varNumberString = varNumber
      if (plotOceanSST.eq.1) then
       varNumberString=varNumberString+"OCE_"
      end if

      PlotName 	= plotTypeName+"_"+expID+"_"\
       		       +hindcastPeriod+"_"+initialisationMonth+"0100_"\
		       +varNumberString+"_"+varLevel+"_"+targetCode
      PlotPath 	= figDir+PlotName

      do iRegion=1,1;nRegions-1	; looping over regions
      		
       PlotPathReg			= PlotPath+RegionNames(iRegion)
       wks 				= gsn_open_wks(plotType,PlotPathReg)
       gsn_define_colormap(wks, colorTable)

       res 				= True ; setting resources
       res@cnFillOn 			= True
       res@cnInfoLabelOn 		= False
       res@cnLevelSelectionMode 	= "ExplicitLevels"
       res@cnLevels 			= climContourLevels(iVar,iPlotType,:)
       res@cnFillMode 			= "CellFill"



       ; For bias and sd ratio plots, include the lines 						either side of zero
       if (plotTypeName.eq."BIAS".or.plotTypeName.eq."SDRT") then
	res@cnLinesOn 		= True	
	res@cnLevelFlags		=\ 		
	       (/"NoLine","NoLine","NoLine","NoLine", \
	       "NoLine","LineOnly","LineOnly","NoLine",\
	       "NoLine","NoLine","NoLine","NoLine"/)
	res@cnLineThicknessF 	= 0.2
       else
	res@cnLinesOn 		= False
       end if

       if (iRegion.ne.0) then
	res@cnLinesOn 		= False
       end if

	if (plotTypeName.eq."SDRT") then
	      res@lbTitleString 		= ""
	end if
	; Reversing the colormap for  clim & bias plots for precipitation and soil moisture
	; since the maximum is wet (blue), compared to max temp, which is hot (red)		     
	if (iVar.eq.0.or.varNumber.eq.3) then
	if (plotTypeName.eq."BIAS".or.plotTypeName.eq."CLIM") then
	gsn_reverse_colormap(wks)
	end if
	end if

	res@gsnRightString 		= ""
	res@gsnLeftString 		= ""

	res@tmXBOn 			= False
	res@tmXTOn 			= False
	res@tmYLOn 			= False
	res@tmYROn 			= False
	res@mpOutlineBoundarySets="AllBoundaries"

	res@mpFillOn                    	= False
	res@mpGeophysicalLineThicknessF 	= 1

	; Resources to modify the labelbar		
	res@lbTitleString 		=  varInfo(iVar,3) 
	res@lbTitlePosition 		= "Bottom"
	res@lbTitleFontHeightF 		= 0.015
	res@pmLabelBarHeightF		= 0.1
	res@pmLabelBarWidthF 		= 0.812

	res@mpMinLatF			= LatLons(iRegion,0)
	res@mpMaxLatF			= LatLons(iRegion,1)
	res@mpMinLonF			= LatLons(iRegion,2)
	res@mpMaxLonF			= LatLons(iRegion,3)

	res@tiMainFontHeightF 		= 0.015

       ; Set up the title
       res@tiMainString 		= plotTypeLongName+": "+varName+"~C~"\
      					 +expID+", month "+\
      					 (targetStart+1)+"-"+(targetEnd+1)+\
      					 " ("+targetName+")"

       if (plotTypeName.eq."BIAS".or.plotTypeName.eq."SDRT") then
	res@tiMainString = res@tiMainString+", reference is "+varReference
       end if


       plot 				= gsn_csm_contour_map(wks,fld_target,res)
       
       if (verbose) then
        print("PlotPath is "+PlotPathReg)
       end if
       
       system("gv "+PlotPathReg+"&")
       exit
      end do ; region loop
     end do ; lead time targets loop

     delete(fld)
     delete(fld_target)

    end do; variable loop	
   end do ; plot type loop
 end do ; start date loop
end do ; experiment loop

end
