; This script plots difference maps for climatologies, biases, model/reference standard deviation ratios and standard deviations.					
;										
; initially written to work with output from ECMWF CDP verification scripts 	
; simulates and calculates scores upstream. Though as long as the scores follow 
; the same naming convention then the script should work.			
;		
; 			Dave MacLeod, University of Oxford, 15th April 2013	
;

; Note that this relies on a control experiment, which should be the first
; 3-dimension element in expInfo, defined in setupInfo.ncl					
;
; Overhauled to separate repeated information into setupInfo.ncl file
; 			DM 26 Mar 2015


load "setupInfo.ncl"
begin
figDir =figDir+"/ClimatologyMaps/"
system ("if [ ! -d "+figDir+\
" ]; then mkdir "+figDir+";fi")

do iStart	= 1,1;nStarts-1 ; loopng over forecast start month
 do iPlotType 	= 0,nClimPlotTypes-1; loopng over plot type
  print("plotting all climatology comparison maps, metric is:"+climPlotInfo(iPlotType,0))
  do iVar 	= 0,nVars-1 ; looping over variables
   do iTarget 	= 0,nTargets-1 ; looping over forecast target

    ;----------------- setting up values with loop iterators -------------------
    initialisationMonth=initialisationMonthList(iStart)

    
    plotTypeName 	= climPlotInfo(iPlotType,0)
    plotTypeLongName 	= climPlotInfo(iPlotType,1)
    colorTable 		= climPlotInfo(iPlotType,2)
    
    varNumber		= varInfo(iVar,0)
    varLevel		= varInfo(iVar,1)
    varName		= varInfo(iVar,2)
    varUnit		= varInfo(iVar,3)
    varReference	= varInfo(iVar,4)

    targetStart = leadtimeInfo(iTarget,0)
    targetEnd = leadtimeInfo(iTarget,1)
    targetCode = leadtimeNames(iTarget,0)
    targetName = leadtimeNames(iTarget,iStart+1)


; Looping over experiments 
    do iExp1 = 0,0 ; the control/reference is always the first
      do iExp2 = 0,nExp-1

       if (iExp1.eq.iExp2) then
	continue
       end if

       do comb=0,1
        if (comb.eq.0) then
	 iExp=iExp1
        else
	 iExp=iExp2
        end if

        expID 			= expInfo(iExp,0)
	expType 		= expInfo(iExp,1)
	expLongName 		= expInfo(iExp,2)
        expIDLong = "C"+expType+"OecmfE"+expID+"S"+Sys+"M001_"\
        +hindcastPeriod+"_"+initialisationMonth+"0100_"
        ReadDataRootDirectory =dataRootDir+runName+"/"+expID+"/clim/"
	
     
       ;------------------------- reading in file ------------------------
           filename  		=  ReadDataRootDirectory\
       +plotTypeName+"_"+expIDLong+\
       varNumber+"_"+varLevel+DataExtension

	 exitcode=0
	if (.not.isfilepresent(filename)) then
	exitcode=1
	print("File missing, breaking loop. File is "+filename)
	continue
       end if
 
       fld			= getFile(filename)
       if (expID.eq."ga9e") then
	fld!0 = "lat"
	fld!1 = "lon"
       else
	fld!0 = "time"
	fld!1 = "lat"
	fld!2 = "lon"
       end if
       
       fld			= scaleVar(fld,varNumber,plotTypeName)
     

      if (expID.eq."ga9e") then
       fld_target=fld
      else
       fld_target = dim_avg_n_Wrap\
       (fld(targetStart:targetEnd,:,:),0)
      end if
      


      if (comb.eq.0) then
       fld_0 = fld_target
      end if

      if (comb.eq.1) then
       fld_1 = fld_target
      end if

      delete(fld)
      delete(fld_target)

      if (comb.eq.0) then	
       expID_0 = expID
       plotName = plotTypeName+"_"+expID
      else
       expID_1 = expID
       plotName = plotName+"_"+expID+"_"
      end if

     end do ; combination loop

     if (exitcode.eq.1) then
      exitcode=0
      continue
     end if
 
     fld_target = fld_0

     if (iPlotType.le.1) then			
      fld_target = fld_1 - fld_0
     else
      fld_0=where(fld_0.eq.0,0.00001,fld_0)
      fld_target = fld_1/fld_0
     end if
 
      fld_target	= maskField(fld_target,varNumber,plotOceanSST)

      delete(fld_1)
      delete(fld_0)


      ;------------------------- plotting -------------------------
      
      varNumberString = varNumber
      if (plotOceanSST.eq.1) then
       varNumberString=varNumberString+"OCE_"
      end if

      plotName 		= plotName+hindcastPeriod+"_"+\
      			initialisationMonth+"0100_"\
      			+varNumberString+"_"+varLevel+"_"+targetCode
      plotPath 		= figDir+plotName

      do iRegion=0,0;nRegions-1 ;region loop
       colorTable="BlueWhiteOrangeRed"
       plotPathReg			= plotPath+RegionNames(iRegion)
       wks 				= gsn_open_wks(plotType,plotPathReg)
       gsn_define_colormap(wks, colorTable)
		

       ; Reversing the colormap for  clim & bias plots for precipitation and soil moisture
       ; since the maximum is wet (blue), compared to max temp, which is hot (red)		     
       if (varNumber.eq."228".or.varNumber.eq."039") then
	if (iPlotType.le.1) then
         gsn_reverse_colormap(wks)
	 end if
       end if

       res 				= True
       res@cnFillOn 			= True
       res@cnInfoLabelOn 		= False
       res@cnLevelSelectionMode 		= "ExplicitLevels"
       res@cnLevels 			=\
        climCompContourLevels*climCompContourLevelScaling(iVar)
       res@cnLinesOn 			= True	
       res@cnLineThicknessF 		= 0.2
       res@cnFillMode 			= "CellFill"
       
       res@cnLevelFlags			=\ 		
      				 (/"NoLine","NoLine","NoLine","NoLine", \
      				 "NoLine","LineOnly","LineOnly","NoLine",\
      				 "NoLine","NoLine","NoLine","NoLine"/)

       if (iRegion.ne.0) then
	       res@cnLinesOn 		= False
       end if
      
       res@gsnRightString 		= ""
       res@gsnLeftString 		= ""

       res@tmXBOn 			= False
       res@tmXTOn 			= False
       res@tmYLOn 			= False
       res@tmYROn 			= False

       res@mpFillOn     			= False
       res@mpGeophysicalLineThicknessF 	= 1

       res@tiMainFontHeightF 		= 0.015



       if (iPlotType.le.1) then ; diff titles, dep. if the field is a diff or ratio
       res@tiMainString 			= plotTypeLongName+" difference: "\
      					 +varName+"~C~"\
      					 +expID_1+" minus "+expID_0+", month "+\
      					 (targetStart+1)+"-"+(targetEnd+1)+\
      					 " ("+targetName+"), reference is "+varReference
       else
	res@cnLevels 			= (/0,0.2,0.4,0.6,0.8,0.9,1.1,1.2,1.5,2,4,8/)
       res@tiMainString 			= plotTypeLongName+" ratio: "\
      					 +varName+"~C~"\
      					 +expID_1+" divided by "+expID_0+", month "+\
      					 (targetStart+1)+"-"+(targetEnd+1)+\
      					 " ("+targetName+"), reference is "+varReference

       end if

       ; Resources to modify the labelbar		
       res@lbTitleString 		= varUnit
       res@lbTitlePosition 		= "Bottom"
       res@lbTitleFontHeightF 		= 0.015
       res@pmLabelBarHeightF 		= 0.1
       res@pmLabelBarWidthF 		= 0.812

       if (iPlotType.ge.2) then
	res@lbTitleString 		= ""
       end if

       res@mpMinLatF			= LatLons(iRegion,0)
       res@mpMaxLatF			= LatLons(iRegion,1)
       res@mpMinLonF			= LatLons(iRegion,2)
       res@mpMaxLonF			= LatLons(iRegion,3)

       plot = gsn_csm_contour_map(wks,fld_target,res)

       if (verbose) then
        print("plotPath is "+plotPathReg)
       end if
       ;system("gv "+plotPathReg+"&")
       ;exit

       end do ; Region loop
       delete(fld_target)

     end do ; exp 2 loop
    end do ; exp 1 loop
   end do ; Lead time targets loop
  end do ; Start date loop
 end do ; variable loop
end do ; plot type loop

end
