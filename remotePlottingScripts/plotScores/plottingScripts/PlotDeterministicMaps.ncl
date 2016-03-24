; This script plots deterministic maps				
;										
; Initially written to work with output from ECMWF CDP verification scripts 	
; simulates and calculates scores upstream. Though as long as the scores follow 
; the same naming convention then the script should work.			
;										
; 			Dave MacLeod, University of Oxford, 15th April 2013	
;
; Overhauled to separate repeated information into setupInfo.ncl file
; 			DM 26 Mar 2015

load "setupInfo.ncl"
begin
figDir =figDir+"/DeterministicMaps/"
system ("if [ ! -d "+figDir+\
" ]; then mkdir "+figDir+";fi")

do iExp 	= 0,nExp-1 ; looping over experiments
 expID 			= expInfo(iExp,0)
 expType 		= expInfo(iExp,1)
 expLongName 		= expInfo(iExp,2)
 			print("Plotting all deterministic maps, experiment: "+expID)
 do iStart	= 1,1;nStarts-1 ; loopng over forecast start month
  do iPlotType 	= 0,nDetPlotTypes-1; loopng over plot type
  
;  if (iPlotType.eq.1) then
;   continue
;  end if
  
   			print("Plotting all maps for score "+detPlotInfo(iPlotType,0))
   do iVar 	= 0,nVars-1 ; looping over variables
    do iTarget	= 0,nTargets-1 ; looping over forecast target

     ;----------------- setting up values with loop iterators -------------------

    ReadDataRootDirectory =dataRootDir+runName+"/"+expID+"/acc/"
     initialisationMonth=initialisationMonthList(iStart)
     expIDLong = "C"+expType+"OecmfE"+expID+"S"+Sys+"M001_"\
   +hindcastPeriod+"_"+initialisationMonth+"0100_CV01_I0_"

     plotTypeName 	= detPlotInfo(iPlotType,0)
     plotTypeLongName 	= detPlotInfo(iPlotType,1)
     colorTable 	= detPlotInfo(iPlotType,2)

     varNumber		= varInfo(iVar,0)
     varLevel		= varInfo(iVar,1)
     varName		= varInfo(iVar,2)
     varUnit		= varInfo(iVar,3)
     varReference	= varInfo(iVar,4)
    
     targetStart 	= leadtimeInfo(iTarget,0)
     targetEnd 		= leadtimeInfo(iTarget,1)
     targetCode 	= leadtimeNames(iTarget,0)
     targetName 	= leadtimeNames(iTarget,iStart+1)



    ;------------------------- reading in file ------------------------
     filenameroot  	=  ReadDataRootDirectory\
     				+plotTypeName+"_"+expIDLong+\
     				varNumber+"_"+varLevel+"_"+targetCode\
				
     filename		= filenameroot+DataExtension
     if (.not.isfilepresent(filename)) then
      print("File missing, breaking loop. File is "+filename)
      continue
     end if

     fld			= getFile(filename)
     fld!0 = "lat"
     fld!1 = "lon"
     fld			= maskField(fld,varNumber,plotOceanSST)
 

     if (iPlotType.ne.1.and.iPlotType.ne.4) then ; Perfect model acc and Normalised spread have no bootstrapping data
       bootstrap=True
     else
       bootstrap=False
     end if
     fld_boot=loadBootstrap(filenameroot,bootstrap)
     fld = maskField(fld,iVar,plotOceanSST)
     
     if (bootstrap) then
      fld_boot = maskField(fld_boot,iVar,plotOceanSST)
     end if
     
     
     
     ;------------------------- Plotting -------------------------
     varNumberString = varNumber
     if (plotOceanSST.eq.1) then
      varNumberString=varNumberString+"OCE_"
     end if

     PlotName 			= plotTypeName+"_"+expID+\
     				"_"+hindcastPeriod+"_"+initialisationMonth+"0100_"\
     				+varNumberString+"_"+varLevel+"_"+targetCode
     PlotPath 			= figDir+PlotName

     do iRegion=0,0;nRegions-1 ; region loop
      PlotPathReg			= PlotPath+RegionNames(iRegion)
      wks 				= gsn_open_wks(plotType,PlotPathReg)
      gsn_define_colormap(wks, colorTable)

      res 				= True
      
      res@gsnDraw 			= False
      res@gsnFrame 			= False
      res@gsnRightString 		= ""
      res@gsnLeftString 		= ""

      res@mpMinLatF			= LatLons(iRegion,0)
      res@mpMaxLatF			= LatLons(iRegion,1)
      res@mpMinLonF			= LatLons(iRegion,2)
      res@mpMaxLonF			= LatLons(iRegion,3)
      res@mpFillOn                  	= False
      res@mpGeophysicalLineThicknessF 	= 1
      
      res@mpOutlineBoundarySets="AllBoundaries"
      res@cnFillOn 			= True
      res@cnInfoLabelOn 		= False
      res@cnLevelSelectionMode 		= "ExplicitLevels"
      res@cnLevels 			= detContourLevels(iPlotType,:)
      res@cnLinesOn 			= False
      res@cnFillMode 			= "CellFill"
 
      res@tmXBOn 			= False
      res@tmXTOn 			= False
      res@tmYLOn 			= False
      res@tmYROn 			= False


      res@tiMainFontHeightF 		= 0.015
      res@tiMainString 			= plotTypeLongName+": "+varName+" ~C~"\
      					+expID+", month "+\
      					(targetStart+1)+"-"+(targetEnd+1)+\
      					" ("+targetName+")"

      if (iPlotType.eq.0.or.iPlotType.eq.2) then
       fld_boot				= where(fld.ge.0,fld_boot,0)
       res@tiMainString 		= res@tiMainString+", reference is "\
       +varReference+\
       "~C~Stippled area indicates where score is greater than 0 at 95% confidence"
      end if

      if (iPlotType.eq.3) then
       fld_boot				= where(fld.ge.0,fld_boot,0)
       res@tiMainString 		= res@tiMainString+", reference is"\
       +varReference+\
       "~C~Stippled area indicates where score is different from 1 at 95% confidence"
      end if

      ; For acp & normalised spread no need to include the reference in the title
      if (iPlotType.eq.1.or.iPlotType.eq.4) then
       res@tiMainString 		= varName+" "+plotTypeLongName+" ("+expID+\
      					")~C~FC period: month "+(targetStart+1)\
					+"-"+(targetEnd+1)+" ("+targetName+")"
      end if

      ; Resources to modify the labelbar		
      ;res@lbTitleString 		= Unit
      res@lbTitlePosition 		= "Bottom"
      res@lbTitleFontHeightF 		= 0.015
      res@pmLabelBarHeightF 		= 0.1
      res@pmLabelBarWidthF 		= 0.812
      plot 				= gsn_csm_contour_map(wks,fld,res)

      ; Resources to modify the stippling
      res2 				= True  

      res2@gsnDraw             		= False  
      res2@gsnFrame            		= False  
      res2@gsnRightString 		= ""
      res2@gsnLeftString 		= ""
      res2@gsnAddCyclic 		= True
      res2@cnLevelSelectionMode 	= "ManualLevels" 
      res2@cnMinLevelValF      		= 0.00  
      res2@cnMaxLevelValF      		= 1.05  
      res2@cnLevelSpacingF     		= 0.05  
      res2@cnInfoLabelOn       		= False  
      res2@cnLinesOn           		= False  
      res2@cnLineLabelsOn      		= False  
      res2@cnFillScaleF        		= 0.6 

      if (bootstrap) then
       plot2   				= gsn_csm_contour(wks,fld_boot,res2) 
       plot2   				= ShadeGtContour(plot2, 0.5, 17)
      overlay (plot, plot2)
      end if
      
      draw(plot)
      frame(wks)

     end do ; region loop
     
     if (verbose) then
      print("PlotPath is "+PlotPathReg)
     end if 
     
     system("gv "+PlotPathReg+"&")
     exit
    
     delete(fld)
     delete(fld_boot)

;StoreDataPath 			="/home/cirrus/pred/macleod/SoilMoistureSystem4/ACC_"+expID+"_228_"+TargetCode+"_1981-2012.nc"
;system ("rm "+StoreDataPath)
;write_file             	 	= addfile(StoreDataPath,"c")
;write_file->ACC    		= fld_target
;print("Wrote file "+StoreDataPath)



    end do ; Lead time targets loop
   end do ; Variable loop
  end do ; Plot type loop
 end do ; Start month loop
end do ; Experiment loop

end
