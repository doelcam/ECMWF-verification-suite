; This script plots differences between probabilistic maps				
;										
; Initially written to work with output from ECMWF CDP verification scripts 	
; simulates and calculates scores upstream. Though as long as the scores follow 
; the same naming convention then the script should work.			
;										
; 			Dave MacLeod, University of Oxford, 15th April 2013	
;
; Note that this relies on a control experiment, which should be the first
; 3-dimension element in expInfo, defined in setupInfo.ncl				;
; Overhauled to separate repeated information into setupInfo.ncl file
; 			DM 26 Mar 2015

load "setupInfo.ncl"
begin
figDir =figDir+"/ProbabilisticMaps/"
system ("if [ ! -d "+figDir+\
" ]; then mkdir "+figDir+";fi")

do iStart	= 1,1;nStarts-1 ; loopng over forecast start month
 do iPlotType 	= 0,nProbPlotTypes-1; loopng over plot type
  print("Plotting all probabilistic maps for score "+probPlotInfo(iPlotType,0))
  do iVar 	= 0,nVars-1 ; looping over variables
   do iEvent 	= 0,nEvents-1 ; looping over forecast Event
    do iTarget 	= 0,nTargets-1 ; looping over forecast target

    ;----------------- setting up values with loop iterators -------------------
    initialisationMonth=initialisationMonthList(iStart)

    plotTypeName 	= probPlotInfo(iPlotType,0)
    plotTypeLongName 	= probPlotInfo(iPlotType,1)
    colorTable 		= probPlotInfo(iPlotType,2)

    varNumber		= varInfo(iVar,0)
    varLevel		= varInfo(iVar,1)
    varName		= varInfo(iVar,2)
    varUnit		= varInfo(iVar,3)
    varReference	= varInfo(iVar,4)

    targetStart = leadtimeInfo(iTarget,0)
    targetEnd = leadtimeInfo(iTarget,1)
    targetCode = leadtimeNames(iTarget,0)
    targetName = leadtimeNames(iTarget,iStart+1)
    
    eventName 		= eventInfo(iEvent,0)
    event 		= eventInfo(iEvent,1)
    eventPercentile 	= eventPercentileList(iEvent)

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

        ;------------------------- reading in file ------------------------
        expID 			= expInfo(iExp,0)
	expType 		= expInfo(iExp,1)
	expLongName 		= expInfo(iExp,2)
        expIDLong = "C"+expType+"OecmfE"+expID+"S"+Sys+"M001_"\
        +hindcastPeriod+"_"+initialisationMonth+"0100_CV01_I0_"

        ReadDataRootDirectory =dataRootDir+runName+"/"+expID+"/rel/"

        filenameroot  		=  ReadDataRootDirectory+plotTypeName\
      				+"_"+expIDLong+varNumber+"_"+varLevel+\
				"_T"+event+"_M1_B10_"+targetCode
        filename 			= filenameroot+DataExtension


	exitcode=0
	if (.not.isfilepresent(filename)) then
	exitcode=1
	print("File missing, breaking loop. File is "+filename)
	continue
       end if

       fld			= getFile(filename)
       fld!0 = "lat"
       fld!1 = "lon"
       fld			= maskField(fld,varNumber,plotOceanSST)
     
     
       if (comb.eq.0) then
	fld_0 = fld
       end if

       if (comb.eq.1) then
	fld_1 = fld
       end if

       delete(fld)

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

; Taking the difference
      fld = fld_0
      fld = fld_1 - fld_0

      fld_target	= maskField(fld,iVar,plotOceanSST)
      
      

      ;------------------------- plotting -------------------------
      
      varNumberString = varNumber
      if (plotOceanSST.eq.1) then
       varNumberString=varNumberString+"OCE_"
      end if


      plotName 		= plotName+hindcastPeriod+"_"+\
      			initialisationMonth+"0100_"\
      			+varNumberString+"_"+varLevel+"_"+event+"_"+targetCode
  

      PlotPath 			        = figDir+plotName
      
      colorTable="BlueWhiteOrangeRed"

      res 			        = True
      res@gsnDraw 		        = False
      res@gsnFrame 		        = False
      res@gsnRightString 	        = ""
      res@gsnLeftString		        = ""

      res@cnFillOn 		        = True
      res@cnInfoLabelOn 	        = False
      res@cnLevelSelectionMode 	        = "ExplicitLevels"
      res@cnLevels 		        = probContourLevels(iPlotType,:)
      res@cnLevels 		        = (/-0.5,-0.4,-0.3,-0.2,-0.1,0.0,0.1,0.2,0.3,0.4,0.5/)
      
      res@cnLinesOn 		        = False
      res@cnFillMode 		        = "CellFill"


      res@tmXBOn 		        = False
      res@tmXTOn 		        = False
      res@tmYLOn 		        = False
      res@tmYROn 		        = False

      res@mpFillOn                    	= False
      res@mpGeophysicalLineThicknessF 	= 1

      res@tiMainFontHeightF 		= 0.015

      res@tiMainString 			= plotTypeLongName+" difference: "\
      					 +varName+"~C~"\
					 +expID_1+" minus "+expID_0+", month "+\
      					 (targetStart+1)+"-"+(targetEnd+1)+\
      					 " ("+targetName+")"

      ; Resources to modify the labelbar		
      res@lbTitlePosition 		= "Bottom"
      res@lbTitleFontHeightF 		= 0.015
      res@pmLabelBarHeightF 		= 0.1
      res@pmLabelBarWidthF 		= 0.812
      res@gsnAddCyclic			=False

;_________________________
; Region loop


      do iRegion=0,0;nRegions-1
       PlotPathReg			=PlotPath+RegionNames(iRegion)
       res@mpMinLatF			=LatLons(iRegion,0)
       res@mpMaxLatF			=LatLons(iRegion,1)
       res@mpMinLonF			=LatLons(iRegion,2)
       res@mpMaxLonF			=LatLons(iRegion,3)
       wks 				= gsn_open_wks(plotType,PlotPathReg)
       gsn_define_colormap(wks, colorTable)
       plot 				= gsn_csm_contour_map(wks,fld,res)

       ; Resources to modify the stippling
       res2 				= True  

       res2@gsnDraw             	= False  
       res2@gsnFrame            	= False  
       res2@gsnRightString 		= ""
       res2@gsnLeftString 		= ""
       res2@gsnAddCyclic 		= True
       res2@cnLevelSelectionMode 	= "ManualLevels" 
       res2@cnMinLevelValF      	= 0.00  
       res2@cnMaxLevelValF      	= 1.05  
       res2@cnLevelSpacingF     	= 0.05  

       res2@cnInfoLabelOn       	= False  
       res2@cnLinesOn           	= False  
       res2@cnLineLabelsOn      	= False  

       res2@cnFillScaleF        	= 0.6 
       ;res2@mpMinLatF=LatLons(iRegion,0)
       ;res2@mpMaxLatF=LatLons(iRegion,1)
       ;res2@mpMinLonF=LatLons(iRegion,2)
       ;res2@mpMaxLonF=LatLons(iRegion,3)

       draw(plot)
       frame(wks)

      end do ; region loop
      
      if (verbose) then
       print("PlotPath is "+PlotPathReg)
      end if
 ;     system("gv "+PlotPath+"&")
 ;     exit

      delete(fld)

      end do ; exp 2 loop
     end do ; exp 1 loop
    end do ; Lead time targets loop
   end do ; Events loop
  end do ; Start date loop
 end do ; variable loop
end do ; plot type loop
end
