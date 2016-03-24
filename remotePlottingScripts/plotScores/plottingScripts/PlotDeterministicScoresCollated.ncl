; initially written to work with output from ECMWF CDP verification scripts 	;
; However the output comes in ASCII format so it is necessary to carefully	;
; read it in (including the nBoot bootsstrap samples). For this purpose a 	;
; function "read_reliability_ascii" has been written, which must be loaded	;
; (hopefully it is in "function_ReadReliabilityDataFromAscii.ncl" above!)	;
; 										;
; Be careful using this function, it is tested for this purpose but if the 	;
; format of the ascii file changes then it may not work. Also N.B. the 		;
; arguments of the function (should be self-explanatory from their names)	;
;										;
;										
; 			Dave MacLeod, University of Oxford, 15th April 2013	
;
; Overhauled to separate repeated information into setupInfo.ncl file
; 			DM 26 Mar 2015

load "setupInfo.ncl"
begin
figDir =figDir+"/DeterministicScores/"
system ("if [ ! -d "+figDir+\
" ]; then mkdir "+figDir+";fi")


collectionName="paper"
expIndexToPlot=(/0,1/) ; choose exp from setupInfo.ncl
nExp=dimsizes(expIndexToPlot)

figDir =figDir+collectionName+"/"
system ("if [ ! -d "+figDir+\
" ]; then mkdir "+figDir+";fi")

do iPlotType = 0,nDetPlotTypes-1 ; Looping over plot type
print("Plotting all deterministic scores for score "+detPlotInfo(iPlotType,0))
 do iRegion=0,nRegions-1 ; Looping over regions
  do iVar =0,nVars-1 ; Looping over variables
   ValuesToPlot = new((/nExp,nTargets,3/),float)
   ; 3 are the mean, min and max to plot
   do iStart=1,1;0,nStarts-1; Looping over forecast start month 
    do iExp = 0,nExp-1 ; Looping over experiments 
     do iTarget=0,nTargets-1; Looping over forecast target

    ;----------------- setting up values with loop iterators -------------------
      initialisationMonth	=initialisationMonthList(iStart)
      expIndex			= expIndexToPlot(iExp)
      expID 			= expInfo(expIndex,0)
      expType 			= expInfo(expIndex,1)
      expLongName 		= expInfo(expIndex,2)


      plotTypeName 		= detPlotInfo(iPlotType,0)
      plotTypeLongName 		= detPlotInfo(iPlotType,1)
      colorTable 		= detPlotInfo(iPlotType,2)

      varNumber			= varInfo(iVar,0)
      varLevel			= varInfo(iVar,1)
      varName			= varInfo(iVar,2)
      varUnit			= varInfo(iVar,3)
      varReference		= varInfo(iVar,4)

      targetStart 		= leadtimeInfo(iTarget,0)
      targetEnd 		= leadtimeInfo(iTarget,1)
      targetCode		= leadtimeNames(iTarget,0)
      targetName 		= leadtimeNames(iTarget,iStart+1)
      regionCode 		= regionData(iRegion,0)
      regionName 		= regionData(iRegion,1)
      
 
       ;------------------------- reading in file ------------------------
 
      ReadDataRootDirectory =dataRootDir+runName+"/"+expID+"/acc/"
      expIDLong 		= "C"+expType+"OecmfE"+expID+"S"+Sys+"M001_"\
      +hindcastPeriod+"_"+initialisationMonth+"0100_CV01_I0_"

      filename  		=  ReadDataRootDirectory+plotTypeName\
      +"_"+expIDLong+varNumber+"_"+\
      varLevel+"_"+targetCode\
      +"_"+regionCode
      if (.not.fileexists(filename)) then
       exitcode=1
       print("File missing, breaking loop. File is "+filename)
       continue
      end if

      ; Different ascii format for the different score
      if (iPlotType.le.2) then
       ValuesToPlot(iExp,iTarget,:) = read_det_scores_ascii(filename,\
      				yIni,yFin,nBins,nBoot,nMembers,SigLvl)
      else
       ValuesToPlot(iExp,iTarget,:) = read_det_ens_scores_ascii(filename,\
      				yIni,yFin,nBins,nBoot,nMembers,SigLvl)
      end if
     
     end do ; Target Loop
    end do	; experiment loop			


      ;------------------------- plotting -------------------------
      


    plotType 		= "ps"
    PlotName 		= plotTypeName+"_"+collectionName+"_"
    PlotName 		= PlotName+hindcastPeriod+"_"+initialisationMonth+"0100_"\
    			+varNumber+"_"+varLevel+"_"+regionCode
    PlotPath 		= figDir+PlotName
    print("Plot path is "+PlotPath)
    wks = gsn_open_wks(plotType,PlotPath)

    res 				= True
    res@gsnDraw 			= False
    res@gsnFrame 			= False

    ; Defining axes, different for each index
    MinYs 				= (/-1,-1,-1,0,0/)
    MaxYs 				= (/1,1,1,2,2/)		
    YRefLineValues 			= (/0,0,0,1,1/)		

    res@trYMinF 			= MinYs(iPlotType)
    res@trYMaxF 			= MaxYs(iPlotType)
    res@gsnYRefLine 			= YRefLineValues(iPlotType)
    res@gsnYRefLineDashPattern 		= 1
    res@gsnYRefLineColor 		= "grey"

    dx_small 				= 0.2 ; distance between experiment plot,
    						; same lead time
    dx_large 				= 0.4 ; distance between different leads
    to_edge 				= 0.5 ; distance between first/last plot 
	    					;and edgeof axis
    x_ini = to_edge

    res@trXMinF 			= x_ini-to_edge
    res@trXMaxF 			= x_ini+(dx_small*(nExp-1)*nTargets)+\
    					dx_large*(nTargets-1)+to_edge

    res@tmYLMode 			= "Explicit"
    res@tmYLValues 			= fspan(res@trYMinF,res@trYMaxF,11) 
    res@tmYLLabels 			= fspan(res@trYMinF,res@trYMaxF,11) 

    res@tiXAxisString 			= "Forecast target"
    res@tiXAxisFontHeightF 		= 0.015

    res@tmXBMode			=  "Explicit"
    res@tmXBValues 			= fspan(x_ini+(dx_small*(nExp-1)/2)\
    			,res@trXMaxF-to_edge-(dx_small*(nExp-1)/2),nTargets)
    res@tmXBLabels 			= leadtimeNames(:,iStart+1)

    res@tiYAxisFontHeightF 		= 0.015

    res@xyLineThicknessF 		= 1.5
    res@xyLineColor 			= "red"
    res@xyMarkLineMode 			= "Markers"
    res@xyMarker 			= 1
    res@xyMarkerSizeF 			= 0.04
    
    res@tmXTOn 				= False
    res@tmYROn 				= False
    res@tmXBMinorOn 			= False
    res@tmYLMinorOn 			= False 
    res@tmXBMajorLengthF 		= 0.005
    res@tmYLMajorLengthF 		= 0.005
    res@tmXBLabelFontHeightF 		= 0.015
    res@tmYLLabelFontHeightF 		= 0.015

    res@tiMainFontHeightF 		= 0.015
    res@tiMainString 			= plotTypeLongName+\
    " ~C~"+varName+\
    " over "+regionName+"~C~Reference is "+varReference+"~C~Bars are "+SigLvl+\
    " confidence intervals computed with "+nBoot+" samples"

    ;creating a dummy plot
    plot 			= gsn_csm_xy(wks,(/-1000,-10000/),(/0,1/),res)
    Colors 			= 							(/"red3","green2","dodgerblue3","blue3","midnightblue","purple"/)

    marker_res				= True
    marker_res@gsMarkerIndex 		= 16
    marker_res@gsMarkerSizeF		= 0.015
    marker_res@gsMarkerThicknessF	= 0.2
    dot_res				= True
    dot_res@gsMarkerColor 		= "black"
    dot_res@gsMarkerSizeF		= 0.02
    dot_res@gsMarkerIndex		= 1
    error_res 				= True
    error_res@gsMarkerIndex 		= 1
    error_res@gsLineThicknessF 		= 10.0
    error_res@gsLineOpacityF 		= 0.6

    error_bar 				= new((/nExp*nTargets/),graphic)
    markers 				= new((/nExp*nTargets/),graphic)

    ; Looping to add all the markers and error bars
    do iTarget = 0,nTargets-1
     do iExp=0,nExp-1
     
      xPos 				= x_ini+iExp*dx_small\
      					+iTarget*(dx_large+dx_small*(nExp-1))
      marker_res@gsMarkerColor 		= Colors(iExp)
      error_res@gsLineColor 		= Colors(iExp)

      ; Plotting error bars
      error_bar(iExp+nExp*iTarget)	= gsn_add_polyline\
     					(wks,plot,(/xPos,xPos/),\
      					(/ValuesToPlot(iExp,iTarget,1),\
      					ValuesToPlot(iExp,iTarget,2)/),error_res)
      ; Plotting markers
      markers(iExp+nExp*iTarget)	= gsn_add_polymarker\
      					(wks,plot,xPos,\
      					ValuesToPlot(iExp,iTarget,0),marker_res)
     end do
    end do

    ; Add legend text
    text_x 				= res@trXMinF+(res@trXMaxF-res@trXMinF)*0.95
    text_y 				= res@trYMinF+(res@trYMaxF-res@trYMinF)*0.35
    delta_y 				= -(res@trYMaxF-res@trYMinF)*0.06

    text_res 				= True
    text_res@txFontHeightF 		= 0.015
    text_res@txJust 			= "TopRight"

    do iExp=0,nExp-1
     text_res@txFontColor 		= Colors(iExp)
     gsn_text(wks,plot,expInfo(expIndexToPlot(iExp),2),text_x,text_y+iExp*delta_y,text_res)
    end do		

    draw(plot)
    frame(wks)
    
    if (verbose) then
     print(""+PlotPath)
    end if
    
    system("gv "+PlotPath+"&")
    exit

    delete(ValuesToPlot)
   end do ; Start month loop
  end do ; Variable loop
 end do ; region loop
end do ; Plot type loop


end
