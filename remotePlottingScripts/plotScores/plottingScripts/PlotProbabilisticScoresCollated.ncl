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
; Variables coded here are for 2mT, P, Z500, T850 and surface temperature. 	;
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


do iRegion=0,nRegions-1; region loop
 do iVar =0,nVars-1 ; variable loop
  do iStart=1,1;0,nStarts-1; Looping over forecast start month
   do iEvent = 0,nEvents-1 ; event loop
    ;; Each plot combines information from experiments, and lead times - 
    ; this double loop collects everything in one array for plotting
    ValuesToPlot = new((/nExp,nTargets,3/),float)
    ; 3 are the mean, min and max to plot
    ProbScores = new((/nExp,nTargets,3,15/),float) 

    do iExp = 0,nExp-1; Looping over experiments 
     do iTarget=0,nTargets-1 ; Looping over forecast target

    ;----------------- setting up values with loop iterators -------------------
      initialisationMonth	=initialisationMonthList(iStart)
      
      expIndex			= expIndexToPlot(iExp)
      expID 			= expInfo(expIndex,0)
      expType 			= expInfo(expIndex,1)
      expLongName 		= expInfo(expIndex,2)



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
      

      eventName 		= eventInfo(iEvent,0)
      event 			= eventInfo(iEvent,1)
      eventPercentile 		= eventPercentileList(iEvent)


      ;------------------------- reading in file ------------------------
 
      ReadDataRootDirectory =dataRootDir+runName+"/"+expID+"/rel/"
      
      expIDLong 		= "C"+expType+"OecmfE"+expID+"S"+Sys+"M001_"\
      +hindcastPeriod+"_"+initialisationMonth+"0100_CV01_I0_"
 
      filename  =  ReadDataRootDirectory+"BRV_"+expIDLong+varNumber+"_"+\
      varLevel+"_T"+event+"_M1_B10_"+targetCode\
      +"_"+regionCode

     if (.not.fileexists(filename)) then
      print("File missing, breaking loop. File is "+filename)
      continue
     end if

      ProbScores(iExp,iTarget,:,:) =\
      read_prob_scores_ascii(filename,yIni,yFin,nBins,nBoot,nMembers,SigLvl)
     end do ; Target Loop
    end do	; Experiment Loop			

    do iMetric = 0,nProbPlotTypes-1	 ; looping over plot type
     MetricIndex = MetricIndexList(iMetric)
     MetricShortName =probPlotInfo(iMetric,0) 
     MetricLongName = probPlotInfo(iMetric,1)
     
     ValuesToPlot(:,:,0) = ProbScores(:,:,0,MetricIndex)
     ValuesToPlot(:,:,1) = ProbScores(:,:,1,MetricIndex)
     ValuesToPlot(:,:,2) = ProbScores(:,:,2,MetricIndex)




      ;------------------------- plotting ------------------------

     PlotName 			= MetricShortName+"_"+collectionName+"_"
     PlotName 			= PlotName+hindcastPeriod+\
     			       "_"+initialisationMonth+"0100_"\
      			       +varNumber+"_"+varLevel+"_"+event+"_"+regionCode
     PlotPath 			= figDir+PlotName

     wks 			= gsn_open_wks(plotType,PlotPath)

     res 			= True
     res@gsnDraw 		= False
     res@gsnFrame 		= False

     ; Defining axes, different for each index
     MinYs			= (/-1,-1,-1,0.5,0,-1,0,-1/)
     MaxYs 			= (/1,1,1,1,1,1,0.015,1/)		
     YRefLineValues 		= (/0,0,0,-999,-999,0,-999,0/)		

     res@trYMinF 		= MinYs(iMetric)
     res@trYMaxF 		= MaxYs(iMetric)
     res@gsnYRefLine 		= YRefLineValues(iMetric)
     res@gsnYRefLineDashPattern = 1
     res@gsnYRefLineColor 	= "grey"
     ;res@gsnXYBarChart 	= True

     dx_small 			= 0.2 ; distance between exp plot same lead time
     dx_large 			= 0.4 ; distance between different leads
     to_edge 			= 0.5 ; d.b. first/last plot and edgeof axis
     x_ini 			= to_edge

     res@trXMinF 		= x_ini-to_edge
     res@trXMaxF 		= x_ini+(dx_small*(nExp-1)*nTargets)+\
      			       dx_large*(nTargets-1)+to_edge

     res@tmYLMode 		= "Explicit"
     res@tmYLValues 		= fspan(res@trYMinF,res@trYMaxF,11) 
     res@tmYLLabels 		= fspan(res@trYMinF,res@trYMaxF,11) 

     res@tiXAxisString 	= "Forecast target"
     res@tiXAxisFontHeightF 	= 0.015

     res@tmXBMode		=  "Explicit"
     res@tmXBValues 		= fspan(x_ini+(dx_small*(nExp-1)/2)\
      		       ,res@trXMaxF-to_edge-(dx_small*(nExp-1)/2),nTargets)

     res@tmXBLabels 		= leadtimeNames(:,iStart+1)

     res@tiYAxisFontHeightF 	= 0.015

     res@xyLineThicknessF 	= 1.5
     res@xyLineColor 		= "red"

     res@xyMarkLineMode 	= "Markers"
     res@xyMarker 		= 1
     res@xyMarkerSizeF 		= 0.04
     res@tmXTOn 		= False
     res@tmYROn 		= False
     res@tmXBMinorOn 		= False
     res@tmYLMinorOn 		= False 
     res@tmXBMajorLengthF 	= 0.005
     res@tmYLMajorLengthF 	= 0.005
     res@tmXBLabelFontHeightF 	= 0.015
     res@tmYLLabelFontHeightF 	= 0.015

     res@tiMainFontHeightF 	= 0.015
     res@tiMainString 		= MetricLongName+\
      		       "~C~"+eventName+" "+varName+\
      		       " over "+regionName+"~C~Reference is "+varReference+\
      		       "~C~Bars are "+SigLvl+" confidence intervals computed with "\
      		       +nBoot+" samples"


     ;creating a dummy plot
     plot 		= gsn_csm_xy(wks,(/-1000,-10000/),(/0,1/),res)
     Colors = (/"red3","green2","dodgerblue3","blue3","midnightblue","purple"/);,"green4","black"/)
     marker_res		= True
     marker_res@gsMarkerIndex 	= 16
     marker_res@gsMarkerSizeF	=0.015
     marker_res@gsMarkerThicknessF=0.2
     dot_res			= True
     dot_res@gsMarkerColor 	= "black"
     dot_res@gsMarkerSizeF	=0.02
     dot_res@gsMarkerIndex	=1
     error_res			= True
     error_res@gsMarkerIndex 	= 1
     error_res@gsLineThicknessF = 10.0
     error_res@gsLineOpacityF 	= 0.6

     error_bar 		= new((/nExp*nTargets/),graphic)
     markers 			= new((/nExp*nTargets/),graphic)
     ; Looping to add all the markers and error bars
     do iTarget = 0,nTargets-1

      do iExp=0,nExp-1

       marker_res@gsMarkerColor	= Colors(iExp)
       error_res@gsLineColor 		= Colors(iExp)

       xPos 				= x_ini+iExp*dx_small\
			       +iTarget*(dx_large+dx_small*(nExp-1))

       ; Plotting error bars
       error_bar(iExp+nExp*iTarget)	= gsn_add_polyline\
		       (wks,plot,(/xPos,xPos/),\
       (/ValuesToPlot(iExp,iTarget,1),ValuesToPlot(iExp,iTarget,2)/),error_res)

       ; Plotting markers
       markers(iExp+nExp*iTarget)	=gsn_add_polymarker\
       (wks,plot,xPos,ValuesToPlot(iExp,iTarget,0),marker_res)
      end do
     end do

     ; Add legend text
     text_x 			= res@trXMinF+(res@trXMaxF-res@trXMinF)*0.95
     text_y 			= res@trYMinF+(res@trYMaxF-res@trYMinF)*0.35
     delta_y 			= -(res@trYMaxF-res@trYMinF)*0.06

     text_res 			= True
     text_res@txFontHeightF 	= 0.015
     text_res@txJust 		= "TopRight"

     do iExp=0,nExp-1
      text_res@txFontColor 	= Colors(iExp)
      gsn_text(wks,plot,expInfo(expIndexToPlot(iExp),2),text_x,text_y+iExp*delta_y,text_res)
     end do		

     draw(plot)
     frame(wks)

     if (verbose) then
      print(""+PlotPath)
     end if
     
     system("gv "+PlotPath+"&")
     exit

    end do ; Metric loop

    delete(ValuesToPlot)
    delete(ProbScores)

    end do ; Event loop
   end do ; Start month loop
 end do ; Variable loop
end do ; Region loop


end
