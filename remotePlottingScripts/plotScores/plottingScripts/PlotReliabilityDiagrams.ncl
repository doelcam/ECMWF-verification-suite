; This script plots reliability diagrams					;
; 										;
; Initially written to work with output from ECMWF XDCP software	 	;
; However the output comes in ASCII format so it is necessary to carefully	;
; read it in (including the nBoot bootsstrap samples). For this purpose a 	;
; function "read_reliability_ascii" has been written, which must be loaded	;
; (in "function_ReadReliabilityDataFromAscii.ncl" loaded by setupInfo)	;
; 										;
; Be careful using this function, it is tested for this purpose but if the 	;
; format of the ascii file changes then it may not work. Also N.B. the 		;
; arguments of the function (should be self-explanatory from their names)	;
;										;
;										;
; Two plots can be produced, one which has error bars and a line connecting each	; marker, the other has a weighted regression line, plus two 'confidence	;
; regression lines. This is controlled by the loop 'Regression' 		;	;										;
; 			Dave MacLeod, University of Oxford, 19th April 2013	;
;
;
; Overhauled to separate repeated information into setupInfo.ncl file
; 			DM 26 Mar 2015

; Uploaded to gitHub (user doelcam


load "setupInfo.ncl"
begin
figDir =figDir+"/ReliabilityDiagrams/"
system ("if [ ! -d "+figDir+\
" ]; then mkdir "+figDir+";fi")

Category = new((/nRegions,nVars,nStarts,nTargets,nEvents,nExp/),float)
Slopes = new((/nRegions,nVars,nStarts,nTargets,nEvents,nExp/),float,-9999.)
Intercepts = new((/nRegions,nVars,nStarts,nTargets,nEvents,nExp/),float,-9999.)
Slope_min = new((/nRegions,nVars,nStarts,nTargets,nEvents,nExp/),float,-9999.)
Intercept_min = new((/nRegions,nVars,nStarts,nTargets,nEvents,nExp/),float,-9999.)
Slope_max = new((/nRegions,nVars,nStarts,nTargets,nEvents,nExp/),float,-9999.)
Intercept_max = new((/nRegions,nVars,nStarts,nTargets,nEvents,nExp/),float,-9999.)

do Regression		=0,1 ; plot with jagged line/error bars or with regressionline & CI
 do iVar 		=0,nVars-1;  Looping over variables
   	print("Proccessing data for "+runName+\
  	", regression switch is "+Regression+" and variable switch is "+iVar)
  print("Plotting all reliability diagrams for "+varInfo(iVar,0))
  do iTarget		=1,nTargets-1 ;Looping over targets
   do iEvent 		=1,nEvents-1 ; Looping over events
    do iExp 		=0,nExp-1 ; Looping over experiments 
     do iStart		=1,1;nStarts-1 ; Looping over forecast start months (may/nov)
      do iRegion	=0,nRegions-1 ; Looping over regions




    ;----------------- setting up values with loop iterators -------------------
      initialisationMonth	=initialisationMonthList(iStart)
      
      expID 			= expInfo(iExp,0)
      expType 			= expInfo(iExp,1)
      expLongName 		= expInfo(iExp,2)

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
      varLevel+"_T"+event+"_M1_B"+nBins+"_"+targetCode+"_"+regionCode

      if (.not.fileexists(filename)) then
       print("File missing, continuing. File is "+filename)
       continue
      end if
       
       fld 			= asciiread(filename,-1,"float")
       ReliabilityData 		= new((/nBins,12/),float,-9999.) 

       ReliabilityData		=read_reliability_ascii(filename,\
       				yIni,yFin,nBins,nBoot,nMembers,SigLvl)
       delete(fld)
       PopulatedIndex 		= ind(ReliabilityData(:,0).ge.2)

      ;------------------------- plotting ------------------------

       PlotName 		= "BRV_"+expID+\
       				"_"+hindcastPeriod+"_"+initialisationMonth+"0100_"\
       				+varNumber+"_"+varLevel+"_"+event+"_"+\
       				targetCode+"_"+regionCode

       ;reg = lspoly(ReliabilityData(1:4,2),ReliabilityData(1:4,3),ReliabilityData(1:4,0),2)
       ;print(reg)
       ;ReliabilityData(0,6:7)=reg

       if (Regression.eq.1) then
        PlotName = PlotName+"_regressionarea"
       end if

       PlotPath 			= figDir+PlotName

       wks 				= gsn_open_wks(plotType,PlotPath)
       ColorMap 			= "amwg"
       gsn_define_colormap(wks, ColorMap)
       res 				= True
       res@gsnDraw 			= False
       res@gsnFrame 			= False
       res@gsnXRefLine 			= eventPercentile
       res@gsnXRefLineThicknessF 	= 2.0
       res@gsnXRefLineColor 		= "grey"
       res@gsnYRefLineThicknessF	= 2.0
       res@gsnYRefLine 			= eventPercentile
       res@gsnYRefLineColor 		= "grey"

       res@trXMinF 			= 0
       res@trXMaxF 			= 1
       res@trYMinF 			= 0
       res@trYMaxF 			= 1

       res@xyLineThicknessF 		= 1.5
       res@xyLineColor 			= "red"

       res@tmXTOn 			= False
       res@tmYROn 			= False
       res@tmXBMinorOn 			= False
       res@tmYLMinorOn 			= False 
       res@tmXBMajorLengthF 		= 0.005
       res@tmYLMajorLengthF 		= 0.005
       res@tmXBLabelFontHeightF 	= 0.015
       res@tmYLLabelFontHeightF 	= 0.015

       res@tiXAxisString 		= "Forecast probability"
       res@tiXAxisFontHeightF 		= 0.015
       res@tiYAxisString 		= "Observed frequency"
       res@tiYAxisFontHeightF 		= 0.015
       res@tiMainFontHeightF 		= 0.015
       res@tiMainString 		= "Reliability Diagram "+\
       					"("+expID+")~C~"+eventName+" "+varName+\
       					" over "+regionName+"~C~Reference is "\
					+varReference+"~C~FC period: month "\
       					+(targetStart+1)+"-"+(targetEnd+1)+\
       					" ("+targetName+")"

       ; For the plot without regression lines, make the plot with a join-the-dots
       ; plot. For the regression line, first initialise the plot with a dummy plot
       ; (hence the -100), then go about calculating categories etc...)

       if (Regression.eq.0) then
        plot			=gsn_csm_xy(wks,ReliabilityData(PopulatedIndex,2)\
				,ReliabilityData(PopulatedIndex,3),res)
       else
        plot			=gsn_csm_xy(wks,ReliabilityData(:,2)-100\
        			,ReliabilityData(:,3)-100,res)

	polyres				=True
	polyres@gsLineDashPattern	=0
	polyres@gsLineThicknessF 	= 1.0
	polyres@gsLineColor 		= "red"
	
;	print(filename)
;	print(ReliabilityData)
;	exit

	min_Intercept 			= ReliabilityData(0,8)
	min_Grad 			= ReliabilityData(0,9)
	min_Intercept 			= eventPercentile*(1-min_Grad)
	gsn_polyline(wks,plot,(/0,1/),(/min_Intercept,min_Intercept\
					+min_Grad/),polyres)

	max_Intercept 			= ReliabilityData(0,10)
	max_Grad 			= ReliabilityData(0,11)
	max_Intercept 			= eventPercentile*(1-max_Grad)
	gsn_polyline(wks,plot,(/0,1/),(/max_Intercept,max_Intercept\
					+max_Grad/),polyres)
;print(max_Intercept)

	; Adding polygons
	polygonres			= polyres
	trnsred 			= NhlNewColor(wks,1,0.75,0.75) 
	polygonres@gsFillColor 		= trnsred
	poly_1 				= new((/2,3/),float)
	poly_2 				= new((/2,3/),float)

	; X co-ordinates
	poly_1(0,0) 			= 0
	poly_1(0,1)                     = 0
	poly_1(0,2)                     = eventPercentile
	poly_2(0,0)                     = 1
	poly_2(0,1)                     = 1
	poly_2(0,2)                     = eventPercentile

	; Y co-ordinates
	poly_1(1,0)                     = min_Intercept
	poly_1(1,1)                     = max_Intercept
	poly_1(1,2)                     = eventPercentile
	poly_2(1,0)                     = min_Intercept+min_Grad
	poly_2(1,1)                     = max_Intercept+max_Grad
	poly_2(1,2)                     = eventPercentile

	gsn_polygon(wks,plot,poly_1(0,:),poly_1(1,:),polygonres)
	gsn_polygon(wks,plot,poly_2(0,:),poly_2(1,:),polygonres)

	; Regression regression line on top
	reg_Intercept 			= ReliabilityData(0,6)
	reg_Grad 			= ReliabilityData(0,7)
	polyres@gsLineDashPattern	=0
	polyres@gsLineColor 		= "red"
	polyres@gsLineThicknessF 		= 2.0
	;reg_Intercept 			= eventPercentile*(1-reg_Grad)
	gsn_polyline(wks,plot,(/0,1/),(/reg_Intercept,reg_Intercept\
					+reg_Grad/),polyres)

	; Defining the category
	reg_max_Intercept 		= reg_Intercept+reg_Grad
	min_I 				= poly_2(1,0) ; higher of two lines @ x=1 
	max_I 				= poly_2(1,1) ; lower of two lines @ x=1 

	Category(iRegion,iVar,iStart,iTarget,iEvent,iExp)\
	=CalcRelCat(eventPercentile,reg_max_Intercept,min_I,max_I)

	; Saving data for the ASCII file
	Slopes(iRegion,iVar,iStart,iTarget,iEvent,iExp)\
	= reg_Grad
	Intercepts(iRegion,iVar,iStart,iTarget,iEvent,iExp)\
	= reg_Intercept
	Slope_min(iRegion,iVar,iStart,iTarget,iEvent,iExp)\
	= min_Grad
	Intercept_min(iRegion,iVar,iStart,iTarget,iEvent,iExp)\
	= min_Intercept
	Slope_max(iRegion,iVar,iStart,iTarget,iEvent,iExp)\
	= max_Grad
	Intercept_max(iRegion,iVar,iStart,iTarget,iEvent,iExp)\
	= max_Intercept
	text_res 			= True
	text_res@txFontColor 		= "red"
	text_xpos 			= 0.95
	text_ypos 			= 0.05
	text_res@txFontHeightF 		= 0.02
	text_res@txJust 		= "BottomRight"

	if (nBoot.ne.0) then
	 gsn_text(wks,plot,"Category"+Category(iRegion\
	 ,iVar,iStart,iTarget,iEvent,iExp)\
	,text_xpos,text_ypos,text_res)
	end if
       end if


       ; Adding the perfect and no skill line
       polyres 				= True
       polyres@gsLineDashPattern 	= 0
       polyres@gsLineThicknessF 	= 2.0
       polyres@gsLineColor 		= "black"
       gsn_polyline(wks,plot,(/0,1/),(/0,1/),polyres)
       polyres@gsLineThicknessF 	= 1.5
       NoSkillStart 			= eventPercentile/2
       NoSkillEnd 			= 1-(1-eventPercentile)/2
       polyres@gsLineDashPattern 	= 11
       gsn_polyline(wks,plot,(/0,1/),(/NoSkillStart,NoSkillEnd/),polyres)

       ; Adding the markers & error bars
       marker_res			= True
       marker_res@gsMarkerColor 	= "red"
       marker_res@gsMarkerIndex 	= 16
       dot_res				= True
       dot_res@gsMarkerColor 		= "black"
       dot_res@gsMarkerSizeF		=0.01
       dot_res@gsMarkerIndex		=1
       error_res 			= True
       error_res@gsMarkerIndex 		= 1
       error_res@gsLineColor 		= "grey"
       error_res@gsLineThicknessF 		= 6.0

       error_bar 			= new((/nBins/),graphic)
       dots 				= new((/nBins/),graphic)
       markers 				= new((/nBins/),graphic)

       marker_sizes			=ReliabilityData(:,0)
       marker_sizes			=sqrt(marker_sizes)
       marker_scale			=0.8
       marker_sizes 			= marker_scale*marker_sizes/max(marker_sizes)
       ; NCL doesn't like marker sizes equal to zero
       marker_sizes			=where(marker_sizes.eq.0,0.0001,marker_sizes)

       ReliabilityData(:,2)=where(marker_sizes.le.0,-1,ReliabilityData(:,2))

       do i = 0,dimsizes(PopulatedIndex)-1
	ibin=i

	; Plotting error bars
	if (Regression.eq.0) then
	 ; Plotting error bars
	 
;	 if (ReliabilityData(ibin,4).eq.0.and.ReliabilityData(ibin,5).eq.0) then
;	  ReliabilityData(ibin,5)=1
;	 end if
	 
	 error_bar(ibin)		=gsn_add_polyline\
	 		(wks,plot,(/ReliabilityData(ibin,2),ReliabilityData\
	 		(ibin,2)/),(/ReliabilityData(ibin,4),\
	 		ReliabilityData(ibin,5)/),error_res)
	end if

	; Plotting markers
	marker_res@gsMarkerSizeF 	= marker_sizes(ibin)/10
	markers(ibin)			=gsn_add_polymarker\
					(wks,plot,ReliabilityData(ibin,2),\
					ReliabilityData(ibin,3),marker_res)

	; Plotting the little dots
	dots(ibin)			=gsn_add_polymarker\
					(wks,plot,ReliabilityData(ibin,2)\
					,ReliabilityData(ibin,3),dot_res)
       end do  ; Regression plot loop

       OutputDataToScreen=0 ; Screen output x,y and population size 
       ;to check regression calculation by hand

       if (OutputDataToScreen.eq.1) then
	; Screen output x,y and population size 
	;to check regression calculation by hand
	print("Bin pop,Forecast prob (x),Observed prob (y),Bin pop")

	do i=0,nBins-1
	 print(""+ReliabilityData(i,0)+","+ReliabilityData(i,2)+\
	 ","+ReliabilityData(i,3))
	end do

	print("Slope,Min slope,Max slope")
	print(""+Slopes(iRegion,iVar,iStart,iTarget,iEvent,iExp)\
	+","+Slope_min(iRegion,iVar,iStart,iTarget,iEvent,iExp)\
	+","+Slope_max(iRegion,iVar,iStart,iTarget,iEvent,iExp))
       end if

       draw(plot)
       frame(wks)
       delete(error_bar)
       delete(PopulatedIndex)

       if (verbose) then
        print(""+PlotPath)
       end if
system("gv "+PlotPath+"&")
exit
      end do ; Region loop

      StoreDataRootDirectory 	= dataRootDir+"ProcessedData/"\
      				+runName+"/"
      system ("if [ ! -d "+StoreDataRootDirectory+\
      " ]; then mkdir "+StoreDataRootDirectory+";fi")

      DataFileName		="Reliability_categories_"+\
      				expID+"_"+initialisationMonth+"0100_"\
      				+varNumber+"_"+varLevel+"_"\
				+event+"_"+targetCode+".nc"
      DataFileName		= StoreDataRootDirectory+DataFileName
      system ("rm "+DataFileName)
      write_file		= addfile(DataFileName,"c")
      write_file->Categories	=Category(:,iVar,iStart,iTarget,iEvent,iExp)

      if (verbose) then
       print("Wrote file "+DataFileName)	
      end if
     ; exit
     end do ; Start month loop
    end do ; Experiment loop
   end do ; Event loop
  end do ; Target loop
 end do ; Variable loop
end do ; Regression outer loop




end
