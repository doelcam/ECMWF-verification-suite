; This script plots reliability diagrams					;
; 										;
; Initially written to work with output from ECMWF XDCP software	 	;
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
;										;
; Two plots are produced, one which has error bars and a line connecting each	;
; marker, the other has a weighted regression line, plus two 'confidence	;
; regression lines								;
; 			Dave MacLeod, University of Oxford, 19th April 2013	;
;
;
; Overhauled to separate repeated information into setupInfo.ncl file
; 			DM 26 Mar 2015

load "setupInfo.ncl"
begin
figDir =figDir+"/ReliabilityDiagrams/"
system ("if [ ! -d "+figDir+\
" ]; then mkdir "+figDir+";fi")
figDir =figDir+"/CategoryMaps/"
system ("if [ ! -d "+figDir+\
" ]; then mkdir "+figDir+";fi")

ReadDataRootDirectory = dataRootDir+"ProcessedData/"+runName+"/"
GiorgiRegionIndex=(/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,24/)
nGRegions = dimsizes(GiorgiRegionIndex)

;_________________________
; Events
EventNameList 		= (/"Lower tercile","Upper tercile"/)
EventList 		= (/"33","67"/)
EventPercentileList	= (/0.3333,0.3333/)



do iVar 		= 0,nVars-1 ; Looping over variables

 print("Plotting all reliability category maps for "+varInfo(iVar,2))
 do iStart		= 1,1
  do iTarget		= 0,nTargets-1 ;Looping over targets
   do iEvent 		= 0,nEvents-1 ; Looping over events
    do iExp 		= 0,nExp-1 ; Looping over experiments



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
      

      eventName 		= eventInfo(iEvent,0)
      event 			= eventInfo(iEvent,1)
      eventPercentile 		= eventPercentileList(iEvent)

      DataFileName		= "Reliability_categories_"+\
				expID+"_"+initialisationMonth+"0100_"\
				+varNumber+"_"+varLevel+"_"+event+"_"\
				+targetCode+".nc"
				
      DataFileName		=ReadDataRootDirectory+DataFileName
     if (.not.isfilepresent(DataFileName)) then
        print("File missing, continuing. File is "+DataFileName)
	continue
     end if
       
     ReadFile			= addfile(DataFileName,"r")
     Categories			= ReadFile->Categories




     ;------------------------- plotting ------------------------

     PlotName 			= "Rel_Cat_Map_"
     PlotName 			= PlotName+expID+"_"+hindcastPeriod+\
     				"_"+initialisationMonth+"0100_"\
     				+varNumber+"_"+varLevel+"_"+event+"_"+\
     				targetCode
     PlotPath 			= figDir+PlotName


     wks 			= gsn_open_wks(plotType,PlotPath)
     ColorMap 			= "amwg"
     gsn_define_colormap(wks, ColorMap)

     trans_red 			= NhlNewColor(wks,0.95,0.1,0.1)
     trans_orange 		= NhlNewColor(wks,0.95,0.5,0.1)
     trans_yellow 		= NhlNewColor(wks,0.95,0.95,0)
     trans_blue 		= NhlNewColor(wks,0.5,0.5,0.95)
     trans_green		= NhlNewColor(wks,0.3,0.95,0.3)

     res 			= True
     res@gsnDraw 		= False
     res@gsnFrame 		= False

     Colors 			= (/trans_red,trans_orange,\
     				trans_yellow,trans_green,trans_blue/)
     ;Colors = (/"red","orange","yellow","blue","green"/)
     res@mpOutlineBoundarySets 	= "Geophysical"
     res@mpFillOn = False
     res@tmXTOn 		= False
     res@tmXBOn 		= False
     res@tmYLOn 		= False
     res@tmYROn 		= False

     res@tiMainString		= varName+", month "\
       				+(targetStart+1)+"-"+(targetEnd+1)+\
       				" ("+targetName+") "+expID 
     plot 			= gsn_csm_map_ce(wks,res)  ; draw global map

     do iRegion=1,1;0,nGRegions-1
      Domain 			= new(4,float)
      RegionIndex		=GiorgiRegionIndex(iRegion)
      RegionCode		=regionData(RegionIndex,0)
      Domain			=AssignGiorgiDomain(RegionCode)

      Cat			=floattoint(Categories(RegionIndex)-1)
      ;		print(RegionCode+"_"+Cat)
      ; Adding polygon for this region
      polygonres		= True
      ;polygonres@gsFillOpacityF = 0.5
      
      polygonres@gsFillColor 	=Colors(Cat)

      poly_x 			= new((/4/),float)
      poly_y 			= new((/4/),float)

      poly_x(0)                 = Domain(2)
      poly_x(1)                 = Domain(2)
      poly_x(2)                 = Domain(3)
      poly_x(3)                 = Domain(3)

      poly_y(0)                 = Domain(0)
      poly_y(1)                 = Domain(1)
      poly_y(2)                 = Domain(1)
      poly_y(3)                 = Domain(0)


      gsn_polygon(wks,plot,poly_x,poly_y,polygonres)
      delete(Domain)			
     end do
     
     resoc 			=res
     resoc@cnFillOn		=True
     resoc@gsnAddCyclic		=True
     resoc@lbLabelBarOn		=False
     delete(resoc@mpFillOn)
     delete(resoc@mpOutlineBoundarySets)
     dlat			=960
     dlon			=1440
     dummydata			=new((/dlat,dlon/),float,-9999)
     dummydata!0 		= "lat"
     dummydata!1 		= "lon"
     lats			=fspan(-90,90,dlat)
     lons			=fspan(0,359.9,dlon)
     dummydata&lat		=lats
     dummydata&lon		=lons
     dummydata&lat@units 	= "degreesN"
     dummydata&lon@units 	= "degreesE"
     dummydata			=0
     dummydata(0,0)		=1
     a		=addfile("$NCARG_ROOT/lib/ncarg/data/cdf/landsea.nc","r")
     lsdata 			= a->LSMASK
     lsm  			= landsea_mask(lsdata,lats,lons)
     dummydata 			= mask(dummydata,lsm.eq.1,False)
     dummydata(0,0)		=1
     resoc@cnFillColors		=(/"white","white","white","white","white","white"/)
     resoc@cnInfoLabelOn 	= False				
     oceanplot 			= gsn_csm_contour(wks,dummydata,resoc)
     overlay(plot,oceanplot)

     getvalues plot                      ; Get plot size for use in
     "vpHeightF" : vph                 ; creating labelbar.
     "vpWidthF"  : vpw
     end getvalues	

     lbres                    	= True          ; labelbar only resources
     lbres@lbAutoManage       	= False          ; Necessary to control sizes
     lbres@vpWidthF           	= 0.5*vpw      ; labelbar width
     lbres@vpHeightF          	= 0.4*vph     ; labelbar height
     lbres@lbBoxMajorExtentF  	= 0.75          ; puts space between color boxes
     lbres@lbFillColors       	= Colors
     lbres@lbMonoFillPattern  	= True          ; Solid fill pattern
     lbres@lbLabelFontHeightF 	= 0.02          ; font height. default is small
     lbres@lbLabelJust        	= "CenterCenter"  ; left justify labels
     lbres@lbTitleString 	= "" ; left justify labels
     lbres@lbTitlePosition 	= "Bottom"  ; left justify labels
     lbres@lbTitleFontHeightF 	= lbres@lbLabelFontHeightF*1.1  ; left justify labels
     lbres@lbOrientation 	= "Horizontal"
     lbres@lbPerimOn 		= False
     ;lbres@lbFill
     amres                 	= True
     amres@amJust           	= "BottomCenter"
     amres@amParallelPosF   	=  0.0
     amres@amOrthogonalPosF 	= 0.95
     nBoxes 			= dimsizes(Colors)
     labels 			= (/"1","2","3","4","5"/)
     lbid 			= gsn_create_labelbar(wks,nBoxes,labels,lbres)
     annoid 			= gsn_add_annotation(plot,lbid,amres)

     draw(plot)
     frame(wks)
     
     if (verbose) then
      print("PlotPath is "+PlotPath)
     end if
;     system("gv "+PlotPath+"&")
;     exit

     delete(Categories)	

    end do ; Experiment loop
   end do ; Event loop
  end do ; Target loop
 end do ; start month loop
end do ; Variable loop

end
