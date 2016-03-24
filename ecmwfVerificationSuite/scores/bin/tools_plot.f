c
c----------------------------------------------------------------------c
c                                                                      c
c     COLLECTION OF SUBROUTINES FOR PLOTTING DIAGNOSTICS               c
c                                                                      c
c----------------------------------------------------------------------c
c
c----------------------------------------------------------------------c
c     SUBROUTINE titles                   F. Doblas-Reyes, 17-Aug-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     Obtains the titles for the numeric values of the variables       c
c                                                                      c
c     INPUT:                                                           c
c     imm   :   start month                                            c
c     ipar  :   parameter                                              c
c     ilev  :   level                                                  c
c     regna :   acronym of the region                                  c
c     dia   :   acronym of the diagnostic                              c
c     nin   :   number of expected colour intervals                    c
c                                                                      c
c     OUTPUT:                                                          c
c     start :   name of the starting month                             c
c     variable: name of the variable                                   c
c     region:   name of the region                                     c
c     diagnostic: full name of the diagnostic                          c
c     clist :   colour intervals                                       c
c     llist :   limits of the intervals                                c
c     xmax  :   maximum level for contouring                           c
c     xmin  :   minimum level for contouring                           c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE titles(imm, ipar, ilev, regna, dia,
     >                  start, variable, region,
     >                  diagnostic, nin, clist, llist, xmax, xmin)
c
      implicit none
c
      integer imm, ipar, ilev, nin
      character*4 regna
      character dia
      real llist(nin+1), xmax, xmin, xint, xgap
      character*20 clist(nin)
      character*20 start_month(12), start
      character*40 variable, region
      character*50 diagnostic
      integer nint1, nint2, iint
      parameter(nint1=13)
      parameter(nint2=7)
      real llist1(nint1+1)
      real llist2(nint1+1)
      real llist3(nint1+1)
      real llist4(nint1+1)
      real llist5(nint2+1)
      real llist6(nint2+1)
      real llist7(nint2+1)
      real llist8(nint1+1)
      real llist9(nint1+1)
      character*20 clist1(nint1)
      character*20 clist2(nint2)
      character*20 clist3(nint2)
      character*20 clist4(nint1)
c
      data start_month/'January','February','March','April',
     >   'May','June','July','August','September','October',
     >   'November','December'/
      start=start_month(imm)
c
      data clist1/'rgb(0.00,0.00,1.00)','rgb(0.03,0.46,0.93)',
     >            'rgb(0.53,0.64,1.00)','rgb(0.49,0.74,1.00)',
     >            'rgb(0.68,0.87,1.00)','rgb(0.70,1.00,1.00)',
     >   'grey','yellow','orange_yellow','yellowish_orange',
     >   'orange','red','burgundy'/
      data clist2/
     >   'grey','yellow','orange_yellow','yellowish_orange',
     >   'orange','red','burgundy'/
      data clist3/
     >   'burgundy','red','orange','yellowish_orange',
     >   'orange_yellow','yellow','grey'/
      data clist4/'rgb(0.00,0.00,1.00)','rgb(0.03,0.46,0.93)',
     >            'rgb(0.53,0.64,1.00)','rgb(0.49,0.74,1.00)',
     >            'rgb(0.68,0.87,1.00)','rgb(0.70,1.00,1.00)',
     >   'yellow','orange_yellow','yellowish_orange',
     >   'orange','red','brick','burgundy'/
c
c.... Intervals for ACC, perfect-model ACC
c
      data llist1/-1.0,-0.9,-0.8,-0.7,-0.6,-0.4,-0.2,
     >   0.2,0.4,0.6,0.7,0.8,0.9,1.0/
      data llist2/0.0,0.3,0.5,0.6,0.7,0.8,0.9,
     >   1.1,1.2,1.3,1.4,1.6,1.8,3.0/
      data llist3/-40.0,-4.0,-2.0,-1.0,-0.8,-0.5,-0.2,
     >   0.2,0.4,0.6,0.7,0.8,0.9,1.0/
c
c.... Intervals for BSS
c
      data llist4/-10.0,-1.0,-0.6,-0.4,-0.2,-0.1,-0.05,
     >   0.05,0.1,0.15,0.3,0.4,0.6,1.0/
c
c.... Intervals for BSS_rel
c
      data llist5/0.0,0.3,0.6,0.8,0.85,0.9,0.95,1.0/
c
c.... Intervals for BSS_res
c
      data llist6/0.0,0.1,0.2,0.3,0.4,0.5,0.6,1.0/
c
c.... Intervals for sharpness
c
      data llist7/0.0,0.02,0.03,0.04,0.05,0.06,0.08,0.15/
c
c.... Intervals for ROCSS
c
      data llist8/-1.0,-0.8,-0.6,-0.4,-0.3,-0.2,-0.1,
     >   0.1,0.2,0.3,0.4,0.6,0.8,1.0/
c
c.... Intervals for TRD1
c
      data llist9/-20.0,-15.0,-10.0,-7.0,-4.0,-2.0,-1.0,
     >   1.0,2.0,4.0,7.0,10.0,15.0,20.0/
c     data llist9/-50.0,-30.0,-20.0,-14.0,-8.0,-4.0,-2.0,
c    >   2.0,4.0,8.0,14.0,20.0,30.0,50.0/
c
      if(ilev.eq.0)then
        if(ipar.eq.139)variable='Surface temperature'
        if(ipar.eq.141)variable='Snow depth'
        if(ipar.eq.146)variable='Surface sensible heat flux'
        if(ipar.eq.147)variable='Surface latent heat flux'
        if(ipar.eq.151)variable='Mean sea level pressure'
        if(ipar.eq.164)variable='Total cloud cover'
        if(ipar.eq.165)variable='10m zonal wind'
        if(ipar.eq.166)variable='10m meridional wind'
        if(ipar.eq.167)variable='Near-surface air temperature'
        if(ipar.eq.168)variable='Near-surface dewpoint temperature'
        if(ipar.eq.169)variable='Surface downward SW radiation'
        if(ipar.eq.175)variable='Surface downward LW radiation'
        if(ipar.eq.176)variable='Surface net SW radiation'
        if(ipar.eq.177)variable='Surface net LW radiation'
        if(ipar.eq.178)variable='Top net SW radiation'
        if(ipar.eq.179)variable='OLR'
        if(ipar.eq.182)variable='Evaporation'
        if(ipar.eq.201)variable='Maximum near-surface air temperature'
        if(ipar.eq.202)variable='Minimum near-surface air temperature'
        if(ipar.eq.228)variable='Precipitation'
        if(ipar.eq.229)variable='Total soil moisture'
      else
        if(ipar.eq.129)then
          write(variable,'(i3.3,a)')ilev,' hPa geopotential height'
        endif
        if(ipar.eq.130)then
          write(variable,'(i3.3,a)')ilev,' hPa temperature'
        endif
        if(ipar.eq.131)then
          write(variable,'(i3.3,a)')ilev,' hPa zonal wind'
        endif
        if(ipar.eq.132)then
          write(variable,'(i3.3,a)')ilev,' hPa meridional wind'
        endif
        if(ipar.eq.133)then
          write(variable,'(i3.3,a)')ilev,' hPa specific humidity'
        endif
        if(ipar.eq.155)then
          write(variable,'(i3.3,a)')ilev,' hPa divergence'
        endif
      endif
c
c.... region names
c
      if(regna.eq.'    ')region='NaN'
      if(regna.eq.'GLOB')region='global'
      if(regna.eq.'TROP')region='tropical band'
      if(regna.eq.'NHEX')region='Northern extratropics'
      if(regna.eq.'SHEX')region='Southern extratropics'
      if(regna.eq.'GLOL')region='global (land only)'
      if(regna.eq.'TROL')region='tropical band (land only)'
      if(regna.eq.'NHEL')region='Northern extratropics (land only)'
      if(regna.eq.'SHEL')region='Southern extratropics (land only)'
      if(regna.eq.'GLOO')region='global (ocean only)'
      if(regna.eq.'TROO')region='tropical band (ocean only)'
      if(regna.eq.'NHEO')region='Northern extratropics (ocean only)'
      if(regna.eq.'SHEO')region='Southern extratropics (ocean only)'
      if(regna.eq.'EURO')region='Europe'
      if(regna.eq.'NAME')region='North America'
      if(regna.eq.'IND1')region='tropical Indian region 1'
      if(regna.eq.'IND2')region='tropical Indian region 2'
      if(regna.eq.'TRAT')region='tropical Atlantic'
      if(regna.eq.'NI03')region='Nino 3'
      if(regna.eq.'NI04')region='Nino 4'
      if(regna.eq.'NI12')region='Nino 1-2'
      if(regna.eq.'NI34')region='Nino 3.4'
      if(regna.eq.'AUSG')region='Australia, G&F'
      if(regna.eq.'AMZG')region='Amazon Basin, G&F'
      if(regna.eq.'SSAG')region='Southern South America, G&F'
      if(regna.eq.'CAMG')region='Central America, G&F'
      if(regna.eq.'WNAG')region='Western North America, G&F'
      if(regna.eq.'CNAG')region='Central North America, G&F'
      if(regna.eq.'ENAG')region='Eastern North America, G&F'
      if(regna.eq.'ALAG')region='Alaska, G&F'
      if(regna.eq.'GRLG')region='Greenland, G&F'
      if(regna.eq.'MEDG')region='Mediterranean Basin, G&F'
      if(regna.eq.'NEUG')region='Northern Europe, G&F'
      if(regna.eq.'WAFG')region='Western Africa, G&F'
      if(regna.eq.'EAFG')region='Eastern Africa, G&F'
      if(regna.eq.'SAFG')region='Southern Africa, G&F'
      if(regna.eq.'SAHG')region='Sahara, G&F'
      if(regna.eq.'SEAG')region='Southeast Asia, G&F'
      if(regna.eq.'EASG')region='East Asia, G&F'
      if(regna.eq.'SASG')region='South Asia, G&F'
      if(regna.eq.'CASG')region='Central Asia, G&F'
      if(regna.eq.'TIBG')region='Tibet, G&F'
      if(regna.eq.'NASG')region='North Asia, G&F' 
      if(regna.eq.'TRIN')region='Tropical Indian' 
      if(regna.eq.'NATL')region='North Atlantic' 
      if(regna.eq.'NPAC')region='North Pacific' 
      if(regna.eq.'EURL')region='Europe (land only)' 
      if(regna.eq.'AFRL')region='Africa (land only)' 
c
c.... climatologies
c
      if(dia(1:4).eq.'CLIM')then
        diagnostic='Mean climate'
        if(ipar.eq.129)then
          xmax=190.
          xmin=110.
          llist(2)=120.
          xint=5.
          if(ilev.eq.500)then
            xmax=600.
            xmin=450.
            llist(2)=470.
            xint=10.
          endif
        endif
        if(ipar.eq.130)then
          xmax=20.
          xmin=-100.
          llist(2)=-80.
          xint=5.
          if(ilev.eq.850)then
            xmax=40.
            xmin=-60.
            llist(2)=-25.
            xint=5.
          endif
        endif
        if(ipar.eq.131)then
          xmax=100.
          xmin=-100.
          llist(2)=-50.
          xint=10.
          if(ilev.eq.850)then
            xmax=50.
            xmin=-50.
            llist(2)=-25.
            xint=5.
          endif
        endif
        if(ipar.eq.132)then
          xmax=25.
          xmin=-25.
          llist(2)=-15.
          xint=3.
          if(ilev.eq.850)then
            xmax=15.
            xmin=-15.
            llist(2)=-10.
            xint=2.
          endif
        endif
        if(ipar.eq.139)then
          xmax=50.
          xmin=-80.
          llist(2)=-25.
          xint=5.
        endif
        if(ipar.eq.151)then
          xmax=1045.
          xmin=970.
          llist(2)=975.
          xint=5.
        endif
        if(ipar.eq.165)then
          xmax=30.
          xmin=-30.
          llist(2)=-20.
          xint=4.
        endif
        if(ipar.eq.166)then
          xmax=15.
          xmin=-15.
          llist(2)=-10.
          xint=2.
        endif
        if((ipar.eq.167).or.(ipar.eq.168).or.
     >     (ipar.eq.201).or.(ipar.eq.202))then
          xmax=60.
          xmin=-80.
          llist(2)=-25.
          xint=5.
        endif
        if(ipar.eq.228)then
          xmax=20.
          xmin=0.
          llist(2)=0.5
          xint=1.
        endif
        do iint=2,nint1-1
           llist(iint+1)=llist(iint)+xint
           clist(iint)=clist4(iint)
        enddo
        llist(1)=xmin
        clist(1)=clist4(1)
        llist(nint1+1)=xmax
        clist(nint1)=clist4(nint1)
      endif
c
      if(dia(1:4).eq.'BIAS')then
        diagnostic='Bias (model minus reference)'
        if(ipar.eq.129)then
          xmax=15.
          xmin=-15.
          llist(2)=-8.
          xint=1.
          xgap=4.
        endif
        if(ipar.eq.130)then
          xmax=10.
          xmin=-10.
          llist(2)=-6.
          xint=1.
          xgap=2.
        endif
        if(ipar.eq.131)then
          xmax=25.
          xmin=-25.
          llist(2)=-12.
          xint=2.
          xgap=2.
          if(ilev.eq.850)then
            xmax=10.
            xmin=-10.
            llist(2)=-5.5
            xint=1.
            xgap=1.
          endif
        endif
        if(ipar.eq.132)then
          xmax=10.
          xmin=-10.
          llist(2)=-5.5
          xint=1.
          xgap=1.
          if(ilev.eq.850)then
            xmax=8.
            xmin=-8.
            llist(2)=-4.0
            xint=0.7
            xgap=1.
          endif
        endif
        if(ipar.eq.139)then
          xmax=15.
          xmin=-15.
          llist(2)=-6.
          xint=1.
          xgap=2.
        endif
        if(ipar.eq.151)then
          xmax=15.
          xmin=-15.
          llist(2)=-11.
          xint=2.
          xgap=2.
        endif
        if(ipar.eq.165)then
          xmax=10.
          xmin=-10.
          llist(2)=-5.5
          xint=1.
          xgap=1.
        endif
        if(ipar.eq.166)then
          xmax=10.
          xmin=-10.
          llist(2)=-5.5
          xint=1.
          xgap=1.
        endif
        if((ipar.eq.167).or.(ipar.eq.168).or.
     >     (ipar.eq.201).or.(ipar.eq.202))then
          xmax=10.
          xmin=-10.
          llist(2)=-6.
          xint=1.
          xgap=2.
        endif
        if(ipar.eq.228)then
          xmax=15.
          xmin=-10.
          llist(2)=-5.
          xgap=1.
          xint=1.
        endif
        do iint=2,nint1-1
           if(iint.eq.((nint1/2)+1))then
             llist(iint+1)=llist(iint)+xgap
           else
             llist(iint+1)=llist(iint)+xint
           endif
           clist(iint)=clist1(iint)
        enddo
        llist(1)=xmin
        clist(1)=clist4(1)
        llist(nint1+1)=xmax
        clist(nint1)=clist4(nint1)
      endif
c
      if(dia(1:4).eq.'STDV')then
        diagnostic='Standard deviation'
        if(ipar.eq.129)then
          xmax=20.
          xmin=0.
          xint=1.
          if(ilev.eq.500)then
            xmax=20.
            xmin=0.
            xint=1.5
          endif
        endif
        if(ipar.eq.130)then
          xmax=10.
          xmin=0.
          xint=0.4
          if(ilev.eq.850)then
            xmax=10.
            xmin=0.
            xint=0.4
          endif
        endif
        if(ipar.eq.131)then
          xmax=15.
          xmin=0.
          xint=1.
          if(ilev.eq.850)then
            xmax=10.
            xmin=0.
            xint=0.5
          endif
        endif
        if(ipar.eq.132)then
          xmax=10.
          xmin=0.
          xint=0.8
          if(ilev.eq.850)then
            xmax=7.
            xmin=0.
            xint=0.4
          endif
        endif
        if(ipar.eq.151)then
          xmax=12.
          xmin=0.
          xint=0.8
        endif
        if(ipar.eq.139)then
          xmax=10.
          xmin=0.1
          xint=0.3
        endif
        if(ipar.eq.165)then
          xmax=8.
          xmin=0.
          xint=0.4
        endif
        if(ipar.eq.166)then
          xmax=6.
          xmin=0.
          xint=0.3
        endif
        if((ipar.eq.167).or.(ipar.eq.168).or.
     >     (ipar.eq.201).or.(ipar.eq.202))then
          xmax=8.
          xmin=0.1
          xint=0.4
        endif
        if(ipar.eq.228)then
          xmax=8.
          xmin=0.
          xint=0.4
        endif
        llist(1)=xmin
        do iint=1,nint1
           llist(iint+1)=llist(iint)+xint
           clist(iint)=clist4(iint)
        enddo
        llist(nint1+1)=xmax
      endif
c
      if(dia(1:4).eq.'SDRT')then
        diagnostic='Ratio of SD (model/reference)'
        llist(1)=0.
        llist(2)=0.2
        llist(3)=0.4
        llist(4)=0.6
        llist(5)=0.7
        llist(6)=0.8
        llist(7)=0.9
        llist(8)=1.1
        llist(9)=1.2
        llist(10)=1.3
        llist(11)=1.4
        llist(12)=2.
        llist(13)=4.
        llist(14)=8.
        do iint=1,nint1
           clist(iint)=clist1(iint)
        enddo
        xmax=llist(14)
        xmin=llist(1)
      endif
c
c.... diagnostics
c
      if(dia(1:3).eq.'ACC')then
        diagnostic='Anomaly Correlation Coefficient'
        xmax=1.
        xmin=-1.
        if(nin.ne.nint1)write(*,*)'Dimension problem for lists'
        do iint=1,nint1
           llist(iint)=llist1(iint)
           clist(iint)=clist1(iint)
        enddo
        llist(nint1+1)=llist1(nint1+1)
      endif
      if(dia(1:3).eq.'ACP')then
        diagnostic=
     >     'Perfect-model Anomaly Correlation Coefficient'
        xmax=1.
        xmin=-1.
        if(nin.ne.nint1)write(*,*)'Dimension problem for lists'
        do iint=1,nint1
           llist(iint)=llist1(iint)
           clist(iint)=clist1(iint)
        enddo
        llist(nint1+1)=llist1(nint1+1)
      endif
      if(dia(1:3).eq.'SPR')then
        diagnostic='Normalized Spread (wrt ref sd)'
        if(nin.ne.nint1)write(*,*)'Dimension problem for lists'
        do iint=1,nint1
           llist(iint)=llist2(iint)
           clist(iint)=clist1(iint)
        enddo
        llist(nint1+1)=llist2(nint1+1)
        xmax=llist(nint1+1)
        xmin=llist(1)
      endif
      if(dia(1:3).eq.'RSR')then
        diagnostic='Ratio Spread(sd)/RMSE'
        if(nin.ne.nint1)write(*,*)'Dimension problem for lists'
        do iint=1,nint1
           llist(iint)=llist2(iint)
           clist(iint)=clist1(iint)
        enddo
        llist(nint1+1)=llist2(nint1+1)
        xmax=llist(nint1+1)
        xmin=llist(1)
      endif
      if(dia(1:3).eq.'MSS')then
        diagnostic='Mean Square Skill Score'
        if(nin.ne.nint1)write(*,*)'Dimension problem for lists'
        do iint=1,nint1
           llist(iint)=llist3(iint)
           clist(iint)=clist1(iint)
        enddo
        llist(nint1+1)=llist3(nint1+1)
        xmax=llist(nint1+1)
        xmin=llist(1)
      endif
      if(dia(1:3).eq.'BRV')then
        diagnostic='Reliability diagram'
      endif
      if(dia(1:3).eq.'BSS')then
        diagnostic='Brier Skill Score'
        if(nin.ne.nint1)write(*,*)'Dimension problem for lists'
        do iint=1,nint1
           llist(iint)=llist4(iint)
           clist(iint)=clist1(iint)
        enddo
        llist(nint1+1)=llist4(nint1+1)
        xmax=llist(nint1+1)
        xmin=llist(1)
      endif
      if(dia(1:3).eq.'BSI')then
        diagnostic='Brier Skill Score (infinite ensemble size)'
        if(nin.ne.nint1)write(*,*)'Dimension problem for lists'
        do iint=1,nint1
           llist(iint)=llist4(iint)
           clist(iint)=clist1(iint)
        enddo
        llist(nint1+1)=llist4(nint1+1)
        xmax=llist(nint1+1)
        xmin=llist(1)
      endif
      if(dia(1:3).eq.'REL')then
        diagnostic='Reliability Skill Score'
        if(nin.ne.nint2)write(*,*)'Dimension problem for lists'
        do iint=1,nint2
           llist(iint)=llist5(iint)
c          clist(iint)=clist3(iint)
           clist(iint)=clist2(iint)
        enddo
        llist(nint2+1)=llist5(nint2+1)
        xmax=llist(nint2+1)
        xmin=llist(1)
      endif
      if(dia(1:3).eq.'RES')then
        diagnostic='Resolution Skill Score'
        if(nin.ne.nint2)write(*,*)'Dimension problem for lists'
        do iint=1,nint2
           llist(iint)=llist6(iint)
           clist(iint)=clist2(iint)
        enddo
        llist(nint2+1)=llist6(nint2+1)
        xmax=llist(nint2+1)
        xmin=llist(1)
      endif
      if(dia(1:3).eq.'SHA')then
        diagnostic='Sharpness'
        if(nin.ne.nint2)write(*,*)'Dimension problem for lists'
        do iint=1,nint2
           llist(iint)=llist7(iint)
           clist(iint)=clist2(iint)
        enddo
        llist(nint2+1)=llist7(nint2+1)
        xmax=llist(nint2+1)
        xmin=llist(1)
      endif
      if(dia(1:3).eq.'ROC')then
        diagnostic='ROC Skill Score'
        if(nin.ne.nint1)write(*,*)'Dimension problem for lists'
        do iint=1,nint1
           llist(iint)=llist8(iint)
           clist(iint)=clist1(iint)
        enddo
        llist(nint1+1)=llist8(nint1+1)
        xmax=llist(nint1+1)
        xmin=llist(1)
      endif
      if(dia(1:4).eq.'TRD1')then
        diagnostic='Normalized slope of lin. trend (%)'
        if(nin.ne.nint1)write(*,*)'Dimension problem for lists'
        do iint=1,nint1
           llist(iint)=llist9(iint)
           clist(iint)=clist1(iint)
        enddo
        llist(nint1+1)=llist9(nint1+1)
        xmax=llist(nint1+1)
        xmin=llist(1)
      endif
c
      goto 998
 997  continue
      write(*,*)'Sorry, no success :('
      stop
 998  continue
      write(*,*)'the titles routine has successfully finished :)'
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE tseries                  F. Doblas-Reyes, 04-Apr-2008 c
c                                                                      c
c     PURPOSE:                                                         c
c     Obtains the titles for the time series                           c
c                                                                      c
c     INPUT:                                                           c
c     name  :   short name of the time series as in the file name      c
c     imm   :   start month                                            c
c                                                                      c
c     OUTPUT:                                                          c
c     title :   full name of the time series                           c
c     yaxti :   units for the y axis                                   c
c     ymax  :   maximum for the y axis                                 c
c     ymin  :   minimum for the y axis                                 c
c     yint  :   interval for the y axis                                c
c     start :   name of the starting month                             c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE tseries(name, imm, title, yaxti, ymax, ymin, yint,
     >                   start)
c
      implicit none
c
      integer imm
      real ymax, ymin, yint
      character*6 name
      character*60 title, yaxti
      character*20 start_month(12), start
c
      data start_month/'January','February','March','April',
     >   'May','June','July','August','September','October',
     >   'November','December'/
      start=start_month(imm)
c
c.... Regional averages and indices
c
      if(name.eq.'USHEAR')then
        title='U-shear index'
        yaxti='Wind anomaly (m/s)'
        ymax=15.
        ymin=-15.
        yint=3.
      endif
      if(name.eq.'VSHEAR')then
        title='V-shear index'
        yaxti='Wind anomaly (m/s)'
        ymax=2.5
        ymin=-2.5
        yint=0.5
      endif
      if(name.eq.'RRAIND')then
        title='All Indian rainfall (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=1.5
        ymin=-1.5
        yint=0.5
      endif
      if(name.eq.'RRCTPA')then
        title='Central tropical Pacific precipitation'
        yaxti='Anomaly (mm/day)'
        ymax=6.0
        ymin=-5.0
        yint=1.0
      endif
      if(name.eq.'RRINDO')then
        title='Indonesian precipitation (land and ocean)'
        yaxti='Anomaly (mm/day)'
        ymax=3.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'RRSAHE')then
        title='Sahel precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=1.5
        ymin=-1.5
        yint=0.5
      endif
      if(name.eq.'RRGUIC')then
        title='Guinea coast precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=2.5
        ymin=-2.5
        yint=0.5
      endif
      if(name.eq.'RREAFR')then
        title='East Africa precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=2.5
        ymin=-2.5
        yint=0.5
      endif
      if(name.eq.'RRSAFR')then
        title='Southern Africa precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=2.0
        ymin=-2.0
        yint=0.5
      endif
      if(name.eq.'T2SAHE')then
        title='Sahel 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'T2EAFR')then
        title='East Africa 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'T2SAFR')then
        title='Southern Africa 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'RRNAME')then
        title='North America precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=1.0
        ymin=-1.0
        yint=0.5
      endif
      if(name.eq.'T2NAME')then
        title='North America 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'RREURO')then
        title='Europe precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=1.5
        ymin=-1.5
        yint=0.5
      endif
      if(name.eq.'RRNEUR')then
        title='Northern Europe precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=1.5
        ymin=-1.5
        yint=0.5
      endif
      if(name.eq.'RRSEUR')then
        title='Southern Europe precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=1.5
        ymin=-1.5
        yint=0.5
      endif
      if(name.eq.'T2EURO')then
        title='Europe 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'T2NEUR')then
        title='Northern Europe 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=5.0
        ymin=-5.0
        yint=1.0
      endif
      if(name.eq.'T2SEUR')then
        title='Southern Europe 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'T2UKRA')then
        title='UKraine 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'RRCENA')then
        title='Central North America precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=1.5
        ymin=-1.5
        yint=0.5
      endif
      if(name.eq.'RRNWNA')then
        title='North West Pacific coast precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=1.5
        ymin=-1.5
        yint=0.5
      endif
      if(name.eq.'RRNENA')then
        title='Northeast North America precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=2.0
        ymin=-2.0
        yint=0.5
      endif
      if(name.eq.'RRGCNA')then
        title='Gulf Coast of North America precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=2.5
        ymin=-2.5
        yint=0.5
      endif
      if(name.eq.'T2CENA')then
        title='Central North America 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'T2NWNA')then
        title='North West Pacific coast 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=5.0
        ymin=-5.0
        yint=1.0
      endif
      if(name.eq.'T2NENA')then
        title='Northeast North America 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=5.0
        ymin=-5.0
        yint=1.0
      endif
      if(name.eq.'T2GCNA')then
        title='Gulf coast 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'T2FLOR')then
        title='Florida 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'RRCAAM')then
        title='Caribbean/Amazon precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=2.0
        ymin=-2.0
        yint=0.5
      endif
      if(name.eq.'RRNEBR')then
        title='Nordeste Brazil precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=3.5
        ymin=-3.5
        yint=1.0
      endif
      if(name.eq.'RRATSA')then
        title='South American Atlantic coast precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=2.5
        ymin=-2.5
        yint=0.5
      endif
      if(name.eq.'RRPASA')then
        title='South American Pacific coast precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=1.5
        ymin=-1.5
        yint=0.5
      endif
      if(name.eq.'T2CAAM')then
        title='Caribbean/Amazon 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'T2NEBR')then
        title='Nordeste Brazil 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'T2ATSA')then
        title='South American Atl. coast 2-m temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'T2PASA')then
        title='South American Pac. coast 2-m temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'RRCASI')then
        title='Central Asia precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=1.5
        ymin=-1.5
        yint=0.5
      endif
      if(name.eq.'RRJAKO')then
        title='Japan/Korea precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=2.5
        ymin=-2.5
        yint=0.5
      endif
      if(name.eq.'RRCHIN')then
        title='China precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=2.5
        ymin=-2.5
        yint=0.5
      endif
      if(name.eq.'RRMEAS')then
        title='Middle East precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=1.0
        ymin=-1.0
        yint=0.2
      endif
      if(name.eq.'RRSEAS')then
        title='Southeast Asia precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=3.0
        ymin=-3.0
        yint=0.5
      endif
      if(name.eq.'RRPHIL')then
        title='Philippines precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=6.0
        ymin=-6.0
        yint=1.0
      endif
      if(name.eq.'T2CASI')then
        title='Central Asia 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=5.0
        ymin=-6.0
        yint=1.0
      endif
      if(name.eq.'T2JAKO')then
        title='Japan/Korea 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'T2CHIN')then
        title='China 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=3.0
        ymin=-3.0
        yint=1.0
      endif
      if(name.eq.'T2MEAS')then
        title='Middle East 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=3.0
        ymin=-3.0
        yint=1.0
      endif
      if(name.eq.'RRAUST')then
        title='Australia precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=2.5
        ymin=-2.5
        yint=0.5
      endif
      if(name.eq.'RRNEAU')then
        title='Northeast Australia precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=3.0
        ymin=-3.0
        yint=0.5
      endif
      if(name.eq.'RRSEAU')then
        title='Southeast Australia precipitation (land only)'
        yaxti='Anomaly (mm/day)'
        ymax=4.0
        ymin=-3.0
        yint=1.0
      endif
      if(name.eq.'T2AUST')then
        title='Australia 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'T2NEAU')then
        title='Northeast Australia 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'T2SEAU')then
        title='Southeast Australia 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'STNIN3')then
        title='Nino3 SST (ocean only)'
        yaxti='Anomaly (K)'
        ymax=5.0
        ymin=-3.0
        yint=1.0
      endif
      if(name.eq.'STNIN4')then
        title='Nino4 SST (ocean only)'
        yaxti='Anomaly (K)'
        ymax=3.0
        ymin=-3.0
        yint=1.0
      endif
      if(name.eq.'STNI12')then
        title='Nino1-2 SST (ocean only)'
        yaxti='Anomaly (K)'
        ymax=5.0
        ymin=-4.0
        yint=1.0
      endif
      if(name.eq.'STNI34')then
        title='Nino3.4 SST (ocean only)'
        yaxti='Anomaly (K)'
        ymax=4.0
        ymin=-3.0
        yint=1.0
      endif
      if(name.eq.'STTATL')then
        title='Tropical Atlantic SST (ocean only)'
        yaxti='Anomaly (K)'
        ymax=1.4
        ymin=-1.4
        yint=0.2
      endif
      if(name.eq.'STIODI')then
        title='Indian Ocean SST dipole (ocean only)'
        yaxti='Anomaly (K)'
        ymax=3.0
        ymin=-3.0
        yint=0.5
      endif
      if(name.eq.'STATL3')then
        title='Tropical Atlantic3 SST (ocean only)'
        yaxti='Anomaly (K)'
        ymax=2.0
        ymin=-2.0
        yint=0.5
      endif
      if(name.eq.'STTAMM')then
        title='Tropical Atlantic meridional SST mode (ocean only)'
        yaxti='Anomaly (K)'
        ymax=2.0
        ymin=-2.0
        yint=0.5
      endif
      if(name.eq.'SLPSOI')then
        title='SLP-based Southern Oscillation Index'
        yaxti='Standardized anomaly'
        ymax=6.0
        ymin=-6.0
        yint=1.0
      endif
      if(name.eq.'STNATL')then
        title='North Atlantic SST'
        yaxti='Anomaly (K)'
        ymax=5.0
        ymin=-5.0
        yint=1.0
      endif
      if(name.eq.'STAMOI')then
        title='North Atlantic Multidecadal Oscillation (AMO)'
        yaxti='Anomaly (K)'
        ymax=2.0
        ymin=-2.0
        yint=0.5
      endif
      if(name.eq.'T2NHLP')then
        title='Northern Hemisphere 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=5.0
        ymin=-5.0
        yint=1.0
      endif
      if(name.eq.'CAAMAZ')then
        title='Amazon catchment precipitation'
        yaxti='Anomaly (mm/day)'
        ymax=2.0
        ymin=-2.0
        yint=0.5
      endif
      if(name.eq.'CANILE')then
        title='Nile catchment precipitation'
        yaxti='Anomaly (mm/day)'
        ymax=2.0
        ymin=-2.0
        yint=0.5
      endif
      if(name.eq.'CAZAMB')then
        title='Zambezi catchment precipitation'
        yaxti='Anomaly (mm/day)'
        ymax=2.5
        ymin=-2.5
        yint=0.5
      endif
      if(name.eq.'CAJIAN')then
        title='Jiang catchment precipitation'
        yaxti='Anomaly (mm/day)'
        ymax=2.0
        ymin=-2.0
        yint=0.5
      endif
      if(name.eq.'CAGANG')then
        title='Ganges catchment precipitation'
        yaxti='Anomaly (mm/day)'
        ymax=3.0
        ymin=-3.0
        yint=1.0
      endif
      if(name.eq.'T2GAVE')then
        title='Global-average 2-metre temperature'
        yaxti='Anomaly (K)'
        ymax=1.5
        ymin=-1.0
        yint=0.5
      endif
      if(name.eq.'T2GALP')then
        title='Global-average 2-metre temperature (land only)'
        yaxti='Anomaly (K)'
        ymax=2.5
        ymin=-2.5
        yint=0.5
      endif
      if(name.eq.'T2GAOP')then
        title='Global-average 2-metre temperature (ocean only)'
        yaxti='Anomaly (K)'
        ymax=1.2
        ymin=-1.2
        yint=0.2
      endif
c
c.... Projections
c
      if(name.eq.'Z5NAOP')then
        title='Projection onto the NAO (Z500)'
        yaxti='Arbitrary units'
        ymax=3.
        ymin=-3.
        yint=0.5
      endif
      if(name.eq.'Z5PNAP')then
        title='Projection onto the PNA (Z500)'
        yaxti='Arbitrary units'
        ymax=3.
        ymin=-3.
        yint=0.5
      endif
      if(name.eq.'SLAONP')then
        title='Projection onto the AO (MSLP)'
        yaxti='Arbitrary units'
        ymax=3.
        ymin=-3.
        yint=0.5
      endif
c
      goto 998
 997  continue
      write(*,*)'Sorry, no success :('
      stop
 998  continue
      write(*,*)'the titles routine has successfully finished :)'
c
      return
c
      end
c
