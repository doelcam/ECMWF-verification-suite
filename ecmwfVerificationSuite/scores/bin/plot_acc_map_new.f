c
c----------------------------------------------------------------------c
c     PROGRAM plot_acc_map                P. Doblas-Reyes 17-Aug-2006  c
c                                                                      c
c     PURPOSE:                                                         c
c     Plots maps of different scores, including perfect-model ACC,     c
c     ratio spread/RMSE and MSSS                                       c
c                                                                      c
c     INPUT:                                                           c
c     DIA_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE  GRIB files    c
c                                                                      c
c     OUTPUT:                                                          c
c     DIA_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE.ps   ps files  c
c                                                                      c
c     USAGE:                                                           c
c     plot_acc_map.x < nlist                                           c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -tp px  plot_acc_map.f tools.f tools_plot.f                c
c     -o plot_acc_map.x $EMOSLIB $MAGPLUSLIB_SHARED                    c
c                                                                      c
c     MODS:                                                            c
c     uses organization regular in order to plot different regions     c   
c                    L. Ferranti 29 Jan 2008                           c
c     adopted for new linux cluster                                    c
c                    A. Weisheimer Feb 2013                            c
c----------------------------------------------------------------------c
c
      PROGRAM plot_acc_map
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: s(:), d(:)
      real, allocatable :: psec4(:), psec4_sig(:)
      integer, allocatable :: lsm(:)
      real, allocatable :: field(:,:), field_sig(:,:), mask(:,:)
      real, allocatable :: field_tmp(:,:)
      character*20, allocatable :: clist(:)
      real, allocatable :: llist(:)
      real, allocatable :: ypos(:), xpos(:)
      integer, allocatable :: imar(:)
c
c.... hard coded field definitions
c
      real eps
      parameter(eps=0.1)
c
c.... GRIB headers
c
      integer ksec0(2), kret
      integer ksec1(1024)
      integer ksec2(1024)
      integer ksec3(2)
      integer ksec4(512)
      real psec2(512)
      real psec3(2)

      real firstlon,firstlat,steplon,steplat
c
c.... other definitions
c
      real topth, botth
      real spxl, spyl, pxl, pyl, sbpxl, sbpxp, sbpyl, sbpyp
      real xlatn, xlats, xlonw, xlone, xloin
      real xmax, xmin
      real rmiss  
      real xtest
      integer nx, ny, nens, iyy1, iyy2, imm, idd, itt, cros, anin
      integer ipar, ilev, nf1, nf2, iarea
      integer boot, nsam, plsi
      integer iunit1, iunit2, nxny, exptl, lena, istat
      integer nlon, nlat, i, ilon, ilat, ilonw
      integer nint, nsig, conf
      character*21 expt
      character*12 expid
      character*3 dia
      character*100 yfile, yifile, yofile, yrfile
      character*120 ctitg1, ctitg2, ctitg3, ctitg4
      character*20 start
      character*40 variable, region
      character*50 diagnostic
      character*20 form1
c
c
      namelist /control/   nx,   ny, nens,
     >                   iyy1, iyy2,  imm,  idd,  itt,
     >                   cros, ipar, ilev, anin,  nf1,  nf2,
     >                   expt, expid, boot, nsam,topth,botth, plsi,
     >                   dia
cAW
      

c
c.... set default values and read input namelist
c
      nx = 144
      ny = 71
      nens = 9
      iyy1 = 0
      iyy2 = 0
      imm  = 11
      idd  = 1
      itt  = 0
      cros = 1
      ipar = 139
      ilev = 000
      anin = 0
      expt = 'scwc'
      expid= 'scwc'
c
c.... Check the order of the parameters for the indices
c
c   nens:   number of ensemble members
c   iyy1:   start year
c   iyy2:   end year
c   imm:    forecast starting month
c   expt:   model id
c
      read(5,control)
      write(*,*)'  nx: ', nx
      write(*,*)'  ny: ', ny
      write(*,*)'nens: ',nens
      write(*,*)'iyy1: ',iyy1
      write(*,*)'iyy2: ',iyy2
      write(*,*)' imm: ',imm
      write(*,*)' idd: ',idd
      write(*,*)' itt: ',itt
      write(*,*)'cros: ',cros
      write(*,*)'ipar: ',ipar
      write(*,*)'ilev: ',ilev
      write(*,*)'expt: ',expt
      write(*,*)'anin: ',anin
      write(*,*)' nf1: ',nf1
      write(*,*)' nf2: ',nf2
      write(*,*)'boot: ',boot
      write(*,*)'nsam: ',nsam
      write(*,*)'topth: ',topth
      write(*,*)'botth: ',botth
      write(*,*)' plsi: ',plsi
      write(*,*)'  dia: ',dia
c
      rmiss=-1.e30
      nxny=nx*ny
      exptl=lena(expt)
c
      allocate (d(nxny),                                     stat=istat)
      allocate (s(nxny),                                     stat=istat)
      allocate (psec4(nxny),                                 stat=istat)
      allocate (psec4_sig(nxny),                             stat=istat)
      allocate (lsm(nxny),                                   stat=istat)
c
c.... open input file
c
      write(form1,'(a,i2.2,a)')'(a',exptl,')'
      yifile='ACC_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE'
      write(yifile(       1:       3),'(a3)')dia
      write(yifile(       5: 4+exptl),form1)expt(1:exptl)
      write(yifile( 5+exptl: 5+exptl),'(a1)')'_'
      write(yifile( 6+exptl: 9+exptl),'(i4.4)')iyy1
      write(yifile(10+exptl:10+exptl),'(a1)')'-'
      write(yifile(11+exptl:14+exptl),'(i4.4)')iyy2
      write(yifile(15+exptl:15+exptl),'(a1)')'_'
      write(yifile(16+exptl:17+exptl),'(i2.2)')imm
      write(yifile(18+exptl:19+exptl),'(i2.2)')idd
      write(yifile(20+exptl:21+exptl),'(i2.2)')itt
      write(yifile(22+exptl:24+exptl),'(a3)')'_CV'
      write(yifile(25+exptl:26+exptl),'(i2.2)')cros
      write(yifile(27+exptl:28+exptl),'(a2)')'_I'
      write(yifile(29+exptl:29+exptl),'(i1.1)')anin
      write(yifile(30+exptl:30+exptl),'(a1)')'_'
      write(yifile(31+exptl:33+exptl),'(i3.3)')ipar
      write(yifile(34+exptl:34+exptl),'(a1)')'_'
      write(yifile(35+exptl:37+exptl),'(i3.3)')ilev
      write(yifile(38+exptl:38+exptl),'(a1)')'_'
      write(yifile(39+exptl:41+exptl),'(i3.3)')nf1
      write(yifile(42+exptl:42+exptl),'(a1)')'-'
      write(yifile(43+exptl:45+exptl),'(i3.3)')nf2
      yfile=yifile(1:lena(yifile))//'.grb'
      write(*,*)'open input file: ',yfile
      kret=0
      call pbopen(iunit1,yfile,'r',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
      yrfile=yifile
      if((boot.eq.1).and.((dia.ne.'ACP').and.(dia.ne.'SPR')))then
        write(yrfile(46+exptl:47+exptl),'(a2)')'_S'
        write(yrfile(48+exptl:52+exptl),'(i5.5)')nsam
        yfile=yrfile(1:lena(yrfile))//'.grb'
        write(*,*)'open input file ',yfile(1:lena(yfile))
        kret=0
        call pbopen(iunit2,yfile,'r',kret)
        if(kret.ne.0)then
          write(*,*)'Error in opening file: kret=',kret
          goto 997
        endif
        conf=int(topth-botth)
      endif
c
c.... reading data
c
      call read1grb(iunit1, nxny, psec4,
     >   ksec0, ksec1, ksec2, ksec3,
     >   ksec4, psec2, psec3)
      nlon=ksec2(2)
      nlat=ksec2(3)
      firstlat=ksec2(4)/1000.
      firstlon=ksec2(5)/1000.
      steplat=ksec2(9)/1000.
      steplon=ksec2(10)/1000.
      write(*,*)'firstlatitude= ',firstlat,'firslongitude= ',firstlon
      write(*,*)'increment in latitude=  ',steplat
      write(*,*)'increment in longitude= ',steplon

      if(nlon*nlat.ne.nxny)then
        write(*,*)'The file contains the wrong number of points'
        goto 998
      endif
      if((boot.eq.1).and.((dia.ne.'ACP').and.(dia.ne.'SPR'))) then
        call read1grb(iunit2, nxny, psec4_sig,
     >     ksec0, ksec1, ksec2, ksec3,
     >     ksec4, psec2, psec3)
      endif
      allocate (field(nlon,nlat),                        stat=istat)
      allocate (field_sig(nlon,nlat),                    stat=istat)
      allocate (field_tmp(nlon,nlat),                    stat=istat)
      do i=1,nxny
         ilat=int((i-1)/nlon)+1
         ilon=i-(ilat-1)*nlon
         field(ilon,ilat)=psec4(i)
         if((boot.eq.1).and.((dia.ne.'ACP').and.(dia.ne.'SPR')))
     >      field_sig(ilon,ilat)=psec4_sig(i)
      enddo
      xlatn=ksec2(4)/1000.
      xlonw=ksec2(5)/1000.
      xlats=ksec2(7)/1000.
      xlone=ksec2(8)/1000.
      xloin=360./nlon


c.... reading land-sea mask
c
      call get_mask(1, lsm, nxny, expt)
      allocate (mask(nlon,nlat),                             stat=istat)
      do i=1,nxny
         ilat=int((i-1)/nlon)+1
         ilon=i-(ilat-1)*nlon
         mask(ilon,ilat)=float(lsm(i))
      enddo
c
c.... looking for the title
c
      nint=13
      allocate (clist(nint),                                 stat=istat)
      allocate (llist(nint),                                 stat=istat)
      call titles(imm, ipar, ilev, '    ', dia,
     >   start, variable, region,
     >   diagnostic, nint, clist, llist, xmax, xmin)
      write(*,*)xmin,xmax
      write(*,*)(clist(i),i=1,nint)
      write(*,*)(llist(i),i=1,nint+1)
c
c.... open output file
c
      yofile='psfiles/'//yrfile(1:lena(yrfile))//'.ps'
      write(*,*)'Output file ',yofile(1:lena(yofile)),
     >   ' is a postscript file'
      
      WRITE(CTITG1,'(A,A,A,A,i2,A)')
     >   diagnostic(1:lena(diagnostic)),' for ',
     >   expt(1:lena(expt)),' with ',
     >   nens,' ensemble members'
      write(*,*)CTITG1
c      WRITE(CTITG1,'(A,A,A,A,i2,A)')
c     >   diagnostic(1:lena(diagnostic)),' for ',
c     >   expid,' with ',
c     >   nens,' ensemble members'
c      write(*,*)CTITG1 
      WRITE(CTITG2,'(A)')variable
      WRITE(CTITG3,'(A,I4.4,A,I4.4,A,A,A,i2,A,i2)')
     >   'Hindcast period ',iyy1,'-',iyy2,
     >   ' with start in ',start(1:lena(start)),
     >   ' average over months ',nf1,' to ',nf2
      write(*,*)CTITG1
      write(*,*)CTITG2
      write(*,*)CTITG3
      if((boot.eq.1).and.(dia.ne.'ACP'))then
        WRITE(CTITG4,'(A,A,i2,A,i5,A)')'Black dots for values ',
     >     'significantly different from zero with ',
     >     conf,'% confidence (',nsam,' samples)'
        write(*,*)CTITG4
      endif

      CALL POPEN
      CALL PSETC('output_format','ps')
      CALL PSETC('output_fullname',yofile)
c
      CALL PSETC('LAYOUT','POSITIONAL')
      CALL PSETC('output_ps_colour_model','RGB')
c
cc       xlatn=85.
cc       xlats=-85.
cc       xlonw=150.
cc       xlone=540.
c
      call plot_map (field,nlon,nlat,field_sig,mask,
     > xlatn,xlats,xlonw,xlone,boot,dia,plsi,nint,llist,clist,
     > xmin,xmax,CTITG1,CTITG2,CTITG3,CTITG4,yofile,
     > firstlon,firstlat,steplon,steplat,0)
c     
cc      do  iarea=1,8

cc       xlatn=rlatn(iarea)
cc       xlats=rlats(iarea)
cc       xlonw=rlonw(iarea)
cc       xlone=rlone(iarea)
cc       call plot_map (field,nlon,nlat,field_sig,mask,
cc     > xlatn,xlats,xlonw,xlone,boot,dia,plsi,nint,llist,clist,
cc     > xmin,xmax,CTITG1,CTITG2,CTITG3,CTITG4,yofile,
cc     > firstlon,firstlat,steplon,steplat,iarea)
    
cc       enddo
c

      call pclose

      goto 999
 997  continue
      if(kret.ne.0)then
        goto 998
      else
        goto 999
      endif
 998  write(*,*)'Sorry, no success :('
      call abort
 999  write(*,*)'program seems to be successfully finished :)'

      end

      subroutine plot_map (field,nlon,nlat,field_sig,mask,
     >  xlatn,xlats,xlonw,xlone,boot,dia,plsi,nint,llist,clist,
     >  xmin,xmax,CTITG1,CTITG2,CTITG3,CTITG4,yofile,
     >  firstlon,firstlat,steplon,steplat,iflag)

      implicit none
      
      integer nlon,nlat,plsi,boot,iflag
      integer  i, ilon, ilat, ilonw, lena, nsig, nint, istat
      integer, allocatable :: imar(:)
      real spxl, spyl, pxl, pyl, sbpxl, sbpxp, sbpyl, sbpyp  
      real xmax, xmin, xtest
      real firstlon,firstlat,steplon,steplat
      real rmiss,eps,xlatn, xlats, xlonw, xlone, xloin
      real, field(nlon,nlat), field_sig(nlon,nlat), mask(nlon,nlat)
      real, allocatable :: ypos(:), xpos(:)
      real, llist(nint)
      character*20, clist(nint)
      character*3 dia
      character*120 ctitg1, ctitg2, ctitg3, ctitg4
      character*100 yofile
      eps=0.1
      rmiss=-1.e30
      xloin=360./nlon
c     
      write(*,*)' inside  plot_map   '
      write(*,*)'firstlatitude= ',firstlat,'firslongitude= ',firstlon
      write(*,*)'increment in latitude=  ',steplat
      write(*,*)'increment in longitude= ',steplon     
      write(*,*)' definition of supage corners: '
      write(*,*)'LOWER_LEFT_LATITUDE:  ', xlats
      write(*,*)'LOWER_LEFT_LONGITUDE: ', xlonw
      write(*,*)'UPPER_RIGHT_LATITUDE: ', xlatn
      write(*,*)'UPPER_RIGHT_LONGITUDE: ',xlone
      write(*,*)'IFLAG= ',iflag

C
C     PLOTTING SECTION
C
      SPXL=29.5
      SPYL=21.
      CALL PSETR('SUPER_PAGE_X_LENGTH',SPXL)
      CALL PSETR('SUPER_PAGE_Y_LENGTH',SPYL)
      CALL PSETC('PLOT_DIRECTION','HORIZONTAL')
      CALL PSETC('PLOT_START','BOTTOM')

c
      CALL PSETC('PAGE_FRAME','OFF')
      CALL PSETC('PAGE_FRAME_COLOUR','BLACK')
      CALL PSETC('PAGE_ID_LINE','OFF')
      CALL PSETC('SUBPAGE_FRAME_COLOUR','BLACK')
C
      CALL PSETC('LEGEND_BORDER','OFF')
      CALL PSETC('LEGEND_BORDER_COLOUR','BLACK')
      CALL PSETC('LEGEND_TEXT_COLOUR','BLACK')
c
c.... general text
c
      PXL=SPXL-eps
      PYL=SPYL/5-eps
      SBPXL=.95*PXL
      SBPXP=(PXL-SBPXL)/2
      SBPYL=.95*PYL
      SBPYP=(PYL-SBPYL)/2.
      SBPYP=0.
     
      CALL PSETC('SUBPAGE_FRAME','OFF')
c
      CALL PSETC('TEXT_MODE','POSITIONAL')
      CALL PSETR('TEXT_BOX_Y_POSITION',4*SPYL/5)
      CALL PSETR('TEXT_BOX_Y_LENGTH',SBPYL)
      CALL PSETR('TEXT_BOX_X_POSITION',SBPXP)
      CALL PSETR('TEXT_BOX_X_LENGTH',SBPXL)
      call psetr('subpage_top_position', SBPYL+2.5)
      call psetc('subpage_align_vertical', "top")
   
c      
      CALL PSETC('TEXT_COLOUR','BLACK')
      CALL PSETC('TEXT_JUSTIFICATION','LEFT')
      CALL PSETR('TEXT_FONT_SIZE',0.6)
      CALL PSETI('TEXT_LINE_COUNT',4)
     
      CALL PSETC('TEXT_LINE_1',CTITG1(1:lena(CTITG1)))
      CALL PSETC('TEXT_LINE_2',CTITG2(1:lena(CTITG2)))
      CALL PSETC('TEXT_LINE_3',CTITG3(1:lena(CTITG3)))
      if((boot.eq.1).and.(dia.ne.'ACP'))then
        CALL PSETC('TEXT_LINE_4',CTITG4(1:lena(CTITG4)))
      endif
      
      
      CALL PSETC('PAGE_FRAME','ON')
      CALL PSETC('SUBPAGE_FRAME','ON')

   

     
      CALL PSETC('SUBPAGE_MAP_PROJECTION','CYLINDRICAL')
      CALL PSETC('SUBPAGE_MAP_AREA_DEFINITION','CORNERS')
      CALL PSETR('SUBPAGE_LOWER_LEFT_LATITUDE',xlats)
      CALL PSETR('SUBPAGE_LOWER_LEFT_LONGITUDE',xlonw)
      CALL PSETR('SUBPAGE_UPPER_RIGHT_LATITUDE',xlatn)
      CALL PSETR('SUBPAGE_UPPER_RIGHT_LONGITUDE',xlone)
      CALL PSETC('MAP_COASTLINE_COLOUR','BLACK')
      CALL PSETC('MAP_COASTLINE_RESOLUTION','LOW')
      CALL PSETI('MAP_COASTLINE_THICKNESS',10)
      CALL PSETC('MAP_GRID','OFF')
      CALL PSETC('MAP_LABEL','OFF')
      CALL PSETC('INPUT_FIELD_SUBPAGE_MAPPING',
     >   'UPPER_LEFT')
c
      CALL PSETC('CONTOUR_METHOD','LINEAR')
      CALL PSETR('CONTOUR_LABEL_HEIGHT',0.3)
      CALL PSETC('CONTOUR_LABEL_FORMAT','F4.1')
      CALL PSETI('CONTOUR_LABEL_FREQUENCY',2)
      CALL PSETC('CONTOUR_LABEL','OFF')
      CALL PSETC('CONTOUR_HILO','OFF')
      CALL PSETC('CONTOUR_HIGHLIGHT','OFF')
c
c.... plot the field
c
C     CALL PSETC('CONTOUR_LEVEL_SELECTION_TYPE','INTERVAL')
C     CALL PSETR('CONTOUR_INTERVAL',0.2)
C     CALL PSETC('CONTOUR_SPLIT_LINE_PLOT','ON')
C     CALL PSETR('CONTOUR_SPLIT_LEVEL',0.)
C     CALL PSETI('CONTOUR_SPLIT_LINE_THICKNESS',5)
C     CALL PSETC('CONTOUR_SPLIT_LINE_STYLE','SOLID')
C     CALL PSETC('CONTOUR_ABOVE_LINE_STYLE','SOLID')
C     CALL PSETI('CONTOUR_ABOVE_LINE_THICKNESS',3)
C     CALL PSETC('CONTOUR_BELOW_LINE_STYLE','DASH')
C     CALL PSETI('CONTOUR_BELOW_LINE_THICKNESS',3)
      CALL PSETC('LEGEND','ON')
      CALL PSETC('CONTOUR_LEVEL_SELECTION_TYPE','LEVEL_LIST')
      CALL PSETR('CONTOUR_MIN_LEVEL',xmin)
      CALL PSETR('CONTOUR_MAX_LEVEL',xmax)
      CALL PSET1R('CONTOUR_LEVEL_LIST',llist,(nint+1))
      CALL PSETC('CONTOUR_SHADE','On')
      CALL PSETC('CONTOUR','Off')
      CALL PSETC('CONTOUR_SHADE_METHOD','AREA_FILL')
      CALL PSETC('CONTOUR_SHADE_TECHNIQUE','CELL_SHADING')
      CALL PSETR('CONTOUR_SHADE_CELL_RESOLUTION',5.)
      CALL PSETR('CONTOUR_SHADE_MIN_LEVEL',xmin)
      CALL PSETR('CONTOUR_SHADE_MAX_LEVEL',xmax)
C     CALL PSETR('CONTOUR_SHADE_MIN_LEVEL_DENSITY',50.)
C     CALL PSETR('CONTOUR_SHADE_MAX_LEVEL_DENSITY',300.)
C     CALL PSETC('CONTOUR_SHADE_MIN_LEVEL_COLOUR','BLUE')
C     CALL PSETC('CONTOUR_SHADE_MAX_LEVEL_COLOUR','RED')
C     CALL PSETC('CONTOUR_SHADE_COLOUR_DIRECTION','CLOCKWISE')
      CALL PSETC('CONTOUR_SHADE_LABEL_BLANKING','OFF')
      CALL PSETC('CONTOUR_SHADE_COLOUR_METHOD','LIST')
      CALL PSET1C('CONTOUR_SHADE_COLOUR_LIST',clist,nint)
c
     
      CALL PSETC('LEGEND_DISPLAY_TYPE','CONTINUOUS')
c
      CALL PSETR('INPUT_FIELD_SUPPRESS_BELOW',rmiss+eps)
      CALL PSETC ('INPUT_FIELD_ORGANIZATION',     'REGULAR')
      CALL PSETR('INPUT_FIELD_INITIAL_LATITUDE',firstlat)
      CALL PSETR('INPUT_FIELD_INITIAL_LONGITUDE',firstlon)
      CALL PSETR('INPUT_FIELD_LATITUDE_STEP',-1.*steplat)
      CALL PSETR('INPUT_FIELD_LONGITUDE_STEP',steplon)
      CALL PSET2R('INPUT_FIELD',field,nlon,nlat)
      write(*,*) ('call pset2r  ')
c
      CALL PCONT
c
      if((boot.eq.1).and.(dia.ne.'ACP'))then
        nsig=0
        do ilat=1,nlat
           do ilon=1,nlon
              if(field_sig(ilon,ilat).eq.1.)then
                xtest=plsi*field(ilon,ilat)
                if(xtest.ge.0.)nsig=nsig+1
              endif
           enddo
        enddo
        allocate (xpos(nsig),                                stat=istat)
        allocate (ypos(nsig),                                stat=istat)
        allocate (imar(1),                                   stat=istat)
        imar(1)=15
        nsig=0
        do ilat=1,nlat
           do ilon=1,nlon
              if(field_sig(ilon,ilat).eq.1.)then
                xtest=plsi*field(ilon,ilat)
                if(xtest.ge.0.)then
                  nsig=nsig+1
                  ypos(nsig)=firstlat-steplat*(ilat-1)
                  xpos(nsig)=firstlon+steplon*(ilon-1)
                endif
              endif
           enddo
        enddo
        CALL PSETC('LEGEND','ON')
        CALL PSETC('SYMBOL_POSITION_MODE','GEOGRAPHIC')
c        CALL PSETC('SYMBOL_QUALITY','HIGH')
        CALL PSETC('SYMBOL_TABLE_MODE','OFF')
        CALL PSETC('SYMBOL_TYPE','MARKER')
        CALL PSETR('SYMBOL_HEIGHT',0.05)
        CALL PSET1R('SYMBOL_INPUT_X_POSITION',XPOS,NSIG)
        CALL PSET1R('SYMBOL_INPUT_Y_POSITION',YPOS,NSIG)
        CALL PSET1I('SYMBOL_INPUT_MARKER_LIST',IMAR,1)
        CALL PSETC('SYMBOL_COLOUR','BLACK')
        CALL PSYMB
      endif
      CALL PCOAST
      CALL PTEXT
      
c
c.... plot land/sea mask
c
      CALL PSETC('CONTOUR_LINE_COLOUR','BLACK')
      CALL PSETC('LEGEND','ON')
      CALL PSETC('CONTOUR_SHADE','OFF')
      CALL PSETR('CONTOUR_MIN_LEVEL',1.)
      CALL PSETR('CONTOUR_MAX_LEVEL',1.)
      CALL PSETR('CONTOUR_INTERVAL',1.)
      CALL PSETI('CONTOUR_LINE_THICKNESS',2)
      CALL PSETC ('INPUT_FIELD_ORGANIZATION',     'REGULAR')
      CALL PSETR ('INPUT_FIELD_INITIAL_LONGITUDE',firstlon)
      CALL PSETR ('INPUT_FIELD_INITIAL_LATITUDE', firstlat)
      CALL PSETR ('INPUT_FIELD_LONGITUDE_STEP',   steplon)
      CALL PSETR ('INPUT_FIELD_LATITUDE_STEP',-1.*steplat)
      CALL PSET2R('INPUT_FIELD',mask,nlon,nlat)
      CALL PCONT
      CALL PNEW('PAGE')
c
      CALL PNEW('SUPER_PAGE')
c
      return
      end
