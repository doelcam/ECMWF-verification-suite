c
c----------------------------------------------------------------------c
c     PROGRAM plot_trd                    F. Doblas-Reyes 02-Jul-2008  c
c                                                                      c
c     PURPOSE:                                                         c
c     Plots maps of the slope of a linear trend normalized by the sd   c
c     of the residuals                                                 c
c                                                                      c
c     INPUT:                                                           c
c     TRD_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_III-EEE.grb              c
c                                                        GRIB files    c
c                                                                      c
c     OUTPUT:                                                          c
c     TRD_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_III-EEE.ps               c
c                                                          ps files    c
c                                                                      c
c     USAGE:                                                           c
c     plot_trd.x < nlist                                               c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 plot_trd.f tools.f tools_plot.f -o plot_trd.x      c
c            $MAGLIB $EMOSLIB $NAGLIB                                  c
c                                                                      c
c     MODS:                                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      PROGRAM plot_trd
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: s(:), d(:)
      real, allocatable :: psec4(:)
      integer, allocatable :: lsm(:)
      real, allocatable :: field(:,:), mask(:,:)
      real, allocatable :: field_tmp(:,:)
      character*20, allocatable :: clist(:)
      real, allocatable :: llist(:)
      real, allocatable :: ypos(:), xpos(:)
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
c
c.... other definitions
c
      real topth, botth
      real spxl, spyl, pxl, pyl, sbpxl, sbpxp, sbpyl, sbpyp
      real xlatn, xlats, xlonw, xlone, xloin
      real xmax, xmin
      real rmiss
      integer nx, ny, nens, iyy1, iyy2, imm, idd, itt, cros, anin
      integer ipar, ilev, nf1, nf2
      integer iunit1, iunit2, nxny, exptl, lena, istat
      integer nlon, nlat, i, ilon, ilat, ilonw
      integer nint
      character*21 expt
      character*4 dia
      character*100 yfile, yifile, yofile, yrfile
      character*120 ctitg1, ctitg2, ctitg3, ctitg4
      character*20 start
      character*40 variable, region
      character*50 diagnostic
      character*20 form1
c
      namelist /control/   nx,   ny, nens,
     >                   iyy1, iyy2,  imm,  idd,  itt,
     >                   cros, ipar, ilev, anin,  nf1,  nf2,
     >                   expt,  dia
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
      write(*,*)' dia: ',dia
c
      rmiss=-1.e30
      nxny=nx*ny
      exptl=lena(expt)
c
      allocate (d(nxny),                                     stat=istat)
      allocate (s(nxny),                                     stat=istat)
      allocate (psec4(nxny),                                 stat=istat)
      allocate (lsm(nxny),                                   stat=istat)
c
c.... open input file
c
      write(form1,'(a,i2.2,a)')'(a',exptl,')'
      yifile='TRD1_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE'
      write(yifile(       1:       4),'(a4)')dia
      write(yifile(       6: 5+exptl),form1)expt(1:exptl)
      write(yifile( 6+exptl: 6+exptl),'(a1)')'_'
      write(yifile( 7+exptl:10+exptl),'(i4.4)')iyy1
      write(yifile(11+exptl:11+exptl),'(a1)')'-'
      write(yifile(12+exptl:15+exptl),'(i4.4)')iyy2
      write(yifile(16+exptl:16+exptl),'(a1)')'_'
      write(yifile(17+exptl:18+exptl),'(i2.2)')imm
      write(yifile(19+exptl:20+exptl),'(i2.2)')idd
      write(yifile(21+exptl:22+exptl),'(i2.2)')itt
      write(yifile(23+exptl:25+exptl),'(a3)')'_CV'
      write(yifile(26+exptl:27+exptl),'(i2.2)')cros
      write(yifile(28+exptl:29+exptl),'(a2)')'_I'
      write(yifile(30+exptl:30+exptl),'(i1.1)')anin
      write(yifile(31+exptl:31+exptl),'(a1)')'_'
      write(yifile(32+exptl:34+exptl),'(i3.3)')ipar
      write(yifile(35+exptl:35+exptl),'(a1)')'_'
      write(yifile(36+exptl:38+exptl),'(i3.3)')ilev
      write(yifile(39+exptl:39+exptl),'(a1)')'_'
      write(yifile(40+exptl:42+exptl),'(i3.3)')nf1
      write(yifile(43+exptl:43+exptl),'(a1)')'-'
      write(yifile(44+exptl:46+exptl),'(i3.3)')nf2
      yfile=yifile(1:lena(yifile))//'.grb'
      write(*,*)'open input file: ',yfile
      kret=0
      call pbopen(iunit1,yfile,'r',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
c
c.... reading data
c
      call read1grb(iunit1, nxny, psec4,
     >   ksec0, ksec1, ksec2, ksec3,
     >   ksec4, psec2, psec3)
      nlon=ksec2(2)
      nlat=ksec2(3)
      if(nlon*nlat.ne.nxny)then
        write(*,*)'The file contains the wrong number of points'
        goto 998
      endif
      allocate (field(nlon,nlat),                        stat=istat)
      allocate (field_tmp(nlon,nlat),                    stat=istat)
      do i=1,nxny
         ilat=int((i-1)/nlon)+1
         ilon=i-(ilat-1)*nlon
         field(ilon,ilat)=100*psec4(i)
      enddo
      xlatn=ksec2(4)/1000.
      xlonw=ksec2(5)/1000.
      xlats=ksec2(7)/1000.
      xlone=ksec2(8)/1000.
      xloin=360./nlon
c
c.... start from 60E
c
      ilonw=int(60./xloin)
      xlonw=xlonw+60.
      xlone=xlonw-xloin
      write(*,*)'ilonw=',ilonw
      write(*,*)'xlonw=',xlonw
      write(*,*)'xlone=',xlone
      do ilat=1,nlat
         do ilon=1,ilonw
            field_tmp(ilon+nlon-ilonw,ilat)=field(ilon,ilat)
         enddo
         do ilon=ilonw+1,nlon
            field_tmp(ilon-ilonw,ilat)=field(ilon,ilat)
         enddo
         do ilon=1,nlon
            field(ilon,ilat)=field_tmp(ilon,ilat)
         enddo
      enddo
c
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
      yofile='psfiles/'//yifile(1:lena(yifile))//'.ps'
      write(*,*)'Output file ',yofile(1:lena(yofile)),
     >   ' is a postscript file'
C
C     INITIALISATION DE MAGICS
C
      CALL POPEN
      CALL PSETC('PS_DEVICE','ps_a4')
      CALL PSETC('PS_FILE_NAME',yofile)
      CALL PSETC('LAYOUT','POSITIONAL')
C
C     PLOTTING SECTION
C
      SPXL=29.5
      SPYL=21.
      CALL PSETR('SUPER_PAGE_X_LENGTH',SPXL)
      CALL PSETR('SUPER_PAGE_Y_LENGTH',SPYL)
      CALL PSETC('PLOT_DIRECTION','HORIZONTAL')
      CALL PSETC('PLOT_START','BOTTOM')
      CALL PSETR('PAGE_X_GAP',0.)
      CALL PSETR('PAGE_Y_GAP',0.)
c
      CALL PSETC('PAGE_FRAME_COLOUR','BLACK')
      CALL PSETC('PAGE_ID_LINE','OFF')
      CALL PSETC('SUBPAGE_FRAME_COLOUR','BLACK')
c
      CALL PSETC('LEGEND_BORDER','OFF')
      CALL PSETC('LEGEND_BORDER_COLOUR','BLACK')
      CALL PSETC('LEGEND_TEXT_COLOUR','BLACK')
      CALL PSETC('LEGEND_TEXT_QUALITY','HIGH')
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
      CALL PSETR('PAGE_X_LENGTH',PXL)
      CALL PSETR('PAGE_Y_LENGTH',PYL)
      CALL PSETR('PAGE_X_POSITION',0.)
      CALL PSETR('PAGE_Y_POSITION',4*SPYL/5)
      CALL PSETC('PAGE_FRAME','OFF')
      CALL PSETR('SUBPAGE_Y_POSITION',SBPYP)
      CALL PSETR('SUBPAGE_Y_LENGTH',SBPYL)
      CALL PSETR('SUBPAGE_X_POSITION',SBPXP)
      CALL PSETR('SUBPAGE_X_LENGTH',SBPXL)
      CALL PSETC('SUBPAGE_FRAME','OFF')
c
c     CALL PSETR('SUBPAGE_TOP_POSITION',SBPYL+1.5)
c     CALL PSETC('SUBPAGE_ALIGN_VERTICAL','TOP')
      CALL PSETC('TEXT_MODE','POSITIONAL')
      CALL PSETR('TEXT_BOX_Y_POSITION',SBPYP)
      CALL PSETR('TEXT_BOX_Y_LENGTH',SBPYL)
      CALL PSETR('TEXT_BOX_X_POSITION',SBPXP)
      CALL PSETR('TEXT_BOX_X_LENGTH',SBPXL)
      CALL PSETC('TEXT_COLOUR','BLACK')
      CALL PSETC('TEXT_JUSTIFICATION','LEFT')
      CALL PSETR('TEXT_REFERENCE_CHARACTER_HEIGHT',0.5)
      CALL PSETC('TEXT_QUALITY','HIGH')
      CALL PSETI('TEXT_LINE_COUNT',4)
      if(expt.eq.'refe')then
        WRITE(CTITG1,'(A,A,A)')
     >     diagnostic(1:lena(diagnostic)),' for ',
     >     'ERA40/ERAInt'
        if(ipar.eq.228)then
          WRITE(CTITG1,'(A,A,A)')
     >       diagnostic(1:lena(diagnostic)),' for ',
     >       'GPCP'
        endif
      else
      WRITE(CTITG1,'(A,A,A,A,i2,A)')
     >   diagnostic(1:lena(diagnostic)),' for ',
     >   expt(1:lena(expt)),' with ',
     >   nens,' ensemble members'
      endif
      WRITE(CTITG2,'(A)')variable
      WRITE(CTITG3,'(A,I4.4,A,I4.4,A,A,A,i2,A,i2)')
     >   'Hindcast period ',iyy1,'-',iyy2,
     >   ' with start in ',start(1:lena(start)),
     >   ' and averaging period ',nf1,' to ',nf2
      write(*,*)CTITG1
      write(*,*)CTITG2
      write(*,*)CTITG3
      CALL PSETC('TEXT_LINE_1',CTITG1(1:lena(CTITG1)))
      CALL PSETC('TEXT_LINE_2',CTITG2(1:lena(CTITG2)))
      CALL PSETC('TEXT_LINE_3',CTITG3(1:lena(CTITG3)))
      CALL PTEXT
      CALL PNEW('PAGE')
c
c.... plotting the map
c
      PXL=SPXL-eps
      PYL=4*SPYL/5-eps
      SBPXL=.97*PXL
      SBPXP=(PXL-SBPXL)/2
      SBPYL=.97*PYL
      SBPYP=(PYL-SBPYL)/2.
c
      CALL PSETR('PAGE_X_LENGTH',PXL)
      CALL PSETR('PAGE_Y_LENGTH',PYL)
      CALL PSETR('PAGE_X_POSITION',0.)
      CALL PSETR('PAGE_Y_POSITION',0.)
      CALL PSETC('PAGE_FRAME','OFF')
      CALL PSETR('SUBPAGE_Y_POSITION',SBPYP)
      CALL PSETR('SUBPAGE_Y_LENGTH',SBPYL)
      CALL PSETR('SUBPAGE_X_POSITION',SBPXP)
      CALL PSETR('SUBPAGE_X_LENGTH',SBPXL)
      CALL PSETC('SUBPAGE_FRAME','ON')
c
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
      CALL PSETC('CONTOUR_LEVEL_SELECTION_TYPE','LEVEL_LIST')
      CALL PSETR('CONTOUR_MIN_LEVEL',xmin)
      CALL PSETR('CONTOUR_MAX_LEVEL',xmax)
      CALL PSET1R('CONTOUR_LEVEL_LIST',llist,(nint+1))
      CALL PSETC('CONTOUR_SHADE','ON')
      CALL PSETC('CONTOUR_SHADE_METHOD','AREA_FILL')
      CALL PSETC('CONTOUR_SHADE_TECHNIQUE','CELL_SHADING')
      CALL PSETR('CONTOUR_SHADE_CELL_RESOLUTION',5.)
      CALL PSETR('CONTOUR_SHADE_MIN_LEVEL',xmin)
      CALL PSETR('CONTOUR_SHADE_MAX_LEVEL',xmax)
      CALL PSETC('CONTOUR_SHADE_LABEL_BLANKING','OFF')
      CALL PSETC('CONTOUR_SHADE_COLOUR_METHOD','LIST')
      CALL PSET1C('CONTOUR_SHADE_COLOUR_LIST',clist,nint)
c
      CALL PSETC('LEGEND','ON')
      CALL PSETC('LEGEND_DISPLAY_TYPE','CONTINUOUS')
c
      CALL PSETC('INPUT_FIELD_ORGANIZATION','REGULAR')
      CALL PSETC('INPUT_FIELD_ORGANIZATION','FITTED')
      CALL PSETC('INPUT_FIELD_SUBPAGE_MAPPING',
     >   'UPPER_LEFT')
c     CALL PSETR('INPUT_FIELD_SUPPRESS_BELOW',rmiss+eps)
      CALL PSET2R('INPUT_FIELD',field,nlon,nlat)
      CALL PCONT
      CALL PCOAST
c
c.... plot land/sea mask
c
      CALL PSETC('CONTOUR_LINE_COLOUR','BLACK')
      CALL PSETC('LEGEND','OFF')
      CALL PSETC('CONTOUR_SHADE','OFF')
      CALL PSETR('CONTOUR_MIN_LEVEL',1.)
      CALL PSETR('CONTOUR_MAX_LEVEL',1.)
      CALL PSETR('CONTOUR_INTERVAL',1.)
      CALL PSETI('CONTOUR_LINE_THICKNESS',2)
      CALL PSET2R('INPUT_FIELD',mask,nlon,nlat)
c     CALL PCONT
      CALL PNEW('PAGE')
c
      CALL PCLOSE
c
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
c
      end
