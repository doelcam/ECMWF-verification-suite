c
c----------------------------------------------------------------------c
c     PROGRAM plot_ts                     P. Doblas-Reyes 03-Apr-2008  c
c                                                                      c
c     PURPOSE:                                                         c
c     Plots time series with the corresponding scores                  c
c                                                                      c
c     INPUT:                                                           c
c     TS_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE_NAME            c
c                                                       ASCII files    c
c                                                                      c
c     OUTPUT:                                                          c
c     TS_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE_NAME.ps         c
c                                                            ps files  c
c                                                                      c
c     USAGE:                                                           c
c     plot_ts.x < nlist                                                c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 plot_ts.f tools.f tools_plot.f                     c
c     -o plot_ts.x $MAGLIB $EMOSLIB $NAGLIB                            c
c                                                                      c
c     MODS:                                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      PROGRAM plot_ts
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: r(:), h(:,:), hh(:,:), hm(:), hhm(:)
      real, allocatable :: x(:), y(:), s(:), d(:), t(:)
      real, allocatable :: mmin(:), mmax(:), mut(:), mlt(:)
c
c.... hard coded field definitions
c
      integer nts_max
      parameter(nts_max=80)
      character*6 namet(nts_max)
      integer par(nts_max), lev(nts_max)
      integer yy1(nts_max), yy2(nts_max)
      integer ncla
      parameter(ncla=3)
      real tru(ncla-1), thu(ncla-1)
      real thi(ncla-1)
      real eps
      parameter(eps=0.1)
c
c.... other definitions
c
      real topth, botth
      real spxl, spyl, pxl, pyl, sbpxl, sbpxp, sbpyl, sbpyp
      real ymax, ymin, yint, ymaxr, yminr, yintr
      real ratio, varn, varni, rms, rmsi
      real cor, sig, cori, sigi, snr, sigr, snri, sigri
      real rpssu, rpsspu, rpss, rpssp, rpssdu, rpssdpu, rpssd, rpssdp
      integer nens, nf1, nf2, imm, idd, itt, cros, anin, nts
      integer ipar, ilev, iyy1, iyy2, icla
      integer iunit1, iunit2, exptl, lena, istat
      integer i, it, nyear, iy, idat, ityp, iens, ut, lt
      character*21 expt, expti
      character*100 yfile, yifile, yofile, yrfile
      character*120 ctitg1, ctitg2, ctitg3, ctitg4, ctitg5, ctitg6
      character*60 title, yaxti
      character*20 start
      character*20 form1
c
      namelist /control/ nens,  imm,  idd,  itt,
     >                   cros, anin,  nf1,  nf2,
     >                   expt,  nts
      namelist /timeseries/
     >                  namet,  par,  lev,  yy1, yy2
c
c.... set default values and read input namelist
c
      nens = 9
      imm  = 11
      idd  = 1
      itt  = 0
      cros = 1
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
      write(*,*)'nens: ',nens
      write(*,*)' imm: ',imm
      write(*,*)' idd: ',idd
      write(*,*)' itt: ',itt
      write(*,*)'cros: ',cros
      write(*,*)'anin: ',anin
      write(*,*)' nf1: ',nf1
      write(*,*)' nf2: ',nf2
      write(*,*)'expt: ',expt
      write(*,*)' nts: ',nts
      read(5,timeseries)
      write(*,*)'namet: ',(namet(i),i=1,nts)
      write(*,*)' par: ',(par(i),i=1,nts)
      write(*,*)' lev: ',(lev(i),i=1,nts)
      write(*,*)' yy1: ',(yy1(i),i=1,nts)
      write(*,*)' yy2: ',(yy2(i),i=1,nts)
c
      exptl=lena(expt)
c
c.... loop over time series to plot
c
      do it=1,nts
         iyy1=yy1(it)
         iyy2=yy2(it)
         ipar=par(it)
         ilev=lev(it)
         nyear=iyy2-iyy1+1
c
c.... allocate fields
c
         allocate (r(nyear),                                 stat=istat)
         allocate (h(nyear,nens),                            stat=istat)
         allocate (hh(nyear,nens),                           stat=istat)
         allocate (hm(nyear),                                stat=istat)
         allocate (hhm(nyear),                               stat=istat)
         allocate (mmin(nyear),                              stat=istat)
         allocate (mmax(nyear),                              stat=istat)
         allocate (mut(nyear),                               stat=istat)
         allocate (mlt(nyear),                               stat=istat)
         allocate (t(nyear),                                 stat=istat)
c
c.... open input file
c
         do idat=1,2
            expti=expt
            if(idat.eq.1)then
              expti = 'refe'
            endif
            exptl=lena(expti)
            write(form1,'(a,i2.2,a)')'(a',exptl,')'
            yifile=
     >        'TS_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE'
            write(yifile(       4: 3+exptl),form1)expti(1:exptl)
            write(yifile( 4+exptl: 4+exptl),'(a1)')'_'
            write(yifile( 5+exptl: 8+exptl),'(i4.4)')iyy1
            write(yifile( 9+exptl: 9+exptl),'(a1)')'-'
            write(yifile(10+exptl:13+exptl),'(i4.4)')iyy2
            write(yifile(14+exptl:14+exptl),'(a1)')'_'
            write(yifile(15+exptl:16+exptl),'(i2.2)')imm
            write(yifile(17+exptl:18+exptl),'(i2.2)')idd
            write(yifile(19+exptl:20+exptl),'(i2.2)')itt
            write(yifile(21+exptl:23+exptl),'(a3)')'_CV'
            write(yifile(24+exptl:25+exptl),'(i2.2)')cros
            write(yifile(26+exptl:27+exptl),'(a2)')'_I'
            if(idat.eq.2)then
              write(yifile(28+exptl:28+exptl),'(i1.1)')0
            else
              write(yifile(28+exptl:28+exptl),'(i1.1)')anin
            endif
            write(yifile(29+exptl:29+exptl),'(a1)')'_'
            write(yifile(30+exptl:32+exptl),'(i3.3)')ipar
            write(yifile(33+exptl:33+exptl),'(a1)')'_'
            write(yifile(34+exptl:36+exptl),'(i3.3)')ilev
            write(yifile(37+exptl:37+exptl),'(a1)')'_'
            write(yifile(38+exptl:40+exptl),'(i3.3)')nf1
            write(yifile(41+exptl:41+exptl),'(a1)')'-'
            write(yifile(42+exptl:44+exptl),'(i3.3)')nf2
            yifile=yifile(1:lena(yifile))//'_'//namet(it)
            write(*,*)'open input file: ',yifile(1:lena(yifile))
            open(10,file=yifile,form='formatted',status='old')
c
c.... reading data
c
            if(idat.eq.1)then
              write(form1,'(a,i1.1,a)')'(4x,e12.4)'
              write(*,*)form1
              do iy=1,nyear
                 read(10,form1)r(iy)
              enddo
            else
              write(form1,'(a,i2.2,a)')'(4x,',nens+1,'e12.3)'
              if((nens+1).ge.100)write(form1,'(a,i3.3,a)')
     >           '(4x,',nens+1,'e12.3)'
              write(*,*)form1
              do iy=1,nyear
                 read(10,form1)(h(iy,iens),iens=1,nens),hm(iy)
              enddo
c
c.... reading scores
c
c.... ratio:   ratio of standard deviations (model vs reference)
c.... varn:    spread/RMSE
c.... varni:   spread/RMSE of inflated values
c.... rms:     root mean square error
c.... rmsi:    root mean square error of the inflated values
c.... cor:     ensemble mean correlation
c.... sig:     statistical significance of the correlation
c.... cori:    ensemble mean correlation of the inflated values
c.... sigi:    statistical significance of the correlation of the inflated values
c.... snr:     signal-to-noise ratio
c.... sigr:    signal-to-noise ratio statistical confidence
c.... snri:    signal-to-noise ratio of the inflated values
c.... sigri:   signal-to-noise ratio statistical confidence for
c....               the inflated values
c.... rpssu:   RPSS of the original values
c.... rpsspu:  RPSS statistical significance of the original values
c.... rpss:    RPSS of the inflated values
c.... rpssp:   RPSS statistical significance of the inflated values
c.... rpssdu:  RPSSd of the original values
c.... rpssdpu: RPSSd statistical significance of the original values
c.... rpssd:   RPSSd of the inflated values
c.... rpssdp:  RPSSd statistical significance of the inflated values
c
              read(10,'(f12.2,21e12.4)')
     >           ratio,varn,varni,rms,rmsi,cor,sig,cori,sigi,
     >           snr,sigr,snri,sigri,
     >           rpssu,rpsspu,rpss,rpssp,rpssdu,rpssdpu,rpssd,rpssdp
              write(form1,'(a,i1.1,a)')'(',3*(ncla-1),'e12.4)'
              read(10,form1)((tru(icla),thu(icla)),icla=1,ncla-1),
     >           (thi(icla),icla=1,ncla-1)
c
              write(form1,'(a,i2.2,a)')'(4x,',nens+1,'e12.3)'
              if((nens+1).ge.100)write(form1,'(a,i3.3,a)')
     >           '(4x,',nens+1,'e12.3)'
              do iy=1,nyear
                 read(10,form1)(hh(iy,iens),iens=1,nens),hhm(iy)
              enddo
            endif
            close(10)
         enddo !idat
c
c.... looking for the title
c
         call tseries(namet(it),imm,title,yaxti,ymax,ymin,yint,start)
c
c.... open output file
c
         yofile='psfiles/'//yifile(1:lena(yifile))//'.ps'
         write(*,*)'Output file ',yofile(1:lena(yofile)),
     >      ' is a postscript file'
C
C     INITIALISATION DE MAGICS
C
         CALL POPEN
         CALL PSETC('PS_DEVICE','ps_a4')
         CALL PSETC('PS_FILE_NAME',yofile)
C        CALL PSETC('WORKSTATION_1','PS_COL_A4_VERTICAL')
C        CALL PSETC('WORKSTATION_1','PS_COL_A4_HORIZONTAL')
         CALL PSETC('LAYOUT','POSITIONAL')
C
C     PLOTTING SECTION
C
         SPXL=29.5
         SPYL=21.
         CALL PSETR('SUPER_PAGE_X_LENGTH',SPXL)
         CALL PSETR('SUPER_PAGE_Y_LENGTH',SPYL)
         CALL PSETC('PLOT_DIRECTION','HORIZONTAL')
c        CALL PSETC('PLOT_DIRECTION','VERTICAL')
c        CALL PSETC('PLOT_START','TOP')
         CALL PSETC('PLOT_START','BOTTOM')
         CALL PSETR('PAGE_X_GAP',0.)
         CALL PSETR('PAGE_Y_GAP',0.)
c
         CALL PSETC('PAGE_FRAME','OFF')
         CALL PSETC('PAGE_FRAME_COLOUR','BLACK')
         CALL PSETC('PAGE_ID_LINE','OFF')
         CALL PSETC('SUBPAGE_FRAME_COLOUR','BLACK')
c
         CALL PSETC('LEGEND_BORDER','OFF')
         CALL PSETC('LEGEND_BORDER_COLOUR','BLACK')
         CALL PSETC('LEGEND_TEXT_COLOUR','BLACK')
         CALL PSETC('LEGEND_TEXT_QUALITY','HIGH')
         CALL PSETR('LEGEND_TEXT_MAXIMUM_HEIGHT',0.3)
c
         CALL PSETI('AXIS_LINE_THICKNESS',4)
         CALL PSETR('AXIS_TITLE_HEIGHT',0.30)
         CALL PSETC('AXIS_TITLE_QUALITY','HIGH')
         CALL PSETR('AXIS_TICK_SIZE',0.1)
         CALL PSETR('AXIS_TICK_LABEL_HEIGHT',0.25)
         CALL PSETC('AXIS_TICK_LABEL_QUALITY','HIGH')
         CALL PSETR('AXIS_MINOR_TICK_SIZE',0.05)
         CALL PSETR('AXIS_MINOR_TICK_MIN_GAP',0.05)
         CALL PSETC('AXIS_GRID_LINE_STYLE','DOT')
         CALL PSETC('AXIS_GRID_COLOUR','BLACK')
c
c.... loop for original and inflated values
c
         do ityp=1,2
c
c.... general text
c
            PXL=3*SPXL/5-eps
            PYL=SPYL/5-eps
            SBPXL=.95*PXL
            SBPXP=(PXL-SBPXL)/2
            SBPYL=.95*PYL
            SBPYP=(PYL-SBPYL)/2.
            SBPYP=0.
c           write(*,*)SBPYP,SBPYL,SBPXP,SBPXL
            CALL PSETR('PAGE_X_LENGTH',PXL)
            CALL PSETR('PAGE_Y_LENGTH',PYL)
            CALL PSETR('PAGE_X_POSITION',0.)
            CALL PSETR('PAGE_Y_POSITION',4*SPYL/5)
            CALL PSETR('SUBPAGE_Y_POSITION',SBPYP)
            CALL PSETR('SUBPAGE_Y_LENGTH',SBPYL)
            CALL PSETR('SUBPAGE_X_POSITION',SBPXP)
            CALL PSETR('SUBPAGE_X_LENGTH',SBPXL)
            CALL PSETC('SUBPAGE_FRAME','OFF')
c
C           CALL PSETC('TEXT_MODE','TITLE')
            CALL PSETC('TEXT_MODE','POSITIONAL')
            CALL PSETR('TEXT_BOX_Y_POSITION',SBPYP)
            CALL PSETR('TEXT_BOX_Y_LENGTH',SBPYL)
            CALL PSETR('TEXT_BOX_X_POSITION',SBPXP)
            CALL PSETR('TEXT_BOX_X_LENGTH',SBPXL)
            CALL PSETC('TEXT_COLOUR','BLACK')
            CALL PSETC('TEXT_JUSTIFICATION','LEFT')
            CALL PSETR('TEXT_REFERENCE_CHARACTER_HEIGHT',0.6)
            CALL PSETC('TEXT_QUALITY','HIGH')
            CALL PSETI('TEXT_LINE_COUNT',4)
            WRITE(CTITG1,'(A)')title(1:lena(title))
            if(ityp.eq.2)WRITE(CTITG1,'(a,a)')title(1:lena(title)),
     >         ' (inflated)'
            WRITE(CTITG2,'(A,A,i3,A)')
     >      expt(1:lena(expt)),' with ',
     >      nens,' ensemble members'
            WRITE(CTITG3,'(A,I4.4,A,I4.4)')
     >         'Hindcast period ',iyy1,'-',iyy2
            form1='(A,A,A,i2,A,i2)'
            if(nf2.gt.99)form1='(A,A,A,i2,A,i3)'
            WRITE(CTITG4,form1)
     >         'Start date ',start(1:lena(start)),
     >         ' and fcst. time ',nf1,' to ',nf2
            write(*,*)CTITG1
            write(*,*)CTITG2
            write(*,*)CTITG3
            write(*,*)CTITG4
            CALL PSETC('TEXT_LINE_1',CTITG1(1:lena(CTITG1)))
            CALL PSETC('TEXT_LINE_2',CTITG2(1:lena(CTITG2)))
            CALL PSETC('TEXT_LINE_3',CTITG3(1:lena(CTITG3)))
            CALL PSETC('TEXT_LINE_4',CTITG4(1:lena(CTITG4)))
            CALL PTEXT
            CALL PNEW('PAGE')
c
c.... scores
c
            PXL=2*SPXL/5-eps
            PYL=SPYL/5-eps
            SBPXL=.95*PXL
            SBPXP=(PXL-SBPXL)/2
            SBPYL=.95*PYL
            SBPYP=(PYL-SBPYL)/2.
            SBPYP=0.
c           write(*,*)SBPYP,SBPYL,SBPXP,SBPXL
            CALL PSETR('PAGE_X_LENGTH',PXL)
            CALL PSETR('PAGE_Y_LENGTH',PYL)
            CALL PSETR('PAGE_X_POSITION',3*SPXL/5)
            CALL PSETR('PAGE_Y_POSITION',4*SPYL/5)
            CALL PSETR('SUBPAGE_Y_POSITION',SBPYP)
            CALL PSETR('SUBPAGE_Y_LENGTH',SBPYL)
            CALL PSETR('SUBPAGE_X_POSITION',SBPXP)
            CALL PSETR('SUBPAGE_X_LENGTH',SBPXL)
            CALL PSETC('SUBPAGE_FRAME','OFF')
c
C           CALL PSETC('TEXT_MODE','TITLE')
            CALL PSETC('TEXT_MODE','POSITIONAL')
            CALL PSETR('TEXT_BOX_Y_POSITION',SBPYP)
            CALL PSETR('TEXT_BOX_Y_LENGTH',SBPYL)
            CALL PSETR('TEXT_BOX_X_POSITION',SBPXP)
            CALL PSETR('TEXT_BOX_X_LENGTH',SBPXL)
            CALL PSETC('TEXT_COLOUR','BLACK')
            CALL PSETC('TEXT_JUSTIFICATION','LEFT')
            CALL PSETR('TEXT_REFERENCE_CHARACTER_HEIGHT',0.35)
            CALL PSETC('TEXT_QUALITY','HIGH')
            CALL PSETI('TEXT_LINE_COUNT',6)
            WRITE(CTITG1,'(a,f4.2)')'Ratio of sd (model/ref): ',ratio
            WRITE(CTITG2,'(a,f4.2)')'Ratio spread/RMSE:      ',varn
            if(ityp.eq.2)WRITE(CTITG2,'(a,f4.2)')
     >         'Ratio spread/RMSE:      ',varni
            WRITE(CTITG3,'(a,f5.2,a,f4.2,a)')'Ens. mean correlation: ',
     >         cor,' (',sig,')'
            if(ityp.eq.2)
     >         WRITE(CTITG3,'(a,f5.2,a,f4.2,a)')
     >         'Ens. mean correlation: ',cori,' (',sigi,')'
            WRITE(CTITG4,'(a,f4.2,a,f4.2,a)')
     >         'SNR:                                ',
     >         snr,' (',sigr,')'
            if(ityp.eq.2)WRITE(CTITG4,'(a,f4.2,a,f4.2,a)')
     >         'SNR:                                ',
     >         snri,' (',sigri,')'
            WRITE(CTITG5,'(a,f5.2,a,f4.2,a)')
     >         'RPSS:                             ',
     >         rpssu,' (',rpsspu,')'
            if(ityp.eq.2)WRITE(CTITG5,'(a,f5.2,a,f4.2,a)')
     >         'RPSS:                             ',
     >         rpss,' (',rpssp,')'
            WRITE(CTITG6,'(a,f5.2,a,f4.2,a)')
     >         'RPSSd:                           ',
     >         rpssdu,' (',rpssdpu,')'
            if(ityp.eq.2)WRITE(CTITG6,'(a,f5.2,a,f4.2,a)')
     >         'RPSSd:                           ',
     >         rpssd,' (',rpssdp,')'
            write(*,*)CTITG1
            write(*,*)CTITG2
            write(*,*)CTITG3
            write(*,*)CTITG4
            write(*,*)CTITG5
            write(*,*)CTITG6
            CALL PSETC('TEXT_LINE_1',CTITG1(1:lena(CTITG1)))
            CALL PSETC('TEXT_LINE_2',CTITG2(1:lena(CTITG2)))
            CALL PSETC('TEXT_LINE_3',CTITG3(1:lena(CTITG3)))
            CALL PSETC('TEXT_LINE_4',CTITG4(1:lena(CTITG4)))
            CALL PSETC('TEXT_LINE_5',CTITG5(1:lena(CTITG5)))
            CALL PSETC('TEXT_LINE_6',CTITG6(1:lena(CTITG6)))
            CALL PTEXT
            CALL PNEW('PAGE')
c
c.... plotting the time series
c
            PXL=SPXL-eps
            PYL=4*SPYL/5-eps
            SBPXL=.85*PXL
            SBPXP=(PXL-SBPXL)/2
            SBPYL=.75*PYL
            SBPYP=(PYL-SBPYL)/2.
c           write(*,*)SBPYP,SBPYL,SBPXP,SBPXL
            CALL PSETR('PAGE_X_LENGTH',PXL)
            CALL PSETR('PAGE_Y_LENGTH',PYL)
            CALL PSETR('PAGE_X_POSITION',0.)
            CALL PSETR('PAGE_Y_POSITION',0.)
            CALL PSETR('SUBPAGE_Y_POSITION',SBPYP)
            CALL PSETR('SUBPAGE_Y_LENGTH',SBPYL)
            CALL PSETR('SUBPAGE_X_POSITION',SBPXP)
            CALL PSETR('SUBPAGE_X_LENGTH',SBPXL)
            CALL PSETC('SUBPAGE_FRAME','ON')
c
            CALL PSETR('AXIS_TITLE_HEIGHT',0.30)
            CALL PSETR('AXIS_TICK_LABEL_HEIGHT',0.30)
            CALL PSETC('AXIS_TICK_LABEL_FORMAT','F3.1')
c
            call tseries(namet(it),imm,title,yaxti,ymax,ymin,yint,start)
            ymaxr=ymax
            yminr=ymin
            yintr=yint
            allocate (x((nens+1)*nyear),                     stat=istat)
            allocate (y((nens+1)*nyear),                     stat=istat)
            i=0
            do iy=1,nyear
               do iens=1,nens
                  i=i+1
                  x(i)=h(iy,iens)
                  if(ityp.eq.2)x(i)=hh(iy,iens)
               enddo
               i=i+1
               x(i)=r(iy)
            enddo
            call sort(1,(nens+1)*nyear,(nens+1)*nyear,x,y)
            if(abs(y(1)).lt.abs(ymin/2.))then
              yminr=ymin/2.
              yintr=yint/2.
            endif
            if(abs(y(1)).lt.abs(ymin/3.))then
              yminr=ymin/3.
              yintr=yint/2.
            endif
            if(abs(y(1)).lt.abs(ymin/4.))then
              yminr=ymin/4.
              yintr=yint/4.
            endif
            if(abs(y(1)).lt.abs(ymin/5.))then
              yminr=ymin/5.
              yintr=yint/4.
            endif
            if(abs(y(1)).gt.abs(ymin))then
              yminr=ymin*1.5
              yintr=yint
            endif
            if(abs(y(1)).gt.abs(1.5*ymin))then
              yminr=ymin*2.0
              yintr=yint
            endif
            if(y((nens+1)*nyear).lt.ymax/2.)then
              ymaxr=ymax/2.
              yintr=yint/2.
            endif
            if(y((nens+1)*nyear).lt.ymax/3.)then
              ymaxr=ymax/3.
              yintr=yint/2.
            endif
            if(y((nens+1)*nyear).lt.ymax/4.)then
              ymaxr=ymax/4.
              yintr=yint/4.
            endif
            if(y((nens+1)*nyear).lt.ymax/5.)then
              ymaxr=ymax/5.
              yintr=yint/4.
            endif
            if(y((nens+1)*nyear).gt.ymax)then
              ymaxr=ymax*1.5
              yintr=yint
            endif
            if(y((nens+1)*nyear).gt.(1.5*ymax))then
              ymaxr=ymax*2.0
              yintr=yint
            endif
            if(y((nens+1)*nyear).gt.(2.0*ymax))then
              ymaxr=ymax*3.0
              yintr=yint
            endif
            write(*,*)'Vertical scale'
            write(*,*)ymin,ymax,yint
            write(*,*)y(1),y((nens+1)*nyear)
            write(*,*)yminr,ymaxr,yintr
            deallocate (x,                                   stat=istat)
            deallocate (y,                                   stat=istat)
            CALL PSETC('AXIS_TYPE','REGULAR')
            CALL PSETC('AXIS_ORIENTATION','VERTICAL')
            CALL PSETC('AXIS_POSITION','LEFT')
            CALL PSETR('AXIS_MAX_VALUE',ymaxr)
            CALL PSETR('AXIS_MIN_VALUE',yminr)
            CALL PSETR('AXIS_TICK_INTERVAL',yintr)
            CALL PSETC('AXIS_TICK','ON')
            CALL PSETC('AXIS_TICK_LABEL','ON')
            form1='f4.1'
            if((ymaxr.ne.ymax).or.(ymin.ne.yminr))form1='f5.2'
            if((abs(ymaxr).ge.10.0).or.(abs(yminr).ge.10.0))form1='f5.1'
            CALL PSETC('AXIS_TICK_LABEL_FORMAT',form1)
            CALL PSETC('AXIS_MINOR_TICK','OFF')
            CALL PSETC('AXIS_TICK_LABEL_FIRST','ON')
            CALL PSETC('AXIS_TICK_LABEL_LAST','ON')
            CALL PSETC('AXIS_TITLE','ON')
            CALL PSETC('AXIS_TITLE_TEXT',yaxti(1:lena(yaxti)))
            CALL PSETC('AXIS_GRID','OFF')
            CALL PAXIS
C
            CALL PSETC('AXIS_TYPE','REGULAR')
            CALL PSETC('AXIS_ORIENTATION','HORIZONTAL')
            CALL PSETC('AXIS_POSITION','BOTTOM')
            CALL PSETR('AXIS_MIN_VALUE',float(iyy1-1))
            CALL PSETR('AXIS_MAX_VALUE',float(iyy1+nyear))
            CALL PSETR('AXIS_TICK_INTERVAL',2.)
            CALL PSETC('AXIS_TICK','ON')
            CALL PSETC('AXIS_TICK_LABEL','ON')
            CALL PSETC('AXIS_TICK_LABEL_FORMAT','I4.4')
            CALL PSETC('AXIS_TICK_LABEL_FIRST','ON')
            CALL PSETC('AXIS_TICK_LABEL_LAST','OFF')
            CALL PSETC('AXIS_MINOR_TICK','ON')
            CALL PSETC('AXIS_TITLE','ON')
            CALL PSETC('AXIS_TITLE_TEXT','Time')
            CALL PSETC('AXIS_GRID','OFF')
            CALL PAXIS
c
c.... terciles
c
            allocate (s(nyear+2),                            stat=istat)
            allocate (d(nyear+2),                            stat=istat)
            CALL PSETC('GRAPH_TYPE','CURVE')
            CALL PSETC('LEGEND','OFF')
            CALL PSETC('GRAPH_LINE','ON')
            CALL PSETC('GRAPH_SYMBOL','OFF')
            CALL PSETC('GRAPH_LINE_COLOUR','RED')
            CALL PSETC('GRAPH_LINE_STYLE','DASH')
            CALL PSETI('GRAPH_LINE_THICKNESS',3)
            do iy=1,nyear+2
               s(iy)=tru(1)
               d(iy)=float(iyy1+iy-2)
            enddo
            CALL PSET1R('GRAPH_CURVE_X_VALUES',d,nyear+2)
            CALL PSET1R('GRAPH_CURVE_Y_VALUES',s,nyear+2)
            CALL PGRAPH
            do iy=1,nyear+2
               s(iy)=tru(2)
            enddo
            CALL PSET1R('GRAPH_CURVE_Y_VALUES',s,nyear+2)
            CALL PGRAPH
            CALL PSETC('GRAPH_LINE_COLOUR','BLUE')
            do iy=1,nyear+2
               s(iy)=thu(1)
               if(ityp.eq.2)s(iy)=thi(1)
            enddo
            CALL PSET1R('GRAPH_CURVE_Y_VALUES',s,nyear+2)
            CALL PGRAPH
            do iy=1,nyear+2
               s(iy)=thu(2)
               if(ityp.eq.2)s(iy)=thi(2)
            enddo
            CALL PSET1R('GRAPH_CURVE_Y_VALUES',s,nyear+2)
            CALL PGRAPH
            deallocate (s,                                   stat=istat)
            deallocate (d,                                   stat=istat)
c
c.... plot box-and-whiskers
c.... boxes are drawn on terciles of the distribution (ut and lt)
c
            allocate (s(nyear),                              stat=istat)
            allocate (d(nyear),                              stat=istat)
            allocate (x(nens),                               stat=istat)
            allocate (y(nens),                               stat=istat)
            ut=2*nens/3
            lt=nens/3+1
            do iy=1,nyear
               do iens=1,nens
                  x(iens)=h(iy,iens)
                  if(ityp.eq.2)x(iens)=hh(iy,iens)
               enddo
               call sort(1,nens,nens,x,y)
               mmin(iy)=y(1)
               mmax(iy)=y(nens)
               mut(iy)=y(ut)
               mlt(iy)=y(lt)
c              write(*,'(2i2,4f10.3)')lt,ut,
c    >            mmin(iy),mlt(iy),mut(iy),mmax(iy)
            enddo
            deallocate (x,                                   stat=istat)
            deallocate (y,                                   stat=istat)
c
            CALL PSETC('GRAPH_TYPE','BAR')
            CALL PSETC('GRAPH_LINE','OFF')
            CALL PSETC('GRAPH_SHADE','ON')
            CALL PSETC('GRAPH_SHADE_STYLE','AREA_FILL')
            CALL PSETC('GRAPH_SHADE_COLOUR','YELLOW_GREEN')
            do iy=1,nyear
               s(iy)=mmin(iy)
               t(iy)=mmax(iy)
               d(iy)=float(iyy1+iy-1)
c              write(*,'(i2,3f10.3)')iy,d(iy),s(iy),t(iy)
            enddo
            CALL PSETR('GRAPH_BAR_WIDTH',0.20)
            CALL PSET1R('GRAPH_BAR_X_VALUES',d,nyear)
            CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',s,nyear)
            CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',t,nyear)
            CALL PGRAPH
            CALL PSETC('GRAPH_LINE','ON')
            CALL PSETI('GRAPH_BAR_LINE_THICKNESS',1)
            CALL PSETC('GRAPH_BAR_LINE_STYLE','SOLID')
            CALL PSETC('GRAPH_BAR_COLOUR','BLACK')
            CALL PSETC('GRAPH_SHADE','OFF')
            CALL PSET1R('GRAPH_BAR_X_VALUES',d,nyear)
            CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',s,nyear)
            CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',t,nyear)
            CALL PGRAPH
            do iy=1,nyear
               s(iy)=mlt(iy)
               t(iy)=mut(iy)
               d(iy)=float(iyy1+iy-1)
c              write(*,'(i2,3f10.3)')iy,d(iy),s(iy),t(iy)
            enddo
            CALL PSETC('GRAPH_BAR_COLOUR','BLACK')
            CALL PSETC('LEGEND','ON')
            CALL PSETC('LEGEND_USER_TEXT','Max/Ter/Min')
            CALL PSETC('GRAPH_LINE','OFF')
            CALL PSETC('GRAPH_SHADE','ON')
            CALL PSETC('GRAPH_SHADE_STYLE','AREA_FILL')
            CALL PSETC('GRAPH_SHADE_COLOUR','YELLOW_GREEN')
            CALL PSETR('GRAPH_BAR_WIDTH',0.45)
            CALL PSET1R('GRAPH_BAR_X_VALUES',d,nyear)
            CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',s,nyear)
            CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',t,nyear)
            CALL PGRAPH
            CALL PSETC('LEGEND','OFF')
            CALL PSETC('GRAPH_LINE','ON')
            CALL PSETI('GRAPH_BAR_LINE_THICKNESS',1)
            CALL PSETC('GRAPH_BAR_LINE_STYLE','SOLID')
            CALL PSETC('GRAPH_BAR_COLOUR','BLACK')
            CALL PSETC('GRAPH_SHADE','OFF')
            CALL PSET1R('GRAPH_BAR_X_VALUES',d,nyear)
            CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',s,nyear)
            CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',t,nyear)
            CALL PGRAPH
c
            CALL PSETC('LEGEND','ON')
            CALL PSETC('LEGEND_USER_TEXT','Reference')
            CALL PSETC('GRAPH_TYPE','CURVE')
            CALL PSETC('GRAPH_LINE','OFF')
            CALL PSETC('GRAPH_SYMBOL','ON')
            CALL PSETR('GRAPH_SYMBOL_HEIGHT',0.4)
            CALL PSETC('GRAPH_SYMBOL_COLOUR','RED')
            CALL PSETI('GRAPH_SYMBOL_MARKER_INDEX',15)
            do iy=1,nyear
               s(iy)=r(iy)
               d(iy)=float(iyy1+iy-1)
c              write(*,'(i2,2f10.3)')iy,d(iy),s(iy)
            enddo
            CALL PSET1R('GRAPH_CURVE_X_VALUES',d,nyear)
            CALL PSET1R('GRAPH_CURVE_Y_VALUES',s,nyear)
            CALL PGRAPH
            CALL PSETR('GRAPH_SYMBOL_HEIGHT',0.35)
            CALL PSETC('GRAPH_SYMBOL_COLOUR','BLUE')
            ctitg1=''
c           write(ctitg1,'(a,a)')expt(1:lena(expt)),' EM'
            write(ctitg1,'(a)')'Ens. mean'
            CALL PSETC('LEGEND_USER_TEXT',ctitg1(1:lena(ctitg1)))
            do iy=1,nyear
               s(iy)=hm(iy)
               if(ityp.eq.2)s(iy)=hhm(iy)
               d(iy)=float(iyy1+iy-1)
c              write(*,'(i2,2f10.3)')iy,d(iy),s(iy)
            enddo
            CALL PGRAPH
c
            CALL PNEW('SUPER_PAGE')
         enddo !ityp loop
         CALL PCLOSE
c
         deallocate (r,                                      stat=istat)
         deallocate (h,                                      stat=istat)
         deallocate (hh,                                     stat=istat)
         deallocate (hm,                                     stat=istat)
         deallocate (hhm,                                    stat=istat)
         deallocate (s,                                      stat=istat)
         deallocate (t,                                      stat=istat)
         deallocate (d,                                      stat=istat)
         deallocate (mmin,                                   stat=istat)
         deallocate (mmax,                                   stat=istat)
         deallocate (mut,                                    stat=istat)
         deallocate (mlt,                                    stat=istat)
c
      enddo !it loop
c
 999  write(*,*)'program seems to be successfully finished :)'
c
      end
