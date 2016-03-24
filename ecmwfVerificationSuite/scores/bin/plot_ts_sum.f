c
c----------------------------------------------------------------------c
c     PROGRAM plot_ts_sum                 P. Doblas-Reyes 05-Jan-2009  c
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
c     plot_ts_sum.x < nlist                                            c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 plot_ts_sum.f tools.f tools_plot.f                 c
c     -o plot_ts_sum.x $MAGLIB $EMOSLIB $NAGLIB                        c
c                                                                      c
c     MODS:                                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      PROGRAM plot_ts_sum
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: val(:,:,:,:), conf(:,:,:,:)
      real, allocatable :: x(:), y1(:), y2(:), score(:)
      character*20, allocatable :: ticklabel(:)
c
c.... hard coded field definitions
c
      integer nts_max
      parameter(nts_max=80)
      character*6 namet(nts_max)
      integer par(nts_max), lev(nts_max)
      integer yy1(nts_max), yy2(nts_max)
      integer nmod_max
      parameter(nmod_max=15)
      integer nenm(nmod_max)
      integer typm(nmod_max)
      character*21 expm(nmod_max)
      integer nsdt_max
      parameter(nsdt_max=12)
      integer imm(nsdt_max), idd(nsdt_max), itt(nsdt_max)
      integer nldt_max
      parameter(nldt_max=13)
      integer nf1(nldt_max), nf2(nldt_max)
      real eps
      parameter(eps=0.1)
c
c.... other definitions
c
      real spxl, spyl, pxl, pyl, sbpxl, sbpxp, sbpyl, sbpyp
      real ymax, ymin, yint
      real shift, xinc
      real ratio, varn, varni, rms, rmsi
      real cor, sig, cori, sigi, snr, sigr, snri, sigri
      real rpssu, rpsspu, rpss, rpssp, rpssdu, rpssdpu, rpssd, rpssdp
      real x_top, x_bot
      real symh
      integer nsdt, cros, anin, nldt, nmod, mult, nts
      integer ipar, ilev, iyy1, iyy2
      integer exptl, lena, istat
      integer i, it, im, il, imod, nyear, iy, ityp
      integer imarker(1)
      character*21 expt
      character*100 yfile, yifile, yofile
      character*120 ctitg1, ctitg2, ctitg3, ctitg4, ctitg5, ctitg6
      character*60 title, yaxti
      character*20 start
      character*20 form1
      character*50 diagnostic
      character*3 dia
      character*25 legend
      character*20 bcolour(15)
      data bcolour/'blue','evergreen','orange','cyan','pink',
     >   'grey','yellow','coral','purple','mustard',
     >   'greenish_yellow','violet','purple','khaki','navy'/
c
      namelist /control/ nenm, nsdt,  imm,  idd,  itt,
     >                   cros, anin, nldt,  nf1,  nf2,
     >                   expm, typm, nmod, mult,  dia,  nts
      namelist /timeseries/
     >                  namet,  par,  lev,  yy1, yy2
c
c.... Check the order of the parameters for the indices
c
      read(5,control)
      write(*,*)'nenm: ',(nenm(i),i=1,nmod)
      write(*,*)'nsdt: ',nsdt
      write(*,*)' imm: ',(imm(i),i=1,nsdt)
      write(*,*)' idd: ',(idd(i),i=1,nsdt)
      write(*,*)' itt: ',(itt(i),i=1,nsdt)
      write(*,*)'cros: ',cros
      write(*,*)'anin: ',anin
      write(*,*)'nldt: ',nldt
      write(*,*)' nf1: ',(nf1(i),i=1,nldt)
      write(*,*)' nf2: ',(nf2(i),i=1,nldt)
      write(*,*)'expm: ',(expm(i),i=1,nmod)
      write(*,*)'typm: ',(typm(i),i=1,nmod)
      write(*,*)'nmod: ',nmod
      write(*,*)'mult: ',mult
      write(*,*)' dia: ',dia
      write(*,*)' nts: ',nts
      read(5,timeseries)
      write(*,*)'namet: ',(namet(i),i=1,nts)
      write(*,*)' par: ',(par(i),i=1,nts)
      write(*,*)' lev: ',(lev(i),i=1,nts)
      write(*,*)' yy1: ',(yy1(i),i=1,nts)
      write(*,*)' yy2: ',(yy2(i),i=1,nts)
c
c.... allocate fields
c
      allocate (val(nsdt,nldt,nmod,2),                       stat=istat)
      allocate (conf(nsdt,nldt,nmod,2),                      stat=istat)
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
c.... loop over the models
c
         do imod=1,nmod
            expt=expm(imod)
c
c.... loop over the start dates
c
            do im=1,nsdt
c
c.... loop over the lead times
c
               do il=1,nldt
c
c.... open input file
c
                  exptl=lena(expt)
                  write(form1,'(a,i2.2,a)')'(a',exptl,')'
                  yifile=
     >               'TS_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE'
                  write(yifile(       4: 3+exptl),form1)expt(1:exptl)
                  write(yifile( 4+exptl: 4+exptl),'(a1)')'_'
                  write(yifile( 5+exptl: 8+exptl),'(i4.4)')iyy1
                  write(yifile( 9+exptl: 9+exptl),'(a1)')'-'
                  write(yifile(10+exptl:13+exptl),'(i4.4)')iyy2
                  write(yifile(14+exptl:14+exptl),'(a1)')'_'
                  write(yifile(15+exptl:16+exptl),'(i2.2)')imm(im)
                  write(yifile(17+exptl:18+exptl),'(i2.2)')idd(im)
                  write(yifile(19+exptl:20+exptl),'(i2.2)')itt(im)
                  write(yifile(21+exptl:23+exptl),'(a3)')'_CV'
                  write(yifile(24+exptl:25+exptl),'(i2.2)')cros
                  write(yifile(26+exptl:27+exptl),'(a2)')'_I'
                  write(yifile(28+exptl:28+exptl),'(i1.1)')anin
                  write(yifile(29+exptl:29+exptl),'(a1)')'_'
                  write(yifile(30+exptl:32+exptl),'(i3.3)')ipar
                  write(yifile(33+exptl:33+exptl),'(a1)')'_'
                  write(yifile(34+exptl:36+exptl),'(i3.3)')ilev
                  write(yifile(37+exptl:37+exptl),'(a1)')'_'
                  write(yifile(38+exptl:40+exptl),'(i3.3)')nf1(il)
                  write(yifile(41+exptl:41+exptl),'(a1)')'-'
                  write(yifile(42+exptl:44+exptl),'(i3.3)')nf2(il)
                  yifile=yifile(1:lena(yifile))//'_'//namet(it)
                  write(*,*)'open input file: ',yifile(1:lena(yifile))
                  open(10,file=yifile,form='formatted',status='old')
c
c.... reading data
c.... skip hindcasts
c
                  do iy=1,nyear
                     read(10,*)
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
     >               ratio,varn,varni,rms,rmsi,cor,sig,cori,sigi,
     >               snr,sigr,snri,sigri,
     >               rpssu,rpsspu,rpss,rpssp,rpssdu,rpssdpu,rpssd,rpssdp
                  close(10)
c
c.... assign values
c
                  if(dia.eq.'COR')then
                    diagnostic='Ensemble-mean correlation'
                    val(im,il,imod,1)=cor
                    conf(im,il,imod,1)=sig
                    val(im,il,imod,2)=cori
                    conf(im,il,imod,2)=sigi
                  endif
                  if(dia.eq.'RSR')then
                    diagnostic='Ratio spread(sd)/RMSE'
                    val(im,il,imod,1)=varn
                    val(im,il,imod,2)=varni
                  endif
                  if(dia.eq.'RPS')then
                    diagnostic='Ranked probability skill score'
                    val(im,il,imod,1)=rpssu
                    conf(im,il,imod,1)=rpsspu
                    val(im,il,imod,2)=rpss
                    conf(im,il,imod,2)=rpssp
                  endif
                  if(dia.eq.'RPD')then
                    diagnostic='Debiased ranked probability skill score'
                    val(im,il,imod,1)=rpssdu
                    conf(im,il,imod,1)=rpssdpu
                    val(im,il,imod,2)=rpssd
                    conf(im,il,imod,2)=rpssdp
                  endif
               enddo !il loop
            enddo !im loop
         enddo !imod loop
c
c.... open output file
c
         yofile='DIA_YYYY-YYYY_CVYY_II_PAR_LEV'
         write(yofile( 1: 3),'(a3)')dia
         write(yofile( 4: 4),'(a1)')'_'
         write(yofile( 5: 8),'(i4.4)')iyy1
         write(yofile( 9: 9),'(a1)')'-'
         write(yofile(10:13),'(i4.4)')iyy2
         write(yofile(14:16),'(a3)')'_CV'
         write(yofile(17:18),'(i2.2)')cros
         write(yofile(19:20),'(a2)')'_I'
         write(yofile(21:21),'(i1.1)')anin
         write(yofile(22:22),'(a1)')'_'
         write(yofile(23:25),'(i3.3)')ipar
         write(yofile(26:26),'(a1)')'_'
         write(yofile(27:29),'(i3.3)')ilev
         yofile=yofile(1:lena(yofile))//'_'//namet(it)
         yofile='psfiles/'//yofile(1:lena(yofile))//'.ps'
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
         CALL PSETR('AXIS_TICK_SIZE',0.1)
         CALL PSETR('AXIS_MINOR_TICK_SIZE',0.05)
         CALL PSETR('AXIS_MINOR_TICK_MIN_GAP',0.05)
         CALL PSETC('AXIS_GRID_LINE_STYLE','DOT')
         CALL PSETC('AXIS_GRID_COLOUR','BLACK')
C
         SPXL=29.5
         SPYL=21.
         CALL PSETR('SUPER_PAGE_X_LENGTH',SPXL)
         CALL PSETR('SUPER_PAGE_Y_LENGTH',SPYL)
         CALL PSETC('PLOT_DIRECTION','HORIZONTAL')
C        CALL PSETC('PLOT_DIRECTION','VERTICAL')
c        CALL PSETC('PLOT_START','TOP')
         CALL PSETC('PLOT_START','BOTTOM')
         CALL PSETR('PAGE_X_GAP',0.)
         CALL PSETR('PAGE_Y_GAP',0.)
c
         CALL PSETC('PAGE_FRAME','OFF')
         CALL PSETC('PAGE_FRAME_COLOUR','BLACK')
         CALL PSETC('PAGE_ID_LINE','OFF')
         CALL PSETC('SUBPAGE_MAP_PROJECTION','NONE')
         CALL PSETC('SUBPAGE_FRAME_COLOUR','BLACK')
C
         CALL PSETC('LEGEND','OFF')
         CALL PSETC('LEGEND_BORDER','OFF')
         CALL PSETC('LEGEND_TEXT_COLOUR','BLACK')
         CALL PSETC('LEGEND_TEXT_QUALITY','HIGH')
         CALL PSETR('LEGEND_TEXT_MAXIMUM_HEIGHT',0.8)
         CALL PSETR('LEGEND_ENTRY_MAXIMUM_WIDTH',8.0)
         CALL PSETR('LEGEND_ENTRY_MAXIMUM_HEIGHT',0.8)
         CALL PSETR('LEGEND_BOX_Y_LENGTH',3.0)
         CALL PSETI('LEGEND_COLUMN_COUNT',5)
c
c.... loop for original and inflated values
c
         do ityp=1,2
c
c.... searching for min/max
c.... required to draw the axes of MSS, SPR and SPR
c
            x_top=0.
            x_bot=0.
            do imod=1,nmod
               do im=1,nsdt
                  do il=1,nldt
                     if(val(im,il,imod,ityp).gt.x_top)
     >                  x_top=val(im,il,imod,ityp)
                     if(val(im,il,imod,ityp).lt.x_bot)
     >                  x_bot=val(im,il,imod,ityp)
                  enddo !il loop
               enddo !im loop
            enddo !imod loop
            write(*,*)'Range ',x_bot,x_top
c
c.... general text
c
            PXL=SPXL-eps
            PYL=SPYL/5-eps
            SBPXL=.95*PXL
            SBPXP=(PXL-SBPXL)/2
            SBPYL=.95*PYL
            SBPYP=(PYL-SBPYL)/2.
            CALL PSETR('PAGE_X_LENGTH',PXL)
            CALL PSETR('PAGE_Y_LENGTH',PYL)
            CALL PSETR('PAGE_X_POSITION',0.)
            CALL PSETR('PAGE_Y_POSITION',4*SPYL/5)
            CALL PSETR('SUBPAGE_Y_POSITION',SBPYP)
            CALL PSETR('SUBPAGE_Y_LENGTH',SBPYL)
            CALL PSETR('SUBPAGE_X_POSITION',SBPXP)
            CALL PSETR('SUBPAGE_X_LENGTH',SBPXL)
            CALL PSETC('SUBPAGE_FRAME','OFF')
            write(*,*)SBPYP,SBPYL,SBPXP,SBPXL
c
C           CALL PSETC('TEXT_MODE','TITLE')
            CALL PSETC('TEXT_MODE','POSITIONAL')
            CALL PSETR('TEXT_BOX_Y_POSITION',SBPYP)
            CALL PSETR('TEXT_BOX_Y_LENGTH',SBPYL)
            CALL PSETR('TEXT_BOX_X_POSITION',SBPXP)
            CALL PSETR('TEXT_BOX_X_LENGTH',SBPXL)
            CALL PSETC('TEXT_COLOUR','BLACK')
            CALL PSETC('TEXT_JUSTIFICATION','LEFT')
            CALL PSETR('TEXT_REFERENCE_CHARACTER_HEIGHT',0.5)
            CALL PSETC('TEXT_QUALITY','HIGH')
            CALL PSETI('TEXT_LINE_COUNT',3)
c
            call tseries(namet(it),imm(1),title,
     >         yaxti,ymax,ymin,yint,start)
            WRITE(CTITG1,'(a,a,a)')
     >        diagnostic(1:lena(diagnostic)),' for ',
     >        title(1:lena(title))
            if(ityp.eq.2)WRITE(CTITG1,'(a,a,a,a)')
     >        diagnostic(1:lena(diagnostic)),' for ',
     >        title(1:lena(title)),' (inflated)'
            WRITE(CTITG2,'(A,I4.4,A,I4.4)')
     >         'Hindcast period ',iyy1,'-',iyy2
            write(*,*)CTITG1
            write(*,*)CTITG2
            CALL PSETC('TEXT_LINE_1',CTITG1(1:lena(CTITG1)))
            CALL PSETC('TEXT_LINE_2',CTITG2(1:lena(CTITG2)))
            if(dia.ne.'RSR')then
              WRITE(CTITG3,'(a)')
     >           'Dots depict the p-value (right axis)'
              CALL PSETC('TEXT_LINE_3',CTITG3(1:lena(CTITG3)))
            endif
            CALL PTEXT
            CALL PNEW('PAGE')
c
c.... plotting the bar diagram
c
            PXL=SPXL-eps
            PYL=4*SPYL/5-eps
            SBPXL=.80*PXL
            SBPXP=(PXL-SBPXL)/2
            SBPYL=.80*PYL
            SBPYP=(PYL-SBPYL)/2.
            CALL PSETR('PAGE_X_LENGTH',PXL)
            CALL PSETR('PAGE_Y_LENGTH',PYL)
            CALL PSETR('PAGE_X_POSITION',0.)
            CALL PSETR('PAGE_Y_POSITION',0.)
            CALL PSETR('SUBPAGE_Y_POSITION',SBPYP)
            CALL PSETR('SUBPAGE_Y_LENGTH',SBPYL)
            CALL PSETR('SUBPAGE_X_POSITION',SBPXP)
            CALL PSETR('SUBPAGE_X_LENGTH',SBPXL)
            CALL PSETC('SUBPAGE_FRAME','ON')
            CALL PSETC('SUBPAGE_MAP_PROJECTION','NONE')
c
c.... plot the axes
c
            CALL PSETR('AXIS_TITLE_HEIGHT',0.25)
            allocate (ticklabel(0:nsdt*nldt+1),              stat=istat)
            i=0
            ticklabel(i)=' '
            do im=1,nsdt
             call tseries(namet(it),imm(im),title,
     >          yaxti,ymax,ymin,yint,start)
               do il=1,nldt
                  i=i+1
                  form1='(a,a,i1.1,a,i1.1,a)'
                  if(nf1(il).gt.9)form1='(a,a,i2.2,a,i2.2,a)'
                  if((nf1(il).le.9).and.(nf2(il).gt.9))
     >               form1='(a,a,i1.1,a,i2.2,a)'
                  if((nf1(il).le.9).and.(nf2(il).gt.99))
     >               form1='(a,a,i1.1,a,i3.3,a)'
                  if((nf1(il).gt.9).and.(nf2(il).gt.9))
     >               form1='(a,a,i2.2,a,i2.2,a)'
                  if((nf1(il).gt.9).and.(nf2(il).gt.99))
     >               form1='(a,a,i2.2,a,i3.3,a)'
                  if(nf1(il).gt.99)form1='(a,a,i3.3,a,i3.3,a)'
                  write(ticklabel(i),form1)
     >               start(1:3),' (',nf1(il),'-',nf2(il),')'
               enddo
            enddo
            ticklabel(nsdt*nldt+1)=' '
            CALL PSETC('AXIS_ORIENTATION','HORIZONTAL')
            CALL PSETC('AXIS_POSITION','BOTTOM')
            CALL PSETC('AXIS_TYPE','REGULAR')
            CALL PSETC('AXIS_TITLE','ON')
            CALL PSETC('AXIS_TITLE_TEXT','Start date (Forecast range)')
            CALL PSETR('AXIS_MIN_VALUE',0.)
            CALL PSETR('AXIS_MAX_VALUE',float(nsdt*nldt+1))
            CALL PSETR('AXIS_TICK_INTERVAL',1.)
            CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',1)
            CALL PSETC('AXIS_TICK_LABEL_TYPE','LABEL_LIST')
            CALL PSETC('AXIS_TICK_LABEL_FIRST','OFF')
            CALL PSETC('AXIS_TICK_LABEL_LAST','ON')
            CALL PSETR('AXIS_TICK_LABEL_HEIGHT',0.23)
            CALL PSET1C('AXIS_TICK_LABEL_LIST',ticklabel,nsdt*nldt+2)
            CALL PAXIS
            deallocate (ticklabel,                           stat=istat)
c
            ymax=1.0
            ymin=-1.0
            yint=0.2
            if(x_bot.ge.0.)ymin=0.
            if((dia.eq.'RPS').or.(dia.eq.'RPD'))then
              yint=0.1
              if((x_top.gt.0.6).and.(x_top.le.0.8))ymax=0.8
              if((x_top.gt.0.5).and.(x_top.le.0.6))ymax=0.6
              if((x_top.gt.0.4).and.(x_top.le.0.5))ymax=0.5
              if((x_top.gt.0.3).and.(x_top.le.0.4))ymax=0.4
              if((x_top.gt.0.2).and.(x_top.le.0.3))ymax=0.3
              if((x_top.gt.0.1).and.(x_top.le.0.2))ymax=0.2
              if((x_top.gt.0.05).and.(x_top.le.0.1))then
                ymax=0.1
                yint=0.05
              endif
              if((x_bot.lt.0.).and.(x_bot.ge.-0.1))then
                ymin=-0.1
                yint=0.05
              endif
              if((x_bot.lt.-0.1).and.(x_bot.ge.-0.2))ymin=-0.2
              if((x_bot.lt.-0.2).and.(x_bot.ge.-0.3))ymin=-0.3
              if((x_bot.lt.-0.3).and.(x_bot.ge.-0.4))ymin=-0.4
              if((x_bot.lt.-0.4).and.(x_bot.ge.-0.5))ymin=-0.5
              if((x_bot.lt.-0.5).and.(x_bot.ge.-0.6))ymin=-0.6
              if((x_bot.lt.-0.6).and.(x_bot.ge.-0.8))ymin=-0.8
            endif
            if(dia.eq.'RSR')then
              ymin=0.0
              yint=0.25
              ymax=1.5
              if((x_top.gt.1.5).and.(x_top.le.2.0))ymax=2.0
              if((x_top.gt.2.0).and.(x_top.le.2.5))ymax=2.5
              if((x_top.gt.2.5).and.(x_top.le.3.0))ymax=3.0
              if((x_top.gt.3.0).and.(x_top.le.3.5))ymax=3.5
            endif
            CALL PSETC('AXIS_ORIENTATION','VERTICAL')
            CALL PSETC('AXIS_POSITION','LEFT')
            CALL PSETC('AXIS_TYPE','REGULAR')
            CALL PSETC('AXIS_TITLE','ON')
            if(dia.eq.'COR')CALL PSETC('AXIS_TITLE_TEXT','Correlation')
            if(dia.eq.'RSR')CALL PSETC('AXIS_TITLE_TEXT','Spread/RMSE')
            if(dia.eq.'RPS')CALL PSETC('AXIS_TITLE_TEXT','RPSS')
            if(dia.eq.'RPD')CALL PSETC('AXIS_TITLE_TEXT','RPSSd')
            CALL PSETC('AXIS_TICK_LABEL_TYPE','NUMBER')
            CALL PSETC('AXIS_TICK_LABEL_FIRST','ON')
            CALL PSETC('AXIS_TICK_LABEL_LAST','ON')
            CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',1)
            CALL PSETC('AXIS_TICK_LABEL_FORMAT','F5.2')
            CALL PSETR('AXIS_TICK_LABEL_HEIGHT',0.30)
            CALL PSETR('AXIS_MIN_VALUE',ymin)
            CALL PSETR('AXIS_MAX_VALUE',ymax)
            CALL PSETR('AXIS_TICK_INTERVAL',yint)
            CALL PAXIS
c
            xinc=0.60/nmod
            shift=nmod*xinc/2.
            if(mult.eq.1)then
              xinc=0.60/(nmod+1)
              shift=(nmod+1)*xinc/2.
            endif
c
c.... reference line
c
            CALL PSETC('LEGEND','OFF')
            allocate (x(0:nsdt*nldt+3),                      stat=istat)
            allocate (y1(0:nsdt*nldt+3),                     stat=istat)
            do i=0,nsdt*nldt+3
               x(i)=float(i)
               y1(i)=0.
               if(dia.eq.'RSR')y1(i)=1.
            enddo
            CALL PSETC('GRAPH_TYPE','CURVE')
            CALL PSETC('GRAPH_LINE_COLOUR','BLACK')
            CALL PSETC('GRAPH_LINE_STYLE','SOLID')
            CALL PSETI('GRAPH_LINE_THICKNESS',1)
            CALL PSET1R('GRAPH_CURVE_X_VALUES',x,nsdt*nldt+4)
            CALL PSET1R('GRAPH_CURVE_Y_VALUES',y1,nsdt*nldt+4)
            CALL PGRAPH
            deallocate (x,                                   stat=istat)
            deallocate (y1,                                  stat=istat)
c
            allocate (x(2),                                  stat=istat)
            allocate (y1(2),                                 stat=istat)
            CALL PSETC('GRAPH_TYPE','CURVE')
            CALL PSETC('GRAPH_LINE_COLOUR','BLACK')
            CALL PSETC('GRAPH_LINE_STYLE','DOT')
            CALL PSETI('GRAPH_LINE_THICKNESS',1)
            do i=1,nsdt-1
               x(1)=float(i*nldt)+nmod*xinc-shift/4.
               x(1)=float(i*nldt)+nmod*xinc
               x(2)=x(1)
               y1(1)=ymin
               y1(2)=ymax
               CALL PSET1R('GRAPH_CURVE_X_VALUES',x,2)
               CALL PSET1R('GRAPH_CURVE_Y_VALUES',y1,2)
               CALL PGRAPH
            enddo
            deallocate (x,                                   stat=istat)
            deallocate (y1,                                  stat=istat)
c
c.... plot the data
c
            CALL PSETC('GRAPH_TYPE','BAR')
            CALL PSETC('GRAPH_SHADE','ON')
            CALL PSETC('GRAPH_SHADE_STYLE','AREA_FILL')
            CALL PSETC('GRAPH_SHADE_COLOUR','RED')
            CALL PSETC('SYMBOL_POSITION_MODE','GRAPH')
            CALL PSETC('SYMBOL_TYPE','MARKER')
            CALL PSETC('LEGEND','OFF')
c
            allocate (x(1),                                  stat=istat)
            allocate (y1(1),                                 stat=istat)
            allocate (y2(1),                                 stat=istat)
            allocate (score(nmod),                           stat=istat)
            if(nmod.eq.1)then
              shift=0.
              xinc=0.
            endif
            do im=1,nsdt
               do il=1,nldt
                  do imod=1,nmod
                     score(imod)=val(im,il,imod,ityp)
                  enddo
                  do imod=1,nmod
c
c.... bars and legends
c
                     CALL PSETC('LEGEND','OFF')
                     if((im.eq.1).and.(il.eq.1))
     >                  CALL PSETC('LEGEND','ON')
                     CALL PSETC('GRAPH_BAR_COLOUR',bcolour(imod))
                     CALL PSETC('GRAPH_SHADE_COLOUR',bcolour(imod))
                     if((imod.eq.nmod).and.(mult.eq.1))then
                       CALL PSETC('GRAPH_BAR_COLOUR','RED')
                       CALL PSETC('GRAPH_SHADE_COLOUR','RED')
                       write(legend,'(a,a,i3,a)')expm(imod)(2:5),
     >                    ' (',nenm(imod),')'
                       CALL PSETR('GRAPH_BAR_WIDTH',0.20)
                     else
                       if(typm(imod).eq.1)then
                         write(legend,'(a,a,i3,a)')expm(imod)(2:5),
     >                      ' (',nenm(imod),')'
                       else
                         write(legend,'(a,a,a,a,i2,a)')
     >                      expm(imod)(5:8),'-',
     >                      expm(imod)(10:13),' (',nenm(imod),')'
                       endif
                       CALL PSETR('GRAPH_BAR_WIDTH',0.16)
                       if(nmod.gt.6)CALL PSETR('GRAPH_BAR_WIDTH',0.10)
                     endif
                     CALL PSETC('LEGEND_USER_TEXT',
     >                  legend(1:lena(legend)))
                     write(*,*)legend
c
c.... abscissa
c
                     x(1)=float((im-1)*nldt+il)-shift+imod*xinc
                     if((imod.eq.nmod).and.(mult.eq.1))x(1)=
     >                  float((im-1)*nldt+il)-shift+(nmod+1)*xinc
c
c.... bars
c
                     if(score(imod).gt.0.)then
                       y1(1)=0.
                       y2(1)=score(imod)
                     else
                       y1(1)=score(imod)
                       y2(1)=0.
                     endif
                     CALL PSET1R('GRAPH_BAR_X_VALUES',x,1)
                     CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',y1,1)
                     CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',y2,1)
                     CALL PGRAPH
                  enddo
               enddo
            enddo
            deallocate (x,                                   stat=istat)
            deallocate (y1,                                  stat=istat)
            deallocate (y2,                                  stat=istat)
            deallocate (score,                               stat=istat)
c
c.... symbols
c
            xinc=0.60/nmod
            shift=nmod*xinc/2.
            if(mult.eq.1)then
              xinc=0.60/(nmod+1)
              shift=(nmod+1)*xinc/2.
            endif
            if(nmod.eq.1)then
              shift=0.
              xinc=0.
            endif
            allocate (x(1),                                  stat=istat)
            allocate (y1(1),                                 stat=istat)
c
            CALL PSETC('AXIS_ORIENTATION','VERTICAL')
            CALL PSETC('AXIS_POSITION','RIGHT')
            ymax=0.000001
            CALL PSETC('AXIS_TYPE','LOGARITHMIC')
            CALL PSETC('AXIS_TICK_LABEL_FORMAT','E9.1')
            CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',10)
            ymax=0.005
            CALL PSETC('AXIS_TYPE','REGULAR')
            CALL PSETC('AXIS_TICK_LABEL_FORMAT','F4.2')
            CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',1)
            CALL PSETC('AXIS_TITLE','ON')
            CALL PSETC('AXIS_TITLE_TEXT','p-value')
            CALL PSETC('AXIS_TICK_LABEL_TYPE','NUMBER')
            CALL PSETC('AXIS_TICK_LABEL_FIRST','ON')
            CALL PSETC('AXIS_TICK_LABEL_LAST','ON')
            CALL PSETR('AXIS_TICK_LABEL_HEIGHT',0.30)
            CALL PSETR('AXIS_MIN_VALUE',0.10)
            CALL PSETR('AXIS_MAX_VALUE',ymax)
            CALL PSETR('AXIS_TICK_INTERVAL',0.01)
            CALL PAXIS
c
            CALL PSETC('LEGEND','OFF')
            do im=1,nsdt
               do il=1,nldt
                  do imod=1,nmod
                     imarker(1)=28
                     CALL PSET1I('SYMBOL_INPUT_MARKER_LIST',imarker,1)
                     CALL PSETC('SYMBOL_COLOUR',bcolour(imod))
                     if((imod.eq.nmod).and.(mult.eq.1))
     >                 CALL PSETC('SYMBOL_COLOUR','RED')
                     x(1)=float((im-1)*nldt+il)-shift+imod*xinc
                     if((imod.eq.nmod).and.(mult.eq.1))x(1)=
     >                  float((im-1)*nldt+il)-shift+(nmod+1)*xinc
                     y1(1)=conf(im,il,imod,ityp)
                     if(y1(1).lt.ymax)y1(1)=ymax
                     CALL PSET1R('SYMBOL_INPUT_X_POSITION',x,1)
                     CALL PSET1R('SYMBOL_INPUT_Y_POSITION',y1,1)
                     if(nmod.le.5)symh=0.16
                     if((nmod.gt.5).and.(nmod.le.10))symh=0.12
                     if((nmod.gt.10).and.(nmod.le.30))symh=0.08
                     if(nmod.gt.30)symh=0.05
                     CALL PSETR('SYMBOL_HEIGHT',symh)
                     if(dia.ne.'RSR')CALL PSYMB
                     symh=symh+0.01
                     imarker(1)=1
                     CALL PSETC('SYMBOL_COLOUR','BLACK')
                     if(dia.ne.'RSR')CALL PSYMB
                  enddo
               enddo
            enddo
            deallocate (x,                                   stat=istat)
            deallocate (y1,                                  stat=istat)
c
            CALL PNEW('SUPER_PAGE')
         enddo !ityp loop
         CALL PCLOSE
      enddo !it loop
      deallocate (val,                                       stat=istat)
      deallocate (conf,                                      stat=istat)
c
      goto 999
 998  write(*,*)'Sorry, no success :('
      call abort
 999  write(*,*)'program seems to be successfully finished :)'
c
      end
