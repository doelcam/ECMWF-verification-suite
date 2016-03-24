c
c----------------------------------------------------------------------c
c     PROGRAM plot_acc_bar                P. Doblas-Reyes 23-Jan-2007  c
c                                                                      c
c     PURPOSE:                                                         c
c     Plots bar diagrams of ACC, perfect-model ACC, ratio spread/RMSE  c
c     spread and MSSS                                                  c
c                                                                      c
c     INPUT:                                                           c
c     DIA_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE  GRIB files    c
c                                                                      c
c     OUTPUT:                                                          c
c     DIA_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE_NAMR.ps        c
c                                                          ps files    c
c                                                                      c
c     USAGE:                                                           c
c     plot_acc_bar.x < nlist                                           c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 plot_acc_bar.f tools.f tools_plot.f                c
c     -o plot_acc_bar.x $MAGLIB $EMOSLIB $NAGLIB                       c
c                                                                      c
c     MODS:                                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      PROGRAM plot_acc_bar
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: x(:), y1(:), y2(:)
      real, allocatable :: xarray(:)
      real, allocatable :: score(:,:,:)
      character*20, allocatable :: clist(:)
      character*4, allocatable :: ticklabel(:)
      real, allocatable :: llist(:)
c
c.... hard coded field definitions
c
      integer nreg_max, nmod_max
      parameter(nreg_max=48,nmod_max=10)
      character*4 namr(nreg_max)
      integer nenm(nmod_max)
      integer typm(nmod_max)
      integer ilsm(nreg_max), rcal(nreg_max)
      real limn(nreg_max), lims(nreg_max)
      real limw(nreg_max), lime(nreg_max)
      character*21 expm(nmod_max)
c
      real eps
      parameter(eps=0.1)
c
c.... other definitions
c
      real topth, botth
      real x_top, x_bot
      real spxl, spyl, pxl, pyl, sbpxl, sbpxp, sbpyl, sbpyp
      real xmax, xmin, ymin, ymax, yint
      real shift, xinc
      integer nx, ny, iyy1, iyy2, imm, idd, itt, cros, anin
      integer ipar, ilev, nf1, nf2, nreg, nmod, boot, nsam, mult
      integer nyear, nensp1, nensr, iy, i, ir, iens, nskip, imod
      integer iunit, exptl, lena, istat
      integer nint, inc, nens, nens_tot, nsamin, ib
      integer ntop, nbot
      character*3 dia
      character*100 yfile, yifile, yrfile, yofile
      character*120 ctitg1, ctitg2, ctitg3, ctitg4
      character*30 mask
      character*20 start
      character*40 variable, regionname
      character*50 diagnostic
      character*25 legend
      character*40 form1, form2
      integer imarker(1)
      character*20 bcolour(7)
      data bcolour/'blue','evergreen','orange','cyan','grey',
     >   'pink','yellow'/
c
      namelist /control/   nx,   ny, nenm,
     >                   iyy1, iyy2,  imm,  idd,  itt,
     >                   cros, ipar, ilev, anin,  nf1,  nf2,
     >                   expm, typm, nmod, mult,  dia, boot, nsam,
     >                  topth,botth, nreg
      namelist /region/  namr, limn, lims, limw, lime, ilsm, rcal
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
      expm = 'scwc'
c
c.... Check the order of the parameters for the indices
c
c   nens:   number of ensemble members
c   iyy1:   start year
c   iyy2:   end year
c   imm:    forecast starting month
c   expm:   model ids
c
      read(5,control)
      write(*,*)'  nx: ', nx
      write(*,*)'  ny: ', ny
      write(*,*)'nenm: ',(nenm(i),i=1,nmod)
      write(*,*)'iyy1: ',iyy1
      write(*,*)'iyy2: ',iyy2
      write(*,*)' imm: ',imm
      write(*,*)' idd: ',idd
      write(*,*)' itt: ',itt
      write(*,*)'cros: ',cros
      write(*,*)'ipar: ',ipar
      write(*,*)'ilev: ',ilev
      write(*,*)'anin: ',anin
      write(*,*)' nf1: ',nf1
      write(*,*)' nf2: ',nf2
      write(*,*)'expm: ',(expm(i),i=1,nmod)
      write(*,*)'typm: ',(typm(i),i=1,nmod)
      write(*,*)'nmod: ',nmod
      write(*,*)'mult: ',mult
      write(*,*)' dia: ',dia
      write(*,*)'boot: ',boot
      write(*,*)'nsam: ',nsam
      write(*,*)'topth: ',topth
      write(*,*)'botth: ',botth
      write(*,*)'nreg: ',nreg
      if(nreg.gt.nreg_max)then
        write(*,*)'Problems with the number of regions'
        goto 998
      endif
      read(5,region)
      write(*,*)'namr: ',(namr(i),i=1,nreg)
      write(*,*)'limn: ',(limn(i),i=1,nreg)
      write(*,*)'lims: ',(lims(i),i=1,nreg)
      write(*,*)'limw: ',(limw(i),i=1,nreg)
      write(*,*)'lime: ',(lime(i),i=1,nreg)
      write(*,*)'ilsm: ',(ilsm(i),i=1,nreg)
      write(*,*)'rcal: ',(rcal(i),i=1,nreg)
c
      nyear=iyy2-iyy1+1
      if(nmod.gt.1)then
        nensp1=nmod
      else
        nens=nenm(nmod)
        nensp1=nens+1
        if((dia.eq.'SPR').or.(dia.eq.'RSR'))nensp1=1
      endif
      if(boot.eq.0)nsam=0
      allocate (score(nyear+1,nensp1,0:nsam),                stat=istat)
      allocate (xarray(nsam),                                stat=istat)
c
c.... loop over regions
c
      do ir=1,nreg
         if(rcal(ir).eq.1)then
           nens_tot=0
           do imod=1,nmod
              exptl=lena(expm(imod))
              nens=nenm(imod)
              if(nmod.gt.1)then
                nskip=nens*10+8
                if((dia.eq.'SPR').or.(dia.eq.'RSR'))nskip=8
                write(form2,'(a,i4,a)')'(',nskip,'x,f10.4)'
                nensr=1
                if(imod.lt.nmod)then
                  nens_tot=nens_tot+nens
                else
                  nens=nens_tot
                endif
              else
                nensr=nens+1
                if((dia.eq.'SPR').or.(dia.eq.'RSR'))nensr=1
                write(form2,'(a,i2.2,a)')'(8x,',nensr,'f10.4)'
                if(nensr.ge.100)write(form2,'(a,i3.3,a)')
     >             '(8x,',nensr,'f10.4)'
                nens_tot=nens
              endif
              write(*,*)form2
c
c.... open input file
c
              write(form1,'(a,i2.2,a)')'(a',exptl,')'
              yifile='ACC_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE'
              write(yifile(       1:       3),'(a3)')dia
              write(yifile(       5: 4+exptl),form1)expm(imod)(1:exptl)
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
              yifile=yifile(1:lena(yifile))//'_'//namr(ir)
              write(*,*)'open input file: ',yifile
              open(10,file=yifile,form='formatted',status='unknown')
              yrfile=yifile
              if(boot.eq.1)then
                write(yrfile(51+exptl:52+exptl),'(a2)')'_S'
                write(yrfile(53+exptl:57+exptl),'(i5.5)')nsam
                yfile=yrfile(1:lena(yrfile))//'.ran'
                write(*,*)'Open input file ',yfile(1:lena(yfile))
                open(11,file=yfile,form='formatted')
                read(11,'(i5)')nsamin
                if(nsam.ne.nsamin)then
                  write(*,*)'Error in the sampling size'
                  goto 998
                endif
                ntop=int(topth*nsam/100)+1
                nbot=int(botth*nsam/100)
                if(nbot.eq.0)nbot=1
              endif
c
c.... open output file
c
              yofile='psfiles/'//yrfile(1:lena(yrfile))//'_bar.ps'
              write(*,*)'Output file ',yofile(1:lena(yofile)),
     >           ' is a postscript file'
c
c.... reading data
c
              read(10,*)
              if(nmod.gt.1)then
                do iy=1,nyear
                   read(10,form2)score(iy,imod,0)
                enddo
                read(10,form2)score(nyear+1,imod,0)
              else
                do iy=1,nyear
                   read(10,form2)(score(iy,iens,0),iens=1,nensr)
                enddo
                read(10,form2)(score(nyear+1,iens,0),iens=1,nensr)
              endif
c
c.... results from bootstrap
c
              if(boot.eq.1)then
                do ib=1,nsam
c                  write(*,*)'Sample ',ib
                   if(nmod.gt.1)then
                     read(11,form2)score(nyear+1,imod,ib)
                   else
                     read(11,form2)(score(nyear+1,iens,ib),iens=1,nensr)
                   endif
                enddo !ib loop
                close(11)
              endif
              close(10)
           enddo !imod loop
c
c.... searching for min/max
c.... required to draw the axes of MSS, RSR and SPR
c
           x_top=0.
           x_bot=0.
           do iens=1,nensp1
              do iy=1,nyear
                 do ib=0,nsam
                    if(score(iy,iens,ib).gt.x_top)
     >                 x_top=score(iy,iens,ib)
                    if(score(iy,iens,ib).lt.x_bot)
     >                 x_bot=score(iy,iens,ib)
                 enddo
              enddo
           enddo
           write(*,*)'Range ',x_bot,x_top
c
c.... looking for the title
c
           nint=13
           allocate (clist(nint),                            stat=istat)
           allocate (llist(nint),                            stat=istat)
           call titles(imm, ipar, ilev, namr(ir), dia,
     >        start, variable, regionname,
     >        diagnostic, nint, clist, llist, xmax, xmin)
           write(*,*)dia,' bar diagram for region ',regionname
C
C     INITIALISATION DE MAGICS
C
           CALL POPEN
           CALL PSETC('PS_DEVICE','ps_a4')
           CALL PSETC('PS_FILE_NAME',yofile)
C          CALL PSETC('WORKSTATION_1','PS_COL_A4_VERTICAL')
C          CALL PSETC('WORKSTATION_1','PS_COL_A4_HORIZONTAL')
           CALL PSETC('LAYOUT','POSITIONAL')
C
C     PLOTTING SECTION
C
           SPXL=29.5
           SPYL=21.
           CALL PSETR('SUPER_PAGE_X_LENGTH',SPXL)
           CALL PSETR('SUPER_PAGE_Y_LENGTH',SPYL)
           CALL PSETC('PLOT_DIRECTION','HORIZONTAL')
c          CALL PSETC('PLOT_DIRECTION','VERTICAL')
c          CALL PSETC('PLOT_START','TOP')
           CALL PSETC('PLOT_START','BOTTOM')
           CALL PSETR('PAGE_X_GAP',0.)
           CALL PSETR('PAGE_Y_GAP',0.)
c
           CALL PSETC('PAGE_FRAME','OFF')
           CALL PSETC('PAGE_FRAME_COLOUR','BLACK')
           CALL PSETC('PAGE_ID_LINE','OFF')
           CALL PSETC('SUBPAGE_FRAME_COLOUR','BLACK')
C
           CALL PSETC('LEGEND_BORDER','OFF')
           CALL PSETC('LEGEND_BORDER_COLOUR','BLACK')
           CALL PSETC('LEGEND_TEXT_COLOUR','BLACK')
           CALL PSETC('LEGEND_TEXT_QUALITY','HIGH')
c          CALL PSETR('LEGEND_TEXT_MAXIMUM_HEIGHT',0.8)
           CALL PSETR('LEGEND_ENTRY_MAXIMUM_WIDTH',5.0)
c          CALL PSETC('LEGEND_TITLE','ON')
c          CALL PSETC('LEGEND_DISPLAY_TYPE','CONTINUOUS')
c
c.... general layout
c
           PXL=SPXL-eps
           PYL=SPYL/5-eps
           SBPXL=.95*PXL
           SBPXP=(PXL-SBPXL)/2
           SBPYL=.95*PYL
           SBPYP=(PYL-SBPYL)/2.
           SBPYP=0.
c          write(*,*)SBPYP,SBPYL,SBPXP,SBPXL
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
C          CALL PSETC('TEXT_MODE','TITLE')
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
           WRITE(CTITG1,'(A,A,A)')
     >        diagnostic(1:lena(diagnostic)),' for ',
     >        regionname(1:lena(regionname))
           if(ilsm(ir).eq.0)mask=' (land and sea points)'
           if(ilsm(ir).eq.1)mask=' (land points only)'
           if(ilsm(ir).eq.-1)mask=' (sea points only)'
           CTITG1=CTITG1(1:lena(CTITG1))//mask(1:lena(mask))
           WRITE(CTITG2,'(A)')variable(1:lena(variable))
           form1='(a,i4.4,a,i4.4,a,a,a,i1.1,a,i1.1)'
           if(nf1.gt.9)form1='(a,i4.4,a,i4.4,a,a,a,i2.2,a,i2.2)'
           if((nf1.le.9).and.(nf2.gt.9))
     >        form1='(a,i4.4,a,i4.4,a,a,a,i1.1,a,i2.2)'
           if((nf1.le.9).and.(nf2.gt.99))
     >        form1='(a,i4.4,a,i4.4,a,a,a,i1.1,a,i3.3)'
           if((nf1.gt.9).and.(nf2.gt.9))
     >        form1='(a,i4.4,a,i4.4,a,a,a,i2.2,a,i2.2)'
           if((nf1.gt.9).and.(nf2.gt.99))
     >        form1='(a,i4.4,a,i4.4,a,a,a,i2.2,a,i3.3)'
           if(nf1.gt.99)form1='(a,i4.4,a,i4.4,a,a,a,i3.3,a,i3.3)'
           WRITE(CTITG3,form1)
     >        'Hindcast period ',iyy1,'-',iyy2,
     >        ' with start in ',start(1:lena(start)),
     >        ' and averaging period ',nf1,' to ',nf2
           write(*,*)CTITG1
           write(*,*)CTITG2
           write(*,*)CTITG3
           CALL PSETC('TEXT_LINE_1',CTITG1(1:lena(CTITG1)))
           CALL PSETC('TEXT_LINE_2',CTITG2(1:lena(CTITG2)))
           CALL PSETC('TEXT_LINE_3',CTITG3(1:lena(CTITG3)))
           CALL PTEXT
           if(boot.eq.1)then
             CALL PSETC('TEXT_LINE_1','')
             CALL PSETC('TEXT_LINE_2','')
             CALL PSETC('TEXT_LINE_3','')
             CALL PSETR('TEXT_REFERENCE_CHARACTER_HEIGHT',0.4)
             WRITE(CTITG4,'(a,i2.2,a,i5,a)')'Bars over AVER are ',
     >          int(topth-botth),'% conf. intervals computed with ',
     >          nsam,' samples'
             CALL PSETC('TEXT_LINE_4',CTITG4(1:lena(CTITG4)))
             write(*,*)CTITG4
             CALL PTEXT
           endif
           CALL PNEW('PAGE')
c
c.... setting the page to plot the bar diagram
c
           PXL=SPXL-eps
           PYL=4*SPYL/5-eps
           SBPXL=.80*PXL
           SBPXP=(PXL-SBPXL)/2
           SBPYL=.85*PYL
           SBPYP=(PYL-SBPYL)/2.
c          write(*,*)SBPYP,SBPYL,SBPXP,SBPXL
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
           allocate (ticklabel(nyear+3),                     stat=istat)
           inc=4
           if(nyear.lt.10)then
             inc=1
           elseif(nyear.lt.25)then
             inc=2
           elseif(nyear.lt.50)then
             inc=3
           endif
           do iy=1,nyear+1
              write(ticklabel(iy),'(a)')'    '
           enddo
           do iy=2,nyear+1,inc
              write(ticklabel(iy),'(i4.4)')iyy1+iy-2
           enddo
           ticklabel(nyear+2)='    '
           ticklabel(nyear+3)='AVER'
           CALL PSETC('AXIS_ORIENTATION','HORIZONTAL')
           CALL PSETC('AXIS_POSITION','BOTTOM')
           CALL PSETC('AXIS_TYPE','REGULAR')
           CALL PSETR('AXIS_TITLE_HEIGHT',0.20)
           CALL PSETC('AXIS_TITLE','OFF')
c          CALL PSETC('AXIS_TITLE_TEXT','Forecast range')
           CALL PSETR('AXIS_MIN_VALUE',0.)
           CALL PSETR('AXIS_MAX_VALUE',float(nyear+3))
           CALL PSETR('AXIS_TICK_INTERVAL',1.)
           CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',1)
           CALL PSETC('AXIS_TICK_LABEL_TYPE','LABEL_LIST')
           CALL PSETC('AXIS_TICK_LABEL_FIRST','OFF')
           CALL PSETC('AXIS_TICK_LABEL_LAST','ON')
           CALL PSET1C('AXIS_TICK_LABEL_LIST',ticklabel,nyear+3)
           CALL PAXIS
           deallocate (ticklabel,                            stat=istat)
c
           ymax=1.0
           ymin=-1.0
           yint=0.2
           if(dia.eq.'MSS')then
             yint=0.5
             if((x_bot.lt.-1.).and.(x_bot.ge.-2.))ymin=-2.
             if((x_bot.lt.-2.).and.(x_bot.ge.-3.))ymin=-3.
             if((x_bot.lt.-3.).and.(x_bot.ge.-4.))ymin=-4.
             if((x_bot.lt.-4.).and.(x_bot.ge.-5.))ymin=-5.
             if((x_bot.lt.-5.).and.(x_bot.ge.-6.))ymin=-6.
             if((x_bot.lt.-6.).and.(x_bot.ge.-7.))ymin=-7.
           endif
           if(dia.eq.'SPR')then
             ymin=0.25
             yint=0.25
             ymax=1.0
             if((x_top.gt.1.0).and.(x_top.le.1.5))ymax=1.5
             if((x_top.gt.1.5).and.(x_top.le.2.0))ymax=2.0
             if((x_top.gt.2.0).and.(x_top.le.2.5))ymax=2.5
             if((x_top.gt.2.5).and.(x_top.le.3.0))ymax=3.0
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
           CALL PSETC('AXIS_TITLE','OFF')
           CALL PSETC('AXIS_TICK_LABEL_TYPE','NUMBER')
           CALL PSETC('AXIS_TICK_LABEL_FIRST','ON')
           CALL PSETC('AXIS_TICK_LABEL_LAST','ON')
           CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',1)
           CALL PSETR('AXIS_MIN_VALUE',ymin)
           CALL PSETR('AXIS_MAX_VALUE',ymax)
           CALL PSETR('AXIS_TICK_INTERVAL',yint)
           CALL PAXIS
c
c.... plot the data
c
           allocate (x(nyear),                               stat=istat)
           allocate (y1(nyear),                              stat=istat)
           allocate (y2(nyear),                              stat=istat)
           CALL PSETC('GRAPH_TYPE','BAR')
           CALL PSETC('GRAPH_SHADE','ON')
           CALL PSETC('GRAPH_SHADE_STYLE','AREA_FILL')
           CALL PSETC('GRAPH_SHADE_COLOUR','RED')
c
           shift=0.5
           xinc=0.04
           xinc=0.60/nensp1
           if(((dia.eq.'SPR').or.(dia.eq.'RSR')).and.(nmod.eq.1))then
             shift=0.
             xinc=0.
           endif
           do iens=1,nensp1
              do iy=1,nyear
                 x(iy)=float(iy)-shift+iens*xinc
                 if(iens.eq.nensp1)x(iy)=
     >              float(iy)-shift+(nensp1+1)*xinc
                 if(score(iy,iens,0).gt.0.)then
                   y1(iy)=0.
                   y2(iy)=score(iy,iens,0)
                 else
                   y1(iy)=score(iy,iens,0)
                   y2(iy)=0.
                 endif
c                write(*,*)iy,iens,score(iy,iens,0),y1(iy),y2(iy)
              enddo
c
c.... bars and legends
c
              if(nmod.gt.1)then
                CALL PSETC('GRAPH_SHADE_COLOUR',bcolour(iens))
                CALL PSETC('GRAPH_BAR_COLOUR',bcolour(iens))
                if(iens.eq.nmod)CALL PSETC('GRAPH_SHADE_COLOUR','RED')
                CALL PSETC('LEGEND','ON')
                if(typm(iens).eq.1)then
                  write(legend,'(a,a,i2,a)')expm(iens)(2:5),
     >               ' (',nenm(iens),')'
                  CALL PSETR('GRAPH_BAR_WIDTH',0.20)
                else
                  write(legend,'(a,a,a,a,i2,a)')expm(iens)(5:8),'-',
     >               expm(iens)(10:13),' (',nenm(iens),')'
                  CALL PSETR('GRAPH_BAR_WIDTH',0.15)
                endif
                CALL PSETC('LEGEND_USER_TEXT',legend(1:lena(legend)))
                write(*,*)legend
              else
                if(iens.eq.nensp1)then
                  CALL PSETC('GRAPH_SHADE_COLOUR','RED')
                  CALL PSETC('GRAPH_BAR_COLOUR','RED')
                  CALL PSETR('GRAPH_BAR_WIDTH',0.15)
                  CALL PSETC('LEGEND','ON')
                  if(typm(1).eq.1)then
                    write(legend,'(a,a,i2,a)')expm(1)(2:5),
     >                 ' (',nenm(1),')'
                  else
                    write(legend,'(a,a,a,a,i2,a)')expm(1)(5:8),'-',
     >                 expm(1)(10:13),' (',nenm(1),')'
                  endif
                  CALL PSETC('LEGEND_USER_TEXT',legend(1:lena(legend)))
                  write(*,*)legend
                else
                  CALL PSETC('GRAPH_SHADE_COLOUR','TURQUOISE')
                  CALL PSETC('GRAPH_BAR_COLOUR','TURQUOISE')
                  if(nensp1.le.10)then
                    CALL PSETR('GRAPH_BAR_WIDTH',0.04)
                  endif
                  if((nensp1.gt.10).and.(nensp1.le.30))then
                    CALL PSETR('GRAPH_BAR_WIDTH',0.015)
                  endif
                  if(nensp1.gt.30)then
                    CALL PSETR('GRAPH_BAR_WIDTH',0.005)
                  endif
                  CALL PSETC('LEGEND','OFF')
                endif
              endif
              CALL PSET1R('GRAPH_BAR_X_VALUES',x,nyear)
              CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',y1,nyear)
              CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',y2,nyear)
              CALL PGRAPH
           enddo
           deallocate (x,                                    stat=istat)
           deallocate (y1,                                   stat=istat)
           deallocate (y2,                                   stat=istat)
c.... confidence intervals
c
           allocate (x(1),                                   stat=istat)
           allocate (y1(1),                                  stat=istat)
           allocate (y2(1),                                  stat=istat)
           iy=nyear+1
           CALL PSETC('LEGEND','OFF')
           if(boot.eq.1)then
             imarker(1)=28
             CALL PSETC('SYMBOL_POSITION_MODE','GRAPH')
             CALL PSETC('SYMBOL_TYPE','MARKER')
             CALL PSET1I('SYMBOL_INPUT_MARKER_LIST',imarker,1)
             CALL PSETC('SYMBOL_COLOUR','BLACK')
             CALL PSETC('LEGEND','OFF')
             do iens=1,nensp1
                do ib=1,nsam
                   xarray(ib)=score(iy,iens,ib)
                enddo
                call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                write(*,'(a,f6.3,a,f6.3,a)')'(',x_bot,',',x_top,')'
                if(nmod.gt.1)then
                  if((iens.eq.nmod).and.(mult.eq.1))then
                    CALL PSETR('GRAPH_BAR_WIDTH',0.15)
                    CALL PSETR('SYMBOL_HEIGHT',0.20)
                    CALL PSETC('GRAPH_SHADE_COLOUR','RED')
                    CALL PSETC('GRAPH_BAR_COLOUR','RED')
                  else
                    CALL PSETR('GRAPH_BAR_WIDTH',0.10)
                    CALL PSETR('SYMBOL_HEIGHT',0.15)
                    CALL PSETC('GRAPH_SHADE_COLOUR',bcolour(iens))
                    CALL PSETC('GRAPH_BAR_COLOUR',bcolour(iens))
                  endif
                else
                  if(iens.eq.nensp1)then
                    CALL PSETR('GRAPH_BAR_WIDTH',0.15)
                    CALL PSETC('GRAPH_SHADE_COLOUR','RED')
                    CALL PSETC('GRAPH_BAR_COLOUR','RED')
                  else
                    if(nensp1.le.10)then
                      CALL PSETR('GRAPH_BAR_WIDTH',0.04)
                      CALL PSETR('SYMBOL_HEIGHT',0.11)
                    endif
                    if((nensp1.gt.10).and.(nensp1.le.30))then
                      CALL PSETR('GRAPH_BAR_WIDTH',0.015)
                      CALL PSETR('SYMBOL_HEIGHT',0.08)
                    endif
                    if(nensp1.gt.30)then
                      CALL PSETR('GRAPH_BAR_WIDTH',0.005)
                      CALL PSETR('SYMBOL_HEIGHT',0.05)
                    endif
                    CALL PSETC('GRAPH_SHADE_COLOUR','TURQUOISE')
                    CALL PSETC('GRAPH_BAR_COLOUR','TURQUOISE')
                  endif
                endif
                x(1)=nyear+2-shift+iens*xinc
                if(iens.eq.nensp1)x(1)=
     >             nyear+2-shift+(nensp1+1)*xinc
                y1(1)=x_bot
                y2(1)=x_top
                CALL PSET1R('GRAPH_BAR_X_VALUES',x,1)
                CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',y1,1)
                CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',y2,1)
                CALL PGRAPH
                y1(1)=score(nyear+1,iens,0)
                CALL PSET1R('SYMBOL_INPUT_X_POSITION',x,1)
                CALL PSET1R('SYMBOL_INPUT_Y_POSITION',y1,1)
                CALL PSYMB
             enddo
           else
             do iens=1,nensp1
                x(1)=nyear+2-shift+iens*xinc
                if(iens.eq.nensp1)x(1)=
     >             nyear+2-shift+(nensp1+1)*xinc
                if(score(iy,iens,0).gt.0.)then
                  y1(1)=0.
                  y2(1)=score(iy,iens,0)
                else
                  y1(1)=score(iy,iens,0)
                  y2(1)=0.
                endif
                if(iens.eq.nensp1)then
                  CALL PSETR('GRAPH_BAR_WIDTH',0.15)
                  CALL PSETC('GRAPH_SHADE_COLOUR','RED')
                  CALL PSETC('GRAPH_BAR_COLOUR','RED')
                else
                  if(nensp1.le.10)then
                    CALL PSETR('GRAPH_BAR_WIDTH',0.04)
                    CALL PSETR('SYMBOL_HEIGHT',0.11)
                  endif
                  if((nensp1.gt.10).and.(nensp1.le.30))then
                    CALL PSETR('GRAPH_BAR_WIDTH',0.015)
                    CALL PSETR('SYMBOL_HEIGHT',0.08)
                  endif
                  if(nensp1.gt.30)then
                    CALL PSETR('GRAPH_BAR_WIDTH',0.005)
                    CALL PSETR('SYMBOL_HEIGHT',0.05)
                  endif
                  CALL PSETC('GRAPH_SHADE_COLOUR','TURQUOISE')
                  CALL PSETC('GRAPH_BAR_COLOUR','TURQUOISE')
                endif
                CALL PSET1R('GRAPH_BAR_X_VALUES',x,1)
                CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',y1,1)
                CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',y2,1)
                CALL PGRAPH
             enddo
           endif
           deallocate (x,                                    stat=istat)
           deallocate (y1,                                   stat=istat)
           deallocate (y2,                                   stat=istat)
c
c.... reference line
c
           allocate (x(0:nyear+3),                           stat=istat)
           allocate (y1(0:nyear+3),                          stat=istat)
           CALL PSETC('LEGEND','OFF')
           do iy=0,nyear+3
              x(iy)=float(iy)
              y1(iy)=0.
              if((dia.eq.'SPR').or.(dia.eq.'RSR'))y1(iy)=1.
           enddo
           CALL PSETC('GRAPH_TYPE','CURVE')
           CALL PSETC('GRAPH_LINE_COLOUR','BLACK')
           CALL PSET1R('GRAPH_CURVE_X_VALUES',x,nyear+4)
           CALL PSET1R('GRAPH_CURVE_Y_VALUES',y1,nyear+4)
           CALL PGRAPH
           deallocate (x,                                    stat=istat)
           deallocate (y1,                                   stat=istat)
           deallocate (y2,                                   stat=istat)
c
           CALL PNEW('PAGE')
c
           CALL PCLOSE
         endif
      enddo
c
      goto 999
 998  write(*,*)'Sorry, no success :('
      call abort
 999  write(*,*)'program seems to be successfully finished :)'
c
      end
