c
c----------------------------------------------------------------------c
c     PROGRAM plot_rel_bar                P. Doblas-Reyes  6-Feb-2007  c
c                                                                      c
c     PURPOSE:                                                         c
c     Plots the probabilistic scores for regions in bar diagrams       c
c     Includes confidence intervals for the scores                     c
c                                                                      c
c     INPUT:                                                           c
c     BRV_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE_NAMR           c
c                                                          ASCII file  c
c                                                                      c
c     OUTPUT:                                                          c
c     REL_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE_NAMR_bar.ps    c
c                                                             ps file  c
c                                                                      c
c     USAGE:                                                           c
c     plot_rel_bar.x < nlist                                           c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 plot_rel_bar.f tools.f tools_plot.f                c
c     -o plot_rel_bar.x $MAGLIB $EMOSLIB $NAGLIB                       c
c                                                                      c
c     MODS:                                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      PROGRAM plot_rel_bar
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: bss_ful(:,:,:), bss_red(:,:,:)
      real, allocatable :: bss_inf(:,:,:)
      real, allocatable :: rel(:,:,:), bss_rel(:,:,:)
      real, allocatable :: res(:,:,:), bss_res(:,:,:)
      real, allocatable :: unc(:,:,:), totocc_rel(:,:,:)
      real, allocatable :: ave_prob(:,:,:), sd_prob(:,:,:)
      real, allocatable :: roc(:,:,:), rocsup(:,:,:), roclow(:,:,:)
      real, allocatable :: xarray(:)
      real, allocatable :: x(:), y1(:), y2(:), score(:,:)
      character*20, allocatable :: clist(:)
      character*4, allocatable :: ticklabel(:)
      real, allocatable :: llist(:)
c
c.... hard coded field definitions
c
      integer nevt_max
      parameter(nevt_max=10)
      integer pevt(nevt_max)
      integer nreg_max
      parameter(nreg_max=48)
      character*4 namr(nreg_max)
      real limn(nreg_max), lims(nreg_max)
      real limw(nreg_max), lime(nreg_max)
      integer ilsm(nreg_max), rcal(nreg_max)
      integer nmod_max
      parameter(nmod_max=10)
      integer nenm(nmod_max)
      integer typm(nmod_max)
      character*21 expm(nmod_max)
c
      real eps
      parameter(eps=0.1)
c
c.... other definitions
c
      integer tevent(7)
      character*30 nevent(7)
      data tevent/50,33,67,25,75,20,80/
      data nevent/'above the median',
     >   'below the lower tercile','above the upper tercile',
     >   'below the lower quartile','above the upper quartile',
     >   'below the lower quintile','above the upper quintile'/
      integer imarker(1)
      character*20 bcolour(7)
      data bcolour/'blue','evergreen','orange','cyan','grey',
     >   'pink','yellow'/
c
c.... other definitions
c
      integer nx, ny, nens
      integer iyy1, iyy2, imm, idd, itt
      integer cros, ipar, ilev, anin,  nf1,  nf2, nmod, mult
      integer nreg, nevt, boot, nsam, iprb, npro, permeth
      integer i, nyear, nbin, istat, ievt
      integer nprob, ir, lena, iprob, ibin
      integer ntop, nbot, nsamin, ib, imod, iy, inc
      integer exptl, nint
      integer ntest, nens_tot
      real topth, botth
      real x_top, x_bot
      real xmax, xmin
      real spxl, spyl, sbpxl, sbpyl, sbpxp, sbpyp, pxl, pyl
      real obsocc_rel
      real ymax, ymin, yint, h, shift, xinc
      character*110 yfile, yifile, yrfile, yofile
      character*3 dia
      character*120 ctitg1, ctitg2, ctitg3
      character*120 ctitg4, ctitg5, ctitg6
      character*30 mask
      character*20 start
      character*40 variable, regionname
      character*50 diagnostic
      character*25 legend
      character*30 form1
      character*21 expt
c
c.... namelist
c
      namelist /control/   nx,   ny, nenm,
     >                   iyy1, iyy2,  imm,  idd,  itt,
     >                   cros, ipar, ilev, anin,  nf1,  nf2,
     >                   expm, typm, nmod, mult,  dia, nevt, pevt,
     >                permeth, boot, nsam, iprb, npro,
     >                  topth,botth, nreg
      namelist /region/  namr, limn, lims, limw, lime, ilsm, rcal
c
c.... set default values and read input namelist
c
      nens  = 9
      iyy1  = 1980
      iyy2  = 2001
      imm   = 11
      expt  = 'scwf'
c
c.... Check the order of the parameters for the indices
c
c   nenm:   number of ensemble members
c   iyy1:   start year
c   iyy2:   end year
c   imm:    forecast starting month
c   expm:   model ids
c
      read(5,control)
      write(*,*)'  nx: ',nx
      write(*,*)'  ny: ',ny
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
      write(*,*)'nevt: ',nevt
      write(*,*)'pevt: ',(pevt(i),i=1,nevt)
      write(*,*)'permeth: ',permeth
      write(*,*)'boot: ',boot
      write(*,*)'nsam: ',nsam
      write(*,*)'iprb: ',iprb
      write(*,*)'npro: ',npro
      write(*,*)'topth: ',topth
      write(*,*)'botth: ',botth
c
      if(nreg.gt.nreg_max)then
        write(*,*)'Problems with the number of regions'
        goto 998
      endif
      read(5,region)
      write(*,*)'nreg: ',nreg
      write(*,*)'namr: ',(namr(i),i=1,nreg)
      write(*,*)'limn: ',(limn(i),i=1,nreg)
      write(*,*)'lims: ',(lims(i),i=1,nreg)
      write(*,*)'limw: ',(limw(i),i=1,nreg)
      write(*,*)'lime: ',(lime(i),i=1,nreg)
      write(*,*)'ilsm: ',(ilsm(i),i=1,nreg)
      write(*,*)'rcal: ',(rcal(i),i=1,nreg)
c
      nyear  = iyy2-iyy1+1
      if(boot.eq.0)nsam=0
c
c.... allocate fields
c
      allocate (bss_ful(nyear+1,nmod,0:nsam),                stat=istat)
      allocate (bss_red(nyear+1,nmod,0:nsam),                stat=istat)
      allocate (bss_inf(nyear+1,nmod,0:nsam),                stat=istat)
      allocate (rel(nyear+1,nmod,0:nsam),                    stat=istat)
      allocate (bss_rel(nyear+1,nmod,0:nsam),                stat=istat)
      allocate (res(nyear+1,nmod,0:nsam),                    stat=istat)
      allocate (bss_res(nyear+1,nmod,0:nsam),                stat=istat)
      allocate (unc(nyear+1,nmod,0:nsam),                    stat=istat)
      allocate (totocc_rel(nyear+1,nmod,0:nsam),             stat=istat)
      allocate (ave_prob(nyear+1,nmod,0:nsam),               stat=istat)
      allocate (sd_prob(nyear+1,nmod,0:nsam),                stat=istat)
      allocate (roc(nyear+1,nmod,0:nsam),                    stat=istat)
      allocate (rocsup(nyear+1,nmod,0:nsam),                 stat=istat)
      allocate (roclow(nyear+1,nmod,0:nsam),                 stat=istat)
      allocate (xarray(nsam),                                stat=istat)
c
c.... loop over all regions
c
      do ir=1,nreg
         if(rcal(ir).eq.1)then
c
c.... looking for the title
c
           nint=13
           allocate (clist(nint),                            stat=istat)
           allocate (llist(nint+1),                          stat=istat)
           call titles(imm, ipar, ilev, namr(ir), dia,
     >        start, variable, regionname,
     >        diagnostic, nint, clist, llist, xmax, xmin)
           write(*,*)diagnostic,' for region ',regionname
c
c.... loop over the events
c
           do ievt=1,nevt
c
c.... open input file
c
              nens_tot=0
              do imod=1,nmod
c
c.... iprb controls the way the probabilities are binned
c.... if 1, the number of probability bins is the ensemble size plus 1
c.... if not 1, the number of bins is a parameter
c.... nbin is used in the potential economic value
c
                 nprob=npro
                 nens=nenm(imod)
                 if(iprb.eq.1)nprob=nens+1
                 nbin=nens+1
                 write(*,'(a,3i3.3)')
     >              'Parameters for probabilistic scores ',
     >              iprb,nprob,nbin
                 nens_tot=nens_tot+nens
c
                 exptl=lena(expm(imod))
                 write(form1,'(a,i2.2,a)')'(a',exptl,')'
                 yifile=
     >'BRV_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_THR_MM_BIN_AAA-EEE'
                 write(yifile(       5: 4+exptl),form1)
     >expm(imod)(1:exptl)
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
                 write(yifile(38+exptl:39+exptl),'(a2)')'_T'
                 write(yifile(40+exptl:41+exptl),'(i2.2)')pevt(ievt)
                 write(yifile(42+exptl:43+exptl),'(a2)')'_M'
                 write(yifile(44+exptl:44+exptl),'(i1.1)')permeth
                 write(yifile(45+exptl:46+exptl),'(a2)')'_B'
                 write(yifile(47+exptl:48+exptl),'(i2.2)')nprob
                 write(yifile(49+exptl:49+exptl),'(a1)')'_'
                 write(yifile(50+exptl:52+exptl),'(i3.3)')nf1
                 write(yifile(53+exptl:53+exptl),'(a1)')'-'
                 write(yifile(54+exptl:56+exptl),'(i3.3)')nf2
                 yifile=yifile(1:lena(yifile))//'_'//namr(ir)
                 write(*,*)'open input file: ',yifile(1:lena(yifile))
                 open(10,file=yifile,form='formatted',status='unknown')
                 yrfile=yifile
                 if(boot.eq.1)then
                   write(yrfile(62+exptl:63+exptl),'(a2)')'_S'
                   write(yrfile(64+exptl:68+exptl),'(i5.5)')nsam
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
                 else
                   nsam=0
                 endif
                 yofile='psfiles/'//yrfile(1:lena(yrfile))//'_bar.ps'
                 write(yofile(9:11),'(a3)')dia
                 write(*,*)'Output file ',yofile(1:lena(yofile)),
     >              ' is a postscript file'
c
c.... reading data
c.... skip the line with the lead time
c
                 read(10,*)
c
c.... then read the results for individual years
c
                 do iy=1,nyear
                    read(10,'(8x,13f10.4)')
     >                 bss_ful(iy,imod,0),bss_red(iy,imod,0),
     >                 bss_inf(iy,imod,0),
     >                 rel(iy,imod,0),bss_rel(iy,imod,0),
     >                 res(iy,imod,0),bss_res(iy,imod,0),
     >                 totocc_rel(iy,imod,0),
     >                 ave_prob(iy,imod,0),sd_prob(iy,imod,0),
     >                 roc(iy,imod,0),
     >                 rocsup(iy,imod,0),roclow(iy,imod,0)
                 enddo
c
c.... skip the results for the attributes diagram
c
                 do iprob=1,nprob
                    read(10,*)
                 enddo
c
c.... read the scores
c
                 read(10,'(11f10.4)')bss_ful(nyear+1,imod,0),
     >              bss_red(nyear+1,imod,0),
     >              bss_inf(nyear+1,imod,0),rel(nyear+1,imod,0),
     >              bss_rel(nyear+1,imod,0),res(nyear+1,imod,0),
     >              bss_res(nyear+1,imod,0),unc(nyear+1,imod,0),
     >              totocc_rel(nyear+1,imod,0),
     >              ave_prob(nyear+1,imod,0),sd_prob(nyear+1,imod,0)
c
c.... ROC skill score
c
                 read(10,'(3f10.4)')roc(nyear+1,imod,0),
     >              rocsup(nyear+1,imod,0),roclow(nyear+1,imod,0)
c
c.... results from bootstrap
c
                 if(boot.eq.1)then
                   do ib=1,nsam
                      do iprob=1,nprob
                         read(11,*)
                      enddo
                      read(11,'(11f10.4)')bss_ful(nyear+1,imod,ib),
     >                   bss_red(nyear+1,imod,ib),
     >                   bss_inf(nyear+1,imod,ib),rel(nyear+1,imod,ib),
     >                   bss_rel(nyear+1,imod,ib),res(nyear+1,imod,ib),
     >                   bss_res(nyear+1,imod,ib),unc(nyear+1,imod,ib),
     >                   totocc_rel(nyear+1,imod,ib),
     >                   ave_prob(nyear+1,imod,ib),
     >                   sd_prob(nyear+1,imod,ib)
                      read(11,'(3f10.4)')roc(nyear+1,imod,ib),
     >                   rocsup(nyear+1,imod,ib),roclow(nyear+1,imod,ib)
c
c.... skip HR/FR and ROC with nbin
c
                      do ibin=1,nbin+1
                         read(11,*)
                      enddo
                   enddo !ib loop
                 endif
                 close(10)
                 close(11)
              enddo !imod loop
C
C     INITIALISATION DE MAGICS
C
              CALL POPEN
              CALL PSETC('PS_DEVICE','ps_a4')
              CALL PSETC('PS_FILE_NAME',yofile)
C             CALL PSETC('WORKSTATION_1','PS_COL_A4_VERTICAL')
C             CALL PSETC('WORKSTATION_1','PS_COL_A4_HORIZONTAL')
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
C             CALL PSETC('PLOT_DIRECTION','VERTICAL')
c             CALL PSETC('PLOT_START','TOP')
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
c             CALL PSETR('LEGEND_TEXT_MAXIMUM_HEIGHT',0.8)
              CALL PSETR('LEGEND_ENTRY_MAXIMUM_WIDTH',5.0)
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
C             CALL PSETC('TEXT_MODE','TITLE')
              CALL PSETC('TEXT_MODE','POSITIONAL')
              CALL PSETR('TEXT_BOX_Y_POSITION',SBPYP)
              CALL PSETR('TEXT_BOX_Y_LENGTH',SBPYL)
              CALL PSETR('TEXT_BOX_X_POSITION',SBPXP)
              CALL PSETR('TEXT_BOX_X_LENGTH',SBPXL)
              CALL PSETC('TEXT_COLOUR','BLACK')
              CALL PSETC('TEXT_JUSTIFICATION','LEFT')
              CALL PSETR('TEXT_REFERENCE_CHARACTER_HEIGHT',0.5)
              CALL PSETC('TEXT_QUALITY','HIGH')
              CALL PSETI('TEXT_LINE_COUNT',5)
              WRITE(CTITG1,'(A,A,A)')
     >           diagnostic(1:lena(diagnostic)),' for ',
     >           regionname(1:lena(regionname))
              if(ilsm(ir).eq.0)mask=' (land and sea points)'
              if(ilsm(ir).eq.1)mask=' (land points only)'
              if(ilsm(ir).eq.-1)mask=' (sea points only)'
              CTITG1=CTITG1(1:lena(CTITG1))//mask(1:lena(mask))
              ntest=0
              do i=1,7
                 if(pevt(ievt).eq.tevent(i))then
                   WRITE(CTITG2,'(a,a,a)')
     >                variable(1:lena(variable)),' anomalies ',
     >                nevent(i)(1:lena(nevent(i)))
                   ntest=1
                 endif
              enddo
              if(ntest.eq.0)WRITE(CTITG2,'(a,a)')
     >             variable(1:lena(variable)),' for ? event'
              WRITE(CTITG3,'(A,I4.4,A,I4.4,A,A,A,i2,A,i2)')
     >           'Hindcast period ',iyy1,'-',iyy2,
     >           ' with start in ',start(1:lena(start)),
     >           ' and averaging period ',nf1,' to ',nf2
              if(permeth.eq.0)then
                 WRITE(CTITG4,'(A)')
     >              'Threshold computed ranking the sample'
              else
                 WRITE(CTITG4,'(A)')
     >            'Threshold estimated with a kernel method for the PDF'
              endif
              write(*,*)CTITG1
              write(*,*)CTITG2
              write(*,*)CTITG3
              write(*,*)CTITG4
              CALL PSETC('TEXT_COLOUR','BLACK')
              CALL PSETR('TEXT_REFERENCE_CHARACTER_HEIGHT',0.5)
              CALL PSETC('TEXT_LINE_1',CTITG1(1:lena(CTITG1)))
              CALL PSETC('TEXT_LINE_2',CTITG2(1:lena(CTITG2)))
              CALL PSETC('TEXT_LINE_3',CTITG3(1:lena(CTITG3)))
              CALL PTEXT
              CALL PSETC('TEXT_LINE_1','')
              CALL PSETC('TEXT_LINE_2','')
              CALL PSETC('TEXT_LINE_3','')
              CALL PSETR('TEXT_REFERENCE_CHARACTER_HEIGHT',0.4)
              CALL PSETC('TEXT_LINE_4',CTITG4(1:lena(CTITG4)))
              if(boot.eq.1)then
                WRITE(CTITG5,'(a,i2.2,a,i5,a)')'Bars over AVER are ',
     >             int(topth-botth),'% conf. intervals computed with ',
     >             nsam,' samples'
                CALL PSETC('TEXT_LINE_5',CTITG5(1:lena(CTITG5)))
                write(*,*)CTITG5
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
              SBPYL=.85*PYL
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
              CALL PSETR('AXIS_TITLE_HEIGHT',0.20)
              allocate (ticklabel(nyear+3),                  stat=istat)
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
              CALL PSETC('AXIS_TITLE','OFF')
c             CALL PSETC('AXIS_TITLE_TEXT','Forecast range')
              CALL PSETR('AXIS_MIN_VALUE',0.)
              CALL PSETR('AXIS_MAX_VALUE',float(nyear+3))
              CALL PSETR('AXIS_TICK_INTERVAL',1.)
              CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',1)
              CALL PSETC('AXIS_TICK_LABEL_TYPE','LABEL_LIST')
              CALL PSETC('AXIS_TICK_LABEL_FIRST','OFF')
              CALL PSETC('AXIS_TICK_LABEL_LAST','ON')
              CALL PSET1C('AXIS_TICK_LABEL_LIST',ticklabel,nyear+3)
              CALL PAXIS
              deallocate (ticklabel,                         stat=istat)
c
              ymax=1.0
              ymin=-1.0
              yint=0.2
              if(dia.eq.'RES')then
                ymin=0.0
              endif
              if(dia.eq.'REL')then
                ymin=0.5
                yint=0.1
              endif
              if(dia.eq.'SHA')then
                ymax=0.03
                ymin=0.0
                yint=0.005
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
              CALL PSETC('GRAPH_TYPE','BAR')
              CALL PSETC('GRAPH_SHADE','ON')
              CALL PSETC('GRAPH_SHADE_STYLE','AREA_FILL')
              CALL PSETC('GRAPH_SHADE_COLOUR','RED')
              CALL PSETC('GRAPH_BAR_COLOUR','RED')
c
              allocate (x(nyear),                            stat=istat)
              allocate (y1(nyear),                           stat=istat)
              allocate (y2(nyear),                           stat=istat)
              allocate (score(nyear+1,nmod),                 stat=istat)
              do imod=1,nmod
                 do iy=1,nyear+1
                    if(dia.eq.'BSS')score(iy,imod)=bss_ful(iy,imod,0)
                    if(dia.eq.'BSI')score(iy,imod)=bss_inf(iy,imod,0)
                    if(dia.eq.'REL')score(iy,imod)=bss_rel(iy,imod,0)
                    if(dia.eq.'RES')score(iy,imod)=bss_res(iy,imod,0)
                    if(dia.eq.'SHA')score(iy,imod)=sd_prob(iy,imod,0)
                    if(dia.eq.'ROC')score(iy,imod)=roc(iy,imod,0)
                 enddo
              enddo
c
              if(nmod.eq.1)then
                shift=0.
                xinc=0.
              else
                shift=0.5
                xinc=0.60/nmod
              endif
              do imod=1,nmod
                 do iy=1,nyear
                    x(iy)=float(iy)-shift+imod*xinc
                    if(imod.eq.nmod)x(iy)=
     >                 float(iy)-shift+(nmod+1)*xinc
                    if(score(iy,imod).gt.0.)then
                      y1(iy)=0.
                      y2(iy)=score(iy,imod)
                    else
                      y1(iy)=score(iy,imod)
                      y2(iy)=0.
                    endif
c                   write(*,*)iy,imod,score(iy,imod),y1(iy),y2(iy)
                 enddo
c
c.... bars and legends
c
                 if(nmod.gt.1)then
                   CALL PSETC('GRAPH_SHADE_COLOUR',bcolour(imod))
                   CALL PSETC('GRAPH_BAR_COLOUR',bcolour(imod))
                   if(imod.eq.nmod)then
                     CALL PSETC('GRAPH_SHADE_COLOUR','RED')
                     CALL PSETC('GRAPH_BAR_COLOUR','RED')
                   endif
                   CALL PSETC('LEGEND','ON')
                   CALL PSETR('GRAPH_BAR_WIDTH',0.20)
                   if(typm(imod).eq.1)then
                     write(legend,'(a,a,i2,a)')expm(imod)(2:5),
     >                  ' (',nenm(imod),')'
                   else
                     write(legend,'(a,a,a,a,i2,a)')expm(imod)(5:8),'-',
     >                  expm(imod)(10:13),' (',nenm(imod),')'
                   endif
                 else
                   CALL PSETC('GRAPH_SHADE_COLOUR','RED')
                   CALL PSETC('GRAPH_BAR_COLOUR','RED')
                   CALL PSETR('GRAPH_BAR_WIDTH',0.15)
                   CALL PSETC('LEGEND','ON')
                   if(typm(1).eq.1)then
                     write(legend,'(a,a,i2,a)')expm(1)(2:5),
     >                  ' (',nenm(1),')'
                   else
                     write(legend,'(a,a,a,a,i2,a)')expm(1)(5:8),'-',
     >                  expm(1)(10:13),' (',nenm(1),')'
                   endif
                 endif
                 write(*,*)legend
                 CALL PSETC('LEGEND_USER_TEXT',legend(1:lena(legend)))
                 CALL PSET1R('GRAPH_BAR_X_VALUES',x,nyear)
                 CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',y1,nyear)
                 CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',y2,nyear)
                 CALL PGRAPH
                 CALL PSETC('LEGEND','OFF')
              enddo
              deallocate (x,                                 stat=istat)
              deallocate (y1,                                stat=istat)
              deallocate (y2,                                stat=istat)
c
c.... confidence intervals
c
              allocate (x(1),                                stat=istat)
              allocate (y1(1),                               stat=istat)
              allocate (y2(1),                               stat=istat)
              if(boot.eq.1)then
                imarker(1)=28
                CALL PSETC('SYMBOL_POSITION_MODE','GRAPH')
                CALL PSETC('SYMBOL_TYPE','MARKER')
                CALL PSET1I('SYMBOL_INPUT_MARKER_LIST',imarker,1)
                CALL PSETC('SYMBOL_COLOUR','BLACK')
                CALL PSETR('SYMBOL_HEIGHT',0.25)
                CALL PSETC('LEGEND','OFF')
                do imod=1,nmod
                   iy=nyear+1
                   do ib=1,nsam
                      if(dia.eq.'BSS')xarray(ib)=bss_ful(iy,imod,ib)
                      if(dia.eq.'BSI')xarray(ib)=bss_inf(iy,imod,ib)
                      if(dia.eq.'REL')xarray(ib)=bss_rel(iy,imod,ib)
                      if(dia.eq.'RES')xarray(ib)=bss_res(iy,imod,ib)
                      if(dia.eq.'SHA')xarray(ib)=sd_prob(iy,imod,ib)
                      if(dia.eq.'ROC')xarray(ib)=roc(iy,imod,ib)
                   enddo
                   call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                   write(*,'(a,f6.3,a,f6.3,a)')'(',x_bot,',',x_top,')'
                   CALL PSETC('GRAPH_SHADE_COLOUR',bcolour(imod))
                   CALL PSETC('GRAPH_BAR_COLOUR',bcolour(imod))
                   CALL PSETR('GRAPH_BAR_WIDTH',0.10)
                   if(((imod.eq.nmod).and.(mult.eq.1)).or.
     >                (imod.eq.1))then
                     CALL PSETR('GRAPH_BAR_WIDTH',0.15)
                     CALL PSETC('GRAPH_SHADE_COLOUR','RED')
                     CALL PSETC('GRAPH_BAR_COLOUR','RED')
                   endif
                   x(1)=nyear+2-shift+imod*xinc
                   if(imod.eq.nmod)x(1)=nyear+2-shift+(nmod+1)*xinc
                   y1(1)=x_bot
                   y2(1)=x_top
                   CALL PSET1R('GRAPH_BAR_X_VALUES',x,1)
                   CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',y1,1)
                   CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',y2,1)
                   CALL PGRAPH
                   y1(1)=score(nyear+1,imod)
                   CALL PSET1R('SYMBOL_INPUT_X_POSITION',x,1)
                   CALL PSET1R('SYMBOL_INPUT_Y_POSITION',y1,1)
                   CALL PSYMB
                enddo
              else
                do imod=1,nmod
                   x(1)=nyear+2-shift+imod*xinc
                   if(imod.eq.nmod)x(1)=nyear+2-shift+(nmod+1)*xinc
                   if(score(nyear+1,imod).gt.0.)then
                     y1(1)=0.
                     y2(1)=score(nyear+1,imod)
                   else
                     y1(1)=score(nyear+1,imod)
                     y2(1)=0.
                   endif
                   CALL PSET1R('GRAPH_BAR_X_VALUES',x,1)
                   CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',y1,1)
                   CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',y2,1)
                   CALL PGRAPH
                enddo
              endif
              deallocate (x,                                 stat=istat)
              deallocate (y1,                                stat=istat)
              deallocate (y2,                                stat=istat)
c
c.... reference line
c
              allocate (x(0:nyear+3),                        stat=istat)
              allocate (y1(0:nyear+3),                       stat=istat)
              CALL PSETC('LEGEND','OFF')
              do iy=0,nyear+3
                 x(iy)=float(iy)
                 y1(iy)=0.
                 if(dia.eq.'SPR')y1(iy)=1.
              enddo
              CALL PSETC('GRAPH_TYPE','CURVE')
              CALL PSETC('GRAPH_LINE_COLOUR','BLACK')
              CALL PSET1R('GRAPH_CURVE_X_VALUES',x,nyear+4)
              CALL PSET1R('GRAPH_CURVE_Y_VALUES',y1,nyear+4)
              CALL PGRAPH
              deallocate (x,                                 stat=istat)
              deallocate (y1,                                stat=istat)
              deallocate (score,                             stat=istat)
c
              CALL PCLOSE
           enddo !ievt loop
         endif !rcal=1
      enddo !ir loop
      deallocate (bss_ful,                                   stat=istat)
      deallocate (bss_red,                                   stat=istat)
      deallocate (bss_inf,                                   stat=istat)
      deallocate (rel,                                       stat=istat)
      deallocate (bss_rel,                                   stat=istat)
      deallocate (res,                                       stat=istat)
      deallocate (bss_res,                                   stat=istat)
      deallocate (unc,                                       stat=istat)
      deallocate (totocc_rel,                                stat=istat)
      deallocate (ave_prob,                                  stat=istat)
      deallocate (sd_prob,                                   stat=istat)
      deallocate (roc,                                       stat=istat)
      deallocate (rocsup,                                    stat=istat)
      deallocate (roclow,                                    stat=istat)
      deallocate (xarray,                                    stat=istat)
c
      goto 999
 998  write(*,*)'Sorry, no success :('
      call abort
 999  write(*,*)'program seems to be successfully finished :)'
c
      end
