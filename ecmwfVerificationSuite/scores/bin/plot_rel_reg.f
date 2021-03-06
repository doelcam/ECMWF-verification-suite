c
c----------------------------------------------------------------------c
c     PROGRAM plot_rel_reg                P. Doblas-Reyes  4-Jul-2006  c
c                                                                      c
c     PURPOSE:                                                         c
c     Plots the reliability diagrams along with other scores           c
c     for binary probabilistic forecasts for defined regions           c
c     Includes confidence intervals for the scores                     c
c                                                                      c
c     INPUT:                                                           c
c     BRV_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE_NAMR           c
c                                                          ASCII file  c
c                                                                      c
c     OUTPUT:                                                          c
c     REL_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE_NAMR.ps        c
c                                                             ps file  c
c                                                                      c
c     USAGE:                                                           c
c     plot_rel_reg.x < nlist                                           c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 plot_rel_reg.f tools.f tools_plot.f                c
c     -o plot_rel_reg.x $MAGLIB $EMOSLIB $NAGLIB                       c
c                                                                      c
c     MODS:                                                            c
c                                         F. Doblas-Reyes, 04-Sep-2006 c
c     Based on the original plot_rel.f in                              c
c     /home/rd/nep/DEMETER/reliability                                 c
c                                                                      c
c----------------------------------------------------------------------c
c
      PROGRAM plot_rel_reg
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: obs_occ(:,:), xprob(:,:)
      real, allocatable :: sample(:,:), yprob(:,:)
      real, allocatable :: bss_ful(:), bss_red(:), bss_inf(:)
      real, allocatable :: rel(:), bss_rel(:)
      real, allocatable :: res(:), bss_res(:), unc(:), totocc_rel(:)
      real, allocatable :: ave_prob(:), sd_prob(:)
      real, allocatable :: roc(:), rocsup(:), roclow(:)
      real, allocatable :: ntot(:), xarray(:)
      real, allocatable :: s(:), d(:)
      character*20, allocatable :: clist(:)
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
      real eps
      parameter(eps=0.1)
c
c.... other definitions
c
      real x(2), y(2), z(1), w(1), v(1)
      integer imarker(1)
      integer tevent(7)
      character*30 nevent(7)
      data tevent/50,33,67,25,75,20,80/
      data nevent/'above the median',
     >   'below the lower tercile','above the upper tercile',
     >   'below the lower quartile','above the upper quartile',
     >   'below the lower quintile','above the upper quintile'/
c
c.... other definitions
c
      integer nx, ny, nens
      integer iyy1, iyy2, imm, idd, itt
      integer cros, ipar, ilev, anin,  nf1,  nf2
      integer nreg, nevt, boot, nsam, iprb, npro, permeth
      integer i, nyear, nbin, istat, ievt, iy
      integer nprob, ir, lena, iprob, ibin
      integer ntop, nbot, nsamin, ib
      integer exptl, nint
      integer ntest
      real topth, botth
      real x_top, x_bot
      real xmax, xmin
      real spxl, spyl, sbpxl, sbpyl, sbpxp, sbpyp, pxl, pyl
      real obsocc_rel
      real ymax, ymin, yint, h
      character*110 yifile, yrfile, yofile
      character*3 dia
      character*120 ctitg0, ctitg1, ctitg2, ctitg3
      character*120 ctitg4, ctitg5, ctitg6, ctitg7
      character*120 ctitg8
      character*30 mask
      character*20 start
      character*40 variable, regionname
      character*50 diagnostic
      character*30 form1
      character*21 expt
c
c.... namelist
c
      namelist /control/   nx,   ny, nens,
     >                   iyy1, iyy2,  imm,  idd,  itt,
     >                   cros, ipar, ilev, anin,  nf1,  nf2,
     >                   expt, nevt, pevt, permeth,
     >                   boot, nsam, iprb, npro,
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
c   nens:   number of ensemble members
c   iyy1:   start year
c   iyy2:   end year
c   imm:    forecast starting month
c   expt:   model id
c
      read(5,control)
      write(*,*)'  nx: ',nx
      write(*,*)'  ny: ',ny
      write(*,*)'nens: ',nens
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
      write(*,*)'expt: ',expt
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
c
c.... iprb controls the way the probabilities are binned
c.... if 1, the number of probability bins is the ensemble size plus 1
c.... if not 1, the number of bins is a parameter
c.... nbin is used in the potential economic value
c
      nprob=npro
      if(iprb.eq.1)nprob=nens+1
      nbin=nens+1
      write(*,*)'Parameters'
      write(*,'(3i3.3)')iprb,nprob,nbin
c
      allocate (d(nprob),                                    stat=istat)
      allocate (s(nprob),                                    stat=istat)
c
c.... loop over all regions
c
      do ir=1,nreg
         if(rcal(ir).eq.1)then
c
c.... looking for the title
c
           nint=13
           dia='BRV'
           allocate (clist(nint),                            stat=istat)
           allocate (llist(nint+1),                          stat=istat)
           call titles(imm, ipar, ilev, namr(ir), dia,
     >        start, variable, regionname,
     >        diagnostic, nint, clist, llist, xmax, xmin)
           write(*,*)'Reliability diagram for ',regionname
c
c.... allocate fields
c
           allocate (obs_occ(0:nsam,nprob),                  stat=istat)
           allocate (xprob(0:nsam,nprob),                    stat=istat)
           allocate (sample(0:nsam,nprob),                   stat=istat)
           allocate (yprob(0:nsam,nprob),                    stat=istat)
           allocate (bss_ful(0:nsam),                        stat=istat)
           allocate (bss_red(0:nsam),                        stat=istat)
           allocate (bss_inf(0:nsam),                        stat=istat)
           allocate (rel(0:nsam),                            stat=istat)
           allocate (bss_rel(0:nsam),                        stat=istat)
           allocate (res(0:nsam),                            stat=istat)
           allocate (bss_res(0:nsam),                        stat=istat)
           allocate (unc(0:nsam),                            stat=istat)
           allocate (totocc_rel(0:nsam),                     stat=istat)
           allocate (ave_prob(0:nsam),                       stat=istat)
           allocate (sd_prob(0:nsam),                        stat=istat)
           allocate (roc(0:nsam),                            stat=istat)
           allocate (rocsup(0:nsam),                         stat=istat)
           allocate (roclow(0:nsam),                         stat=istat)
           allocate (ntot(0:nsam),                           stat=istat)
           allocate (xarray(nsam),                           stat=istat)
c
c.... loop over the events
c
           do ievt=1,nevt
c
c.... open input file
c
              exptl=lena(expt)
              write(form1,'(a,i2.2,a)')'(a',exptl,')'
              yifile=
     >'BRV_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_THR_MM_BIN_AAA-EEE'
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
              if(boot.eq.1)then
                yrfile=yifile
                write(yrfile(62+exptl:63+exptl),'(a2)')'_S'
                write(yrfile(64+exptl:68+exptl),'(i5.5)')nsam
                yofile='psfiles/'//yrfile(1:lena(yrfile))//'.ps'
                yrfile=yrfile(1:lena(yrfile))//'.ran'
                write(*,*)'Open input file ',yrfile(1:lena(yrfile))
                open(11,file=yrfile,form='formatted')
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
                yofile='psfiles/'//yifile(1:lena(yifile))//'.ps'
              endif
              write(*,*)'Output file ',yofile(1:lena(yofile)),
     >        ' is a postscript file'
C
C     INITIALISATION DE MAGICS
C
              CALL POPEN
              CALL PSETC('PS_DEVICE','ps_a4')
              CALL PSETC('PS_FILE_NAME',yofile)
C             CALL PSETC('WORKSTATION_1','PS_COL_A4_VERTICAL')
C             CALL PSETC('WORKSTATION_1','PS_COL_A4_HORIZONTAL')
C
C     PLOTTING SECTION
C
              CALL PSETR('AXIS_TICK_SIZE',0.1)
              CALL PSETR('AXIS_MINOR_TICK_SIZE',0.05)
              CALL PSETR('AXIS_MINOR_TICK_MIN_GAP',0.05)
              CALL PSETC('AXIS_GRID_LINE_STYLE','DOT')
              CALL PSETC('AXIS_GRID_COLOUR','BLACK')
C
              CALL PSETC('LAYOUT','POSITIONAL')
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
              CALL PSETC('LEGEND_BORDER','ON')
              CALL PSETC('LEGEND_TEXT_COLOUR','BLACK')
              CALL PSETC('LEGEND_TEXT_QUALITY','HIGH')
c
c.... reading data
c.... skip the line with the lead time
c
              read(10,*)
c
c.... then skip the results for individual years
c
              do iy=1,nyear
                 read(10,*)
              enddo
c
c.... reliability and Brier score
c
              ntot(0)=0.
              do iprob=1,nprob
                 read(10,'(8x,2f13.2,2f12.5)')
     >              sample(0,iprob),obs_occ(0,iprob),
     >              xprob(0,iprob),yprob(0,iprob)
                    ntot(0)=ntot(0)+sample(0,iprob)
              enddo
              read(10,'(11f10.4)')bss_ful(0),bss_red(0),bss_inf(0),
     >           rel(0),bss_rel(0),
     >           res(0),bss_res(0),unc(0),totocc_rel(0),
     >           ave_prob(0),sd_prob(0)
              write(*,*)'ntot=',ntot(0)
c
c.... ROC skill score
c
              read(10,'(3f10.4)')roc(0),rocsup(0),roclow(0)
c
c.... results from bootstrap
c
              if(boot.eq.1)then
                do ib=1,nsam
                   ntot(ib)=0.
                   do iprob=1,nprob
                      read(11,'(8x,2f13.2,2f12.5)')
     >                   sample(ib,iprob),obs_occ(ib,iprob),
     >                   xprob(ib,iprob),yprob(ib,iprob)
                      ntot(ib)=ntot(ib)+sample(ib,iprob)
                   enddo
                   read(11,'(11f10.4)')bss_ful(ib),bss_red(ib),
     >                bss_inf(ib),
     >                rel(ib),bss_rel(ib),
     >                res(ib),bss_res(ib),unc(ib),totocc_rel(ib),
     >                ave_prob(ib),sd_prob(ib)
                   read(11,'(3f10.4)')roc(ib),rocsup(ib),roclow(ib)
c
c.... skip HR/FR and ROC with nbin
c
                   do ibin=1,nbin
                      read(11,*)
                   enddo
                   read(11,*)
                enddo !ib loop
              endif
c
c.... general text
c
              PXL=SPXL-eps
c              PYL=SPYL/5-eps
               PYL=SPYL/5-eps
c              SBPXL=.95*PXL
              SBPXL=.95*PXL
              SBPXP=(PXL-SBPXL)/2
c              SBPYL=.95*PYL
              SBPYL=.95*PYL
              SBPYP=(PYL-SBPYL)/2.
              CALL PSETR('PAGE_X_LENGTH',PXL)
              CALL PSETR('PAGE_Y_LENGTH',PYL)
              CALL PSETR('PAGE_X_POSITION',0.)
c              CALL PSETR('PAGE_Y_POSITION',4*SPYL/5)
              CALL PSETR('PAGE_Y_POSITION',5*SPYL/6)
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
              CALL PSETC('TEXT_QUALITY','HIGH')
              CALL PSETI('TEXT_LINE_COUNT',4)
              WRITE(CTITG0,'(A,A,A,i2,A,A)')
     >           'Reliability diagram for ',expt(1:lena(expt)),
     >           ' with ',nens,' ensemble members'
              write(*,*)CTITG0
              ntest=0
              do i=1,7
                 if(pevt(ievt).eq.tevent(i))then
                   WRITE(CTITG1,'(a,a,a,a,a)')
     >                variable(1:lena(variable)),' anomalies ',
     >                nevent(i)(1:lena(nevent(i))),' over ',
     >                regionname(1:lena(regionname))
                   ntest=1
                 endif
              enddo
              if(ntest.eq.0)then
                WRITE(CTITG1,'(a,a,a)')
     >             variable(1:lena(variable)),' for ? event over ',
     >             regionname(1:lena(regionname))
              endif
              if(ilsm(ir).eq.0)mask=' (land and sea points)'
              if(ilsm(ir).eq.1)mask=' (land points only)'
              if(ilsm(ir).eq.-1)mask=' (sea points only)'
              CALL PSETR('TEXT_FONT_SIZE',0.55)
              CTITG1=CTITG1(1:lena(CTITG1))//mask(1:lena(mask))
              write(*,*)CTITG1
              WRITE(CTITG2,'(A,I4.4,A,I4.4,A,A,A,i2,A,i2)')  
     >           'Hindcast period ',iyy1,'-',iyy2,
     >           ' with start in ',start(1:lena(start)),
     >           ' and averaging period ',nf1,' to ',nf2
              write(*,*)CTITG2
              if(permeth.eq.0)then
                 WRITE(CTITG3,'(A)')
     >              '<font size="0.5">Threshold computed' 
     >              //'ranking the sample</font>'
              else
                 WRITE(CTITG3,'(A)')
     >             '<font size="0.5">Threshold estimated with'
     >             //' a kernel method for the PDF</font>'
              endif

              write(*,*)CTITG3
              CALL PSETC('TEXT_LINE_1',CTITG0(1:lena(CTITG0)))
              CALL PSETC('TEXT_LINE_2',CTITG1(1:lena(CTITG1)))
              CALL PSETC('TEXT_LINE_3',CTITG2(1:lena(CTITG2)))
              CALL PSETR('TEXT_FONT_SIZE',0.55)
c              CALL PTEXT
              CALL PSETC('TEXT_LINE_4',CTITG3(1:lena(CTITG3)))
              CALL PSETR('TEXT_FONT_SIZE',0.55)
              CALL PTEXT
              CALL PNEW('PAGE')
c
c.... plotting the reliability diagram
c
              PXL=3*SPXL/5-eps
              PYL=4*SPYL/5-eps
              SBPXL=.75*PXL
              SBPXP=(PXL-SBPXL)/2
              SBPYL=.75*PYL
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
C
              YMAX=1.0
              YMIN=0.0
              YINT=0.2
c
              CALL PSETR('AXIS_TITLE_HEIGHT',0.6)
              CALL PSETR('AXIS_TICK_LABEL_HEIGHT',0.50)
              CALL PSETC('AXIS_TICK_LABEL_FORMAT','F3.1')
C
              CALL PSETC('AXIS_TYPE','REGULAR')
              CALL PSETC('AXIS_ORIENTATION','VERTICAL')
              CALL PSETC('AXIS_POSITION','LEFT')
              CALL PSETR('AXIS_MAX_VALUE',YMAX)
              CALL PSETR('AXIS_MIN_VALUE',YMIN)
              CALL PSETR('AXIS_TICK_INTERVAL',YINT)
              CALL PSETC('AXIS_TICK','ON')
              CALL PSETC('AXIS_TICK_LABEL','ON')
              CALL PSETC('AXIS_MINOR_TICK','OFF')
              CALL PSETC('AXIS_TITLE','ON')
              CALL PSETC('AXIS_TITLE_TEXT','Observed frequency')
              CALL PSETC('AXIS_GRID','OFF')
              CALL PAXIS
C
              CALL PSETC('AXIS_TYPE','REGULAR')
              CALL PSETC('AXIS_ORIENTATION','HORIZONTAL')
              CALL PSETC('AXIS_POSITION','BOTTOM')
              CALL PSETR('AXIS_MAX_VALUE',YMAX)
              CALL PSETR('AXIS_MIN_VALUE',YMIN)
              CALL PSETR('AXIS_TICK_INTERVAL',YINT)
              CALL PSETC('AXIS_TICK','ON')
              CALL PSETC('AXIS_TICK_LABEL','ON')
              CALL PSETC('AXIS_MINOR_TICK','OFF')
              CALL PSETC('AXIS_TITLE','ON')
              CALL PSETC('AXIS_TITLE_TEXT','Forecast probability')
              CALL PSETC('AXIS_GRID','OFF')
              CALL PAXIS
C
              write(*,*)'Call to plot results'
c
c.... diagonal
c
              CALL PSETC('GRAPH_TYPE','CURVE')
              CALL PSETC('GRAPH_LINE_COLOUR','BLACK')
              CALL PSETC('GRAPH_LINE_STYLE','SOLID')
              CALL PSETI('GRAPH_LINE_THICKNESS',5)
              CALL PSETR('GRAPH_Y_SUPPRESS_BELOW',0.)
              x(1)=0.001
              y(1)=0.001
              x(2)=1.001
              y(2)=1.001
              CALL PSET1R('GRAPH_CURVE_Y_VALUES',y,2)
              CALL PSET1R('GRAPH_CURVE_X_VALUES',x,2)
              write(*,*)'Plotting diagonal'
              CALL PGRAPH
c
c.... reliability=resolution line
c
              CALL PSETC('GRAPH_LINE_STYLE','DASH')
              CALL PSETI('GRAPH_LINE_THICKNESS',2)
              x(1)=0.
              y(1)=totocc_rel(0)/2
              x(2)=1.
              y(2)=totocc_rel(0)+(1.-totocc_rel(0))/2
              CALL PSET1R('GRAPH_CURVE_Y_VALUES',y,2)
              CALL PSET1R('GRAPH_CURVE_X_VALUES',x,2)
              write(*,*)'Plotting rel=res line'
              CALL PGRAPH
c
c.... average probability and climatological frequency
c
              CALL PSETC('GRAPH_LINE_COLOUR','BLUE')
              CALL PSETC('GRAPH_LINE_STYLE','SOLID')
              CALL PSETI('GRAPH_LINE_THICKNESS',5)
              x(1)=0.
              y(1)=totocc_rel(0)
               x(2)=1.
              y(2)=totocc_rel(0)
              CALL PSET1R('GRAPH_CURVE_Y_VALUES',y,2)
              CALL PSET1R('GRAPH_CURVE_X_VALUES',x,2)
              write(*,*)'Plotting climatology'
              CALL PGRAPH
              y(1)=0.001
              x(1)=ave_prob(0)
              y(2)=0.9999
              x(2)=ave_prob(0)
              CALL PSET1R('GRAPH_CURVE_Y_VALUES',y,2)
              CALL PSET1R('GRAPH_CURVE_X_VALUES',x,2)
              write(*,*)'Plotting average probability'
              CALL PGRAPH
c
c.... reliability curve
c
              CALL PSETC('GRAPH_LINE_COLOUR','RED')
              CALL PSETC('GRAPH_LINE_STYLE','SOLID')
              CALL PSETI('GRAPH_LINE_THICKNESS',5)
              do iprob=1,nprob-1
                 x(1)=xprob(0,iprob)
                 y(1)=yprob(0,iprob)
                 x(2)=xprob(0,iprob+1)
                 y(2)=yprob(0,iprob+1)
                 if((y(1).eq.0.).or.(y(2).eq.0.))then
                   write(*,*)'Bin with no observations'
                 else
                   CALL PSET1R('GRAPH_CURVE_X_VALUES',x,2)
                   CALL PSET1R('GRAPH_CURVE_Y_VALUES',y,2)
c                   CALL PSET1R('GRAPH_CURVE_X_VALUES',x,nprob)
c                   CALL PSET1R('GRAPH_CURVE_Y_VALUES',y,nprob)
                   CALL PGRAPH
                 endif
              enddo
c
c.... bars for confidence intervals of the reliability curve
c
              if(boot.eq.1)then
                CALL PSETC('GRAPH_TYPE','BAR')
                CALL PSETC('GRAPH_SHADE','ON')
                CALL PSETC('GRAPH_SHADE_STYLE','AREA_FILL')
                CALL PSETC('GRAPH_SHADE_COLOUR','GREY')
                CALL PSETC('GRAPH_BAR_COLOUR','GREY')
                CALL PSETC('GRAPH_BAR_LINE_COLOUR','GREY')
c                CALL PSETR('GRAPH_BAR_WIDTH',0.01)
                 CALL PSETR('GRAPH_BAR_WIDTH',0.007)
                do iprob=1,nprob
                   do ib=1,nsam
                      xarray(ib)=xprob(ib,iprob)
                   enddo
                   call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                   write(*,'(i3,3f12.2)')
     >                iprob,xprob(0,iprob),x_bot,x_top
                   do ib=1,nsam
                      xarray(ib)=yprob(ib,iprob)
                   enddo
                   call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                   z(1)=xprob(0,iprob)
                   w(1)=x_bot
                   v(1)=x_top
                   CALL PSET1R('GRAPH_BAR_X_VALUES',z,1)
                   CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',w,1)
                   CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',v,1)
                   CALL PGRAPH
                enddo
              endif
c
c.... symbols
c
              CALL PSETC('SYMBOL_TYPE','MARKER')
              CALL PSETI('SYMBOL_MARKER_INDEX',15)
              CALL PSETC('SYMBOL_COLOUR','RED')
              do iprob=1,nprob
                 z(1)=xprob(0,iprob)
                 w(1)=yprob(0,iprob)
c                write(*,*)z(1),w(1)
                 h=2*sqrt(sample(0,iprob)/ntot(0))
                 if(h.lt.0.05)h=0.05
                 if(h.gt.5.)h=5.0
c                write(*,*)'Symbol height ',h
                 CALL PSETR('SYMBOL_HEIGHT',h*2.)
                 CALL PSET1R('SYMBOL_INPUT_X_POSITION',z,1)
                 CALL PSET1R('SYMBOL_INPUT_Y_POSITION',w,1)
                 CALL PSETC('SYMBOL_COLOUR','RED')
                 CALL PSYMB
                 CALL PSETC('SYMBOL_COLOUR','BLACK')
c                 CALL PSETR('SYMBOL_HEIGHT',0.40)
                 CALL PSETR('SYMBOL_HEIGHT',0.20)
                 CALL PSYMB
              enddo
              CALL PNEW('PAGE')
c
c.... reliability and resolution terms
c
              PXL=2*SPXL/5-eps
              PYL=3*SPYL/5-eps
              SBPXL=.70*PXL
              SBPXP=(PXL-SBPXL)/2
              SBPYL=.70*PYL
              SBPYP=(PYL-SBPYL)/2.
              CALL PSETR('PAGE_X_LENGTH',PXL)
              CALL PSETR('PAGE_Y_LENGTH',PYL)
              CALL PSETR('PAGE_X_POSITION',3*SPXL/5)
              CALL PSETR('PAGE_Y_POSITION',0.)
              CALL PSETR('SUBPAGE_Y_POSITION',SBPYP)
              CALL PSETR('SUBPAGE_Y_LENGTH',SBPYL)
              CALL PSETR('SUBPAGE_X_POSITION',SBPXP)
              CALL PSETR('SUBPAGE_X_LENGTH',SBPXL)
              CALL PSETC('SUBPAGE_FRAME','ON')
c              CALL PSETR('LEGEND_TEXT_MAXIMUM_HEIGHT',0.25)
              CALL PSETR('legend_text_font_size',0.4)
              call psetc("legend_box_mode", "positional")
c             call psetr("legend_box_x_position", SBPXP)
              call psetr("legend_box_x_position", SBPXP*0.95)
c              call psetr("legend_box_x_length", SBPXL*0.66)
              call psetr("legend_box_x_length", SBPXL*0.88)
c              call psetr("legend_box_y_position", SBPYL*1.2)
              call psetr("legend_box_y_position", SBPYP*5.65)
c              call psetr("legend_box_y_length", 1.)
              call psetr("legend_box_y_length",SBPYL*0.1 )
              call psetc("legend_border", "off")

c
              YMAX=1.0
              YMIN=0.0001
              YINT=0.2
c
              CALL PSETC('AXIS_ORIENTATION','VERTICAL')
              CALL PSETR('AXIS_TITLE_HEIGHT',0.5)
              CALL PSETR('AXIS_TICK_LABEL_HEIGHT',0.4)
              CALL PSETC('AXIS_TICK_LABEL_FORMAT','F6.4')
C
              CALL PSETC('AXIS_TYPE','REGULAR')
              CALL PSETC('AXIS_TYPE','LOGARITHMIC')
              CALL PSETC('AXIS_ORIENTATION','VERTICAL')
              CALL PSETC('AXIS_POSITION','LEFT')
              CALL PSETR('AXIS_MAX_VALUE',YMAX)
              CALL PSETR('AXIS_MIN_VALUE',YMIN)
c             CALL PSETR('AXIS_TICK_INTERVAL',YINT)
              CALL PSETC('AXIS_TICK','ON')
              CALL PSETC('AXIS_TICK_LABEL','ON')
              CALL PSETC('AXIS_MINOR_TICK','OFF')
              CALL PSETC('AXIS_TITLE','ON')
              CALL PSETC('AXIS_TITLE_TEXT',
     >           'Sharp/Rel/Res')
              CALL PSETC('AXIS_GRID','OFF')
              CALL PAXIS
C
              CALL PSETC('AXIS_ORIENTATION','HORIZONTAL')
              CALL PSETC('AXIS_TYPE','REGULAR')
              CALL PSETC('AXIS_POSITION','BOTTOM')
              CALL PSETR('AXIS_MAX_VALUE',YMAX)
              CALL PSETR('AXIS_MIN_VALUE',YMIN)
              CALL PSETR('AXIS_TICK_INTERVAL',YINT)
              CALL PSETC('AXIS_TICK_LABEL_FORMAT','F3.1')
              CALL PSETC('AXIS_TICK','ON')
              CALL PSETC('AXIS_TICK_LABEL','ON')
              CALL PSETC('AXIS_MINOR_TICK','OFF')
              CALL PSETC('AXIS_TITLE','ON')
              CALL PSETC('AXIS_TITLE_TEXT','Forecast probability')
              CALL PSETC('AXIS_GRID','OFF')
              CALL PAXIS
c
              CALL PSETC('GRAPH_TYPE','CURVE')
              CALL PSETC('GRAPH_LINE_COLOUR','BLUE')
              CALL PSETC('GRAPH_LINE_STYLE','SOLID')
              CALL PSETI('GRAPH_LINE_THICKNESS',10)
              y(1)=ymin
              x(1)=ave_prob(0)
              y(2)=1.
              x(2)=ave_prob(0)
              CALL PSET1R('GRAPH_CURVE_Y_VALUES',y,2)
              CALL PSET1R('GRAPH_CURVE_X_VALUES',x,2)
              CALL PGRAPH
c
              write(*,*)'Sharpness'
              CALL PSETC('GRAPH_SYMBOL','ON')
              CALL PSETI('GRAPH_SYMBOL_MARKER_INDEX',15)
              CALL PSETR('GRAPH_SYMBOL_HEIGHT',0.4)
              CALL PSETR('GRAPH_Y_SUPPRESS_BELOW',YMIN)
              do iprob=1,nprob
                 s(iprob)=sample(0,iprob)/ntot(0)
                 if(s(iprob).eq.0.)s(iprob)=ymin/10.
                 d(iprob)=xprob(0,iprob)
                 write(*,'(2f10.5)')d(iprob),s(iprob)
              enddo
              CALL PSETC('GRAPH_LINE_COLOUR','RED')
              CALL PSETC('GRAPH_SYMBOL_COLOUR','RED')
              CALL PSETC('GRAPH_LINE_STYLE','SOLID')
              CALL PSETI('GRAPH_LINE_THICKNESS',5)
              CALL PSET1R('GRAPH_CURVE_X_VALUES',d,nprob)
              CALL PSET1R('GRAPH_CURVE_Y_VALUES',s,nprob)
              CALL PSETC('LEGEND','ON')
              CALL PSETC('LEGEND_USER_TEXT','Sharp')
              CALL PGRAPH
              write(*,*)'Reliability'
              do iprob=1,nprob
                 if(sample(0,iprob).ne.0.)then
                   obsocc_rel=obs_occ(0,iprob)/sample(0,iprob)
                 else
                   obsocc_rel=0.
                 endif
                 s(iprob)=(xprob(0,iprob)-obsocc_rel)**2
                 d(iprob)=xprob(0,iprob)
                 write(*,'(2f10.5)')d(iprob),s(iprob)
              enddo
              CALL PSET1R('GRAPH_CURVE_X_VALUES',d,nprob)
              CALL PSET1R('GRAPH_CURVE_Y_VALUES',s,nprob)
              CALL PSETC('GRAPH_LINE_COLOUR','GREY')
              CALL PSETC('GRAPH_SYMBOL_COLOUR','GREY')
              CALL PSETC('LEGEND','ON')
              CALL PSETC('LEGEND_USER_TEXT','Rel')
              CALL PSETR('GRAPH_Y_SUPPRESS_BELOW',YMIN)
              CALL PGRAPH
              write(*,*)'Resolution'
              do iprob=1,nprob
                 if(sample(0,iprob).ne.0.)then
                   obsocc_rel=obs_occ(0,iprob)/sample(0,iprob)
                 else
                   obsocc_rel=0.
                 endif
                 s(iprob)=(totocc_rel(0)-obsocc_rel)**2
                 d(iprob)=xprob(0,iprob)
                 write(*,'(2f10.5)')d(iprob),s(iprob)
              enddo
              CALL PSET1R('GRAPH_CURVE_X_VALUES',d,nprob)
              CALL PSET1R('GRAPH_CURVE_Y_VALUES',s,nprob)
              CALL PSETC('GRAPH_LINE_COLOUR','GREEN')
              CALL PSETC('GRAPH_SYMBOL_COLOUR','GREEN')
              CALL PSETC('LEGEND','ON')
              CALL PSETC('LEGEND_USER_TEXT','Res')
              CALL PSETR('GRAPH_Y_SUPPRESS_BELOW',YMIN)
              CALL PGRAPH
c
              CALL PNEW('PAGE')
c   
              call psetr("legend_box_x_position", SBPXP+(SBPXL*0.8))
c              call psetr("legend_box_x_length", SBPXL*0.30)
c               call psetr("legend_box_x_position", SBPXP+(SBPXL*0.8))
               call psetr("legend_box_x_length", SBPXL*0.25)

              YMAX=0.025
              YMIN=-0.025
              YINT=0.005
              CALL PAXIS 
              CALL PSETC('AXIS_ORIENTATION','VERTICAL')
              CALL PSETC('AXIS_TYPE','REGULAR')
              CALL PSETC('AXIS_POSITION','RIGHT')
              CALL PSETR('AXIS_MAX_VALUE',YMAX)
              CALL PSETR('AXIS_MIN_VALUE',YMIN)
              CALL PSETR('AXIS_TICK_INTERVAL',YINT)
              CALL PSETC('AXIS_TICK_LABEL_FORMAT','F6.3')
              CALL PSETC('AXIS_TICK','ON')
              CALL PSETC('AXIS_TICK_LABEL','ON')
              CALL PSETC('AXIS_MINOR_TICK','OFF')
              CALL PSETC('AXIS_TITLE','ON')
              CALL PSETC('AXIS_TITLE_TEXT',
     >           'Brier score')
              CALL PSETC('AXIS_GRID','OFF')
              CALL PAXIS
c
              write(*,*)'Brier score'
              do iprob=1,nprob
                 if(sample(0,iprob).ne.0.)then
                   obsocc_rel=obs_occ(0,iprob)/sample(0,iprob)
                 else
                   obsocc_rel=0.
                 endif
                 s(iprob)=(sample(0,iprob)/ntot(0))*
     >              ((xprob(0,iprob)-obsocc_rel)**2-
     >              (totocc_rel(0)-obsocc_rel)**2)
                 d(iprob)=xprob(0,iprob)
                 write(*,'(2f10.5)')d(iprob),s(iprob)
              enddo
              CALL PSETR('GRAPH_Y_SUPPRESS_BELOW',YMIN)
              CALL PSET1R('GRAPH_CURVE_X_VALUES',d,nprob)
              CALL PSET1R('GRAPH_CURVE_Y_VALUES',s,nprob)
              CALL PSETC('GRAPH_LINE_COLOUR','BLACK')
              CALL PSETC('GRAPH_SYMBOL_COLOUR','BLACK')
              CALL PSETC('LEGEND','ON')
              CALL PSETC('LEGEND_USER_TEXT','BS')
              CALL PGRAPH
              CALL PSETC('LEGEND','OFF')
              CALL PSETC('GRAPH_SYMBOL','OFF')
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
              CALL PSETR('PAGE_X_LENGTH',PXL)
              CALL PSETR('PAGE_Y_LENGTH',PYL)
              CALL PSETR('PAGE_X_POSITION',3*SPXL/5)
              CALL PSETR('PAGE_Y_POSITION',3*SPYL/5)
              CALL PSETR('PAGE_X_GAP',0.)
              CALL PSETR('PAGE_Y_GAP',0.)
              CALL PSETR('SUBPAGE_Y_POSITION',SBPYP)
              CALL PSETR('SUBPAGE_Y_LENGTH',SBPYL)
              CALL PSETR('SUBPAGE_X_POSITION',SBPXP)
              CALL PSETR('SUBPAGE_X_LENGTH',SBPXL)
              CALL PSETC('SUBPAGE_FRAME','OFF')
c
              CALL PSETI('TEXT_LINE_COUNT',9)
              CALL PSETR('TEXT_REFERENCE_CHARACTER_HEIGHT',0.4)
              CALL PSETC('TEXT_MODE','POSITIONAL')
              CALL PSETR('TEXT_BOX_X_POSITION',SBPXP)
              CALL PSETR('TEXT_BOX_Y_POSITION',SBPYP)
              CALL PSETR('TEXT_BOX_X_LENGTH',SBPXL)
              CALL PSETR('TEXT_BOX_Y_LENGTH',SBPYL)
c
              if(boot.eq.1)then
                WRITE(CTITG0,'(a,i2.2,a,i5,a)')
     >             'Skill scores and ',int(topth-botth),
     >             '% conf. intervals (',nsam,' samples)'
                CALL PSETC('TEXT_LINE_1',CTITG0(1:lena(CTITG0)))
                write(*,*)CTITG0
              else
                WRITE(CTITG0,'(a)')
     >           'Skill scores'
                CALL PSETC('TEXT_LINE_1',CTITG0(1:lena(CTITG0)))
                write(*,*)CTITG0
              endif
c
              WRITE(CTITG1,'(a,f6.3)')
     >           'Brier skill score (full):        ',bss_ful(0)
              if(boot.eq.1)then
                do ib=1,nsam
                   xarray(ib)=bss_ful(ib)
                enddo
                call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                write(form1,'(a,f6.3,a,f6.3,a)')
     >             ' (',x_bot,',',x_top,')'
                ctitg1=ctitg1(1:lena(ctitg1))//form1(1:lena(form1))
              endif
              CALL PSETC('TEXT_LINE_2',CTITG1(1:lena(CTITG1)))
              write(*,*)CTITG1
c
              WRITE(CTITG2,'(a,f6.3)')
     >           'Brier skill score (binned):  ',bss_red(0)
              if(boot.eq.1)then
                do ib=1,nsam
                   xarray(ib)=bss_red(ib)
                enddo
                call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                write(form1,'(a,f6.3,a,f6.3,a)')
     >             ' (',x_bot,',',x_top,')'
                ctitg2=ctitg2(1:lena(ctitg2))//form1(1:lena(form1))
              endif
              CALL PSETC('TEXT_LINE_3',CTITG2(1:lena(CTITG2)))
              write(*,*)CTITG2
c
              WRITE(CTITG3,'(a,f6.3)')
     >           'Brier skill score (infinite ensemble): ',bss_inf(0)
              if(boot.eq.1)then
                do ib=1,nsam
                   xarray(ib)=bss_inf(ib)
                enddo
                call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                write(form1,'(a,f6.3,a,f6.3,a)')
     >             ' (',x_bot,',',x_top,')'
                ctitg3=ctitg3(1:lena(ctitg3))//form1(1:lena(form1))
              endif
              CALL PSETC('TEXT_LINE_4',CTITG3(1:lena(CTITG3)))
              write(*,*)CTITG3
c
              WRITE(CTITG4,'(a,f6.3)')
     >           'Reliability skill score:        ',bss_rel(0)
              if(boot.eq.1)then
                do ib=1,nsam
                   xarray(ib)=bss_rel(ib)
                enddo
                call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                write(form1,'(a,f6.3,a,f6.3,a)')
     >             ' (',x_bot,',',x_top,')'
                ctitg4=ctitg4(1:lena(ctitg4))//form1(1:lena(form1))
              endif
              CALL PSETC('TEXT_LINE_5',CTITG4(1:lena(CTITG4)))
              write(*,*)CTITG4
c
              WRITE(CTITG5,'(a,f6.3)')
     >           'Resolution skill score:       ',bss_res(0)
              if(boot.eq.1)then
                do ib=1,nsam
                   xarray(ib)=bss_res(ib)
                enddo
                call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                write(form1,'(a,f6.3,a,f6.3,a)')
     >             ' (',x_bot,',',x_top,')'
                ctitg5=ctitg5(1:lena(ctitg5))//form1(1:lena(form1))
              endif
              CALL PSETC('TEXT_LINE_6',CTITG5(1:lena(CTITG5)))
              write(*,*)CTITG5
c
              WRITE(CTITG6,'(a,f6.4)')
     >           'Sharpness:                          ',sd_prob(0)
              if(boot.eq.1)then
                do ib=1,nsam
                   xarray(ib)=sd_prob(ib)
                enddo
                call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                write(form1,'(a,f6.4,a,f6.4,a)')
     >             ' (',x_bot,',',x_top,')'
                ctitg6=ctitg6(1:lena(ctitg6))//form1(1:lena(form1))
              endif
              CALL PSETC('TEXT_LINE_7',CTITG6(1:lena(CTITG6)))
              write(*,*)CTITG6
c
              WRITE(CTITG7,'(a,f6.3)')
     >           'ROC skill score:                  ',roc(0)
              if(boot.eq.1)then
                do ib=1,nsam
                   xarray(ib)=roc(ib)
                enddo
                call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                write(form1,'(a,f6.3,a,f6.3,a)')
     >             ' (',x_bot,',',x_top,')'
                ctitg7=ctitg7(1:lena(ctitg7))//form1(1:lena(form1))
              else
                write(form1,'(a,f6.3,a,f6.3,a)')
     >             ' (',roclow(0),',',rocsup(0),')'
                ctitg7=ctitg7(1:lena(ctitg7))//form1(1:lena(form1))
              endif
              CALL PSETC('TEXT_LINE_8',CTITG7(1:lena(CTITG7)))
              write(*,*)CTITG7
c
              if(boot.eq.1)then
                WRITE(CTITG8,'(a,a,f6.3,a,f6.3,a)')
     >             '                                           ',
     >             '             (',roclow(0),',',rocsup(0),')'
                CALL PSETC('TEXT_LINE_9',CTITG8(1:lena(CTITG8)))
                write(*,*)CTITG8
              endif
              CALL PTEXT
c
              CALL PCLOSE
              close(1)
              close(2)
           enddo !ievt loop
c
           deallocate (obs_occ,                              stat=istat)
           deallocate (xprob,                                stat=istat)
           deallocate (sample,                               stat=istat)
           deallocate (yprob,                                stat=istat)
           deallocate (bss_ful,                              stat=istat)
           deallocate (bss_red,                              stat=istat)
           deallocate (rel,                                  stat=istat)
           deallocate (bss_rel,                              stat=istat)
           deallocate (res,                                  stat=istat)
           deallocate (bss_res,                              stat=istat)
           deallocate (unc,                                  stat=istat)
           deallocate (totocc_rel,                           stat=istat)
           deallocate (ave_prob,                             stat=istat)
           deallocate (sd_prob,                              stat=istat)
           deallocate (roc,                                  stat=istat)
           deallocate (rocsup,                               stat=istat)
           deallocate (roclow,                               stat=istat)
           deallocate (ntot,                                 stat=istat)
           deallocate (xarray,                               stat=istat)
c
         endif !rcal=1
      enddo !ir loop
c
      goto 999
 998  write(*,*)'Sorry, no success :('
      call abort
 999  write(*,*)'program seems to be successfully finished :)'
c
      end
