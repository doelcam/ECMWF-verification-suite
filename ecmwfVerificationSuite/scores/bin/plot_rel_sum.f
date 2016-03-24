c
c----------------------------------------------------------------------c
c     PROGRAM plot_rel_sum                P. Doblas-Reyes 15-Feb-2007  c
c                                                                      c
c     PURPOSE:                                                         c
c     Plots the probabilistic scores for regions in bar diagrams for   c
c     all the start dates and lead times dealt with in the suite.      c
c     Includes confidence intervals for the scores.                    c
c                                                                      c
c     INPUT:                                                           c
c     BRV_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE_NAMR           c
c                                                          ASCII file  c
c                                                                      c
c     OUTPUT:                                                          c
c     REL_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_NAMR_sum.ps            c
c                                                             ps file  c
c                                                                      c
c     USAGE:                                                           c
c     plot_rel_sum.x < nlist                                           c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 plot_rel_sum.f tools.f tools_plot.f                c
c     -o plot_rel_sum.x $MAGLIB $EMOSLIB $NAGLIB                       c
c                                                                      c
c     MODS:                                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      PROGRAM plot_rel_sum
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: bss_ful(:,:,:,:), bss_red(:,:,:,:)
      real, allocatable :: bss_inf(:,:,:,:)
      real, allocatable :: rel(:,:,:,:), bss_rel(:,:,:,:)
      real, allocatable :: res(:,:,:,:), bss_res(:,:,:,:)
      real, allocatable :: unc(:,:,:,:), totocc_rel(:,:,:,:)
      real, allocatable :: ave_prob(:,:,:,:), sd_prob(:,:,:,:)
      real, allocatable :: roc(:,:,:,:)
      real, allocatable :: rocsup(:,:,:,:), roclow(:,:,:,:)
      real, allocatable :: xarray(:)
      real, allocatable :: x(:), y1(:), y2(:), score(:)
      character*20, allocatable :: clist(:)
      character*20, allocatable :: ticklabel(:)
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
      character*20 bcolour(15)
      data bcolour/'blue','evergreen','orange','cyan','pink',
     >   'grey','yellow','coral','purple','mustard',
     >   'greenish_yellow','violet','purple','khaki','navy'/
c
c.... other definitions
c
      integer nx, ny, nens
      integer iyy1, iyy2, nsdt
      integer cros, ipar, ilev, anin, nmod, nldt, mult
      integer nreg, nevt, boot, nsam, iprb, npro, permeth
      integer i, nyear, nbin, istat, ievt
      integer nprob, ir, lena, iprob, ibin
      integer ntop, nbot, nsamin, ib, imod, im, il
      integer exptl, nint
      integer ntest, nens_tot
      real topth, botth
      real x_top, x_bot
      real xmax, xmin
      real spxl, spyl, sbpxl, sbpyl, sbpxp, sbpyp, pxl, pyl
      real obsocc_rel
      real ymax, ymin, yint, h, shift, xinc
      character*110 yifile, yrfile, yofile
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
     >                   iyy1, iyy2, nsdt,  imm,  idd,  itt,
     >                   cros, ipar, ilev, anin, nldt,  nf1,  nf2,
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
      write(*,*)'nsdt: ',nsdt
      write(*,*)' imm: ',(imm(i),i=1,nsdt)
      write(*,*)' idd: ',(idd(i),i=1,nsdt)
      write(*,*)' itt: ',(itt(i),i=1,nsdt)
      write(*,*)'cros: ',cros
      write(*,*)'ipar: ',ipar
      write(*,*)'ilev: ',ilev
      write(*,*)'anin: ',anin
      write(*,*)'nldt: ',nldt
      write(*,*)' nf1: ',(nf1(i),i=1,nldt)
      write(*,*)' nf2: ',(nf2(i),i=1,nldt)
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
      allocate (bss_ful(nsdt,nldt,nmod,0:nsam),              stat=istat)
      allocate (bss_red(nsdt,nldt,nmod,0:nsam),              stat=istat)
      allocate (bss_inf(nsdt,nldt,nmod,0:nsam),              stat=istat)
      allocate (rel(nsdt,nldt,nmod,0:nsam),                  stat=istat)
      allocate (bss_rel(nsdt,nldt,nmod,0:nsam),              stat=istat)
      allocate (res(nsdt,nldt,nmod,0:nsam),                  stat=istat)
      allocate (bss_res(nsdt,nldt,nmod,0:nsam),              stat=istat)
      allocate (unc(nsdt,nldt,nmod,0:nsam),                  stat=istat)
      allocate (totocc_rel(nsdt,nldt,nmod,0:nsam),           stat=istat)
      allocate (ave_prob(nsdt,nldt,nmod,0:nsam),             stat=istat)
      allocate (sd_prob(nsdt,nldt,nmod,0:nsam),              stat=istat)
      allocate (roc(nsdt,nldt,nmod,0:nsam),                  stat=istat)
      allocate (rocsup(nsdt,nldt,nmod,0:nsam),               stat=istat)
      allocate (roclow(nsdt,nldt,nmod,0:nsam),               stat=istat)
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
           im=1
           call titles(im, ipar, ilev, namr(ir), dia,
     >        start, variable, regionname,
     >        diagnostic, nint, clist, llist, xmax, xmin)
           write(*,*)diagnostic,' for region ',regionname
c
c.... loop over the events
c
           do ievt=1,nevt
c
c.... open output file
c
              yofile='DIA_YYYY-YYYY_CVYY_II_PAR_LEV_THR_BIN'
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
              write(yofile(30:31),'(a2)')'_T'
              write(yofile(32:33),'(i2.2)')pevt(ievt)
              write(yofile(34:35),'(a2)')'_B'
              write(yofile(36:37),'(i2.2)')npro
              yofile=yofile(1:lena(yofile))//'_'//namr(ir)
              if(boot.eq.1)then
                write(yofile(43:44),'(a2)')'_S'
                write(yofile(45:49),'(i5.5)')nsam
              endif
              yofile='psfiles/'//yofile(1:lena(yofile))//'_sum.ps'
              write(*,*)'Output file ',yofile(1:lena(yofile)),
     >           ' is a postscript file'
c
c.... loop over the models
c
              nens_tot=0
              do imod=1,nmod
                 exptl=lena(expm(imod))
                 write(form1,'(a,i2.2,a)')'(a',exptl,')'
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
                 write(*,'(a,3i3)')
     >              'Parameters for probabilistic scores ',
     >              iprb,nprob,nbin
                 nens_tot=nens_tot+nens
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
                       yifile=
     >'BRV_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_THR_MM_BIN_AAA-EEE'
                       write(yifile(       5: 4+exptl),form1)
     >expm(imod)(1:exptl)
                       write(yifile( 5+exptl: 5+exptl),'(a1)')'_'
                       write(yifile( 6+exptl: 9+exptl),'(i4.4)')iyy1
                       write(yifile(10+exptl:10+exptl),'(a1)')'-'
                       write(yifile(11+exptl:14+exptl),'(i4.4)')iyy2
                       write(yifile(15+exptl:15+exptl),'(a1)')'_'
                       write(yifile(16+exptl:17+exptl),'(i2.2)')imm(im)
                       write(yifile(18+exptl:19+exptl),'(i2.2)')idd(im)
                       write(yifile(20+exptl:21+exptl),'(i2.2)')itt(im)
                       write(yifile(22+exptl:24+exptl),'(a3)')'_CV'
                       write(yifile(25+exptl:26+exptl),'(i2.2)')cros
                       write(yifile(27+exptl:28+exptl),'(a2)')'_I'
                       write(yifile(29+exptl:29+exptl),'(i1.1)')anin
                       write(yifile(30+exptl:30+exptl),'(a1)')'_'
                       write(yifile(31+exptl:33+exptl),'(i3.3)')ipar
                       write(yifile(34+exptl:34+exptl),'(a1)')'_'
                       write(yifile(35+exptl:37+exptl),'(i3.3)')ilev
                       write(yifile(38+exptl:39+exptl),'(a2)')'_T'
                       write(yifile(40+exptl:41+exptl),'(i2.2)')
     >pevt(ievt)
                       write(yifile(42+exptl:43+exptl),'(a2)')'_M'
                       write(yifile(44+exptl:44+exptl),'(i1.1)')permeth
                       write(yifile(45+exptl:46+exptl),'(a2)')'_B'
                       write(yifile(47+exptl:48+exptl),'(i2.2)')nprob
                       write(yifile(49+exptl:49+exptl),'(a1)')'_'
                       write(yifile(50+exptl:52+exptl),'(i3.3)')nf1(il)
                       write(yifile(53+exptl:53+exptl),'(a1)')'-'
                       write(yifile(54+exptl:56+exptl),'(i3.3)')nf2(il)
                       yifile=yifile(1:lena(yifile))//'_'//namr(ir)
                       write(*,*)'open input file: ',
     >                    yifile(1:lena(yifile))
                       open(10,file=yifile,form='formatted',
     >                    status='unknown')
                       if(boot.eq.1)then
                         yrfile=yifile
                         write(yrfile(62+exptl:63+exptl),'(a2)')'_S'
                         write(yrfile(64+exptl:68+exptl),'(i5.5)')nsam
                         yrfile=yrfile(1:lena(yrfile))//'.ran'
                         write(*,*)'Open input file ',
     >                      yrfile(1:lena(yrfile))
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
                       endif
c
c.... reading data (skips the value information at the end)
c.... skip the line with the lead time
c
                       read(10,*)
c
c.... skip the results for individual years
c
                       do i=1,nyear
                          read(10,*)
                       enddo
c
c.... skip the results for the attributes diagram
c
                       do iprob=1,nprob
                          read(10,*)
                       enddo
c
c.... read the average scores
c
                       read(10,'(11f10.4)')bss_ful(im,il,imod,0),
     >                    bss_red(im,il,imod,0),
     >                    bss_inf(im,il,imod,0),rel(im,il,imod,0),
     >                    bss_rel(im,il,imod,0),res(im,il,imod,0),
     >                    bss_res(im,il,imod,0),unc(im,il,imod,0),
     >                    totocc_rel(im,il,imod,0),
     >                    ave_prob(im,il,imod,0),sd_prob(im,il,imod,0)
c
c.... ROC skill score
c
                       read(10,'(3f10.4)')roc(im,il,imod,0),
     >                    rocsup(im,il,imod,0),roclow(im,il,imod,0)
c
c.... results from bootstrap
c
                       if(boot.eq.1)then
                         do ib=1,nsam
                            do iprob=1,nprob
                               read(11,*)
                            enddo
                            read(11,'(11f10.4)')bss_ful(im,il,imod,ib),
     >                         bss_red(im,il,imod,ib),
     >                         bss_inf(im,il,imod,ib),
     >                         rel(im,il,imod,ib),
     >                         bss_rel(im,il,imod,ib),
     >                         res(im,il,imod,ib),
     >                         bss_res(im,il,imod,ib),
     >                         unc(im,il,imod,ib),
     >                         totocc_rel(im,il,imod,ib),
     >                         ave_prob(im,il,imod,ib),
     >                         sd_prob(im,il,imod,ib)
                            read(11,'(3f10.4)')roc(im,il,imod,ib),
     >                         rocsup(im,il,imod,ib),
     >                         roclow(im,il,imod,ib)
c
c.... skip HR/FR and ROC with nbin
c
                            do ibin=1,nbin+1
                               read(11,*)
                            enddo
                         enddo !ib loop
                         close(11)
                       endif
                       close(10)
                    enddo !il loop
                 enddo !im loop
              enddo !imod loop

C**************************************
C     INITIALISATION DE MAGICS
C**************************************
              CALL POPEN
              CALL PSETC('PS_DEVICE','ps_a4')
              CALL PSETC('PS_FILE_NAME',yofile)
C             CALL PSETC('WORKSTATION_1','PS_COL_A4_VERTICAL')
C             CALL PSETC('WORKSTATION_1','PS_COL_A4_HORIZONTAL')
              CALL PSETC('LAYOUT','POSITIONAL')
C
C     PLOTTING SECTION
C
              CALL PSETR('AXIS_TICK_SIZE',0.3)
              CALL PSETR('AXIS_MINOR_TICK_SIZE',0.15)
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
              CALL PSETR('legend_text_font_size',0.38)
              CALL PSETR('LEGEND_TEXT_MAXIMUM_HEIGHT',0.8)
              CALL PSETR('LEGEND_ENTRY_MAXIMUM_WIDTH',8.0)
              CALL PSETR('LEGEND_ENTRY_MAXIMUM_HEIGHT',0.8)
              CALL PSETR('LEGEND_BOX_Y_LENGTH',8.0)
              CALL PSETI('LEGEND_COLUMN_COUNT',5)
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
c              CALL PSETR('TEXT_REFERENCE_CHARACTER_HEIGHT',0.5)
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
              WRITE(CTITG3,'(A,I4.4,A,I4.4)')
     >           'Hindcast period ',iyy1,'-',iyy2
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
              CALL PSETC('TEXT_font_size','0.6')
c              CALL PSETC('TEXT_REFERENCE_CHARACTER_HEIGHT',0.5)
              CALL PSETC('TEXT_LINE_1',CTITG1(1:lena(CTITG1)))
              CALL PSETC('TEXT_LINE_2',CTITG2(1:lena(CTITG2)))
              CALL PSETC('TEXT_LINE_3',CTITG3(1:lena(CTITG3)))
              CALL PSETC('TEXT_LINE_4', '<font size="0.5">' //
     >               CTITG4(1:lena(CTITG4)) // '</font>')
              CALL PSETC('TEXT_LINE_5'," ")
c
              if(boot.eq.1)then
                 WRITE(CTITG5,'(a,i2.2,a,i5,a)')'Bars are ',
     >             int(topth-botth),'% conf. intervals computed with ',
     >             nsam,' samples'
                 CALL PSETC('TEXT_LINE_5','<font size="0.5">' //
     >                 CTITG5(1:lena(CTITG5))// '</font>')

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
              CALL PSETR('AXIS_TITLE_HEIGHT',0.20)
              allocate (ticklabel(0:nsdt*nldt+1),            stat=istat)
              i=0
              ticklabel(i)=' '
              do im=1,nsdt
                 call titles(imm(im), ipar, ilev, namr(ir), dia,
     >              start, variable, regionname,
     >              diagnostic, nint, clist, llist, xmax, xmin)
                 do il=1,nldt
                    i=i+1
                    write(ticklabel(i),'(a,a,i1.1,a,i1.1,a)')
     >                 start(1:3),' (',nf1(il),'-',nf2(il),')'
                    if(nf2(il).gt.9)
     >                 write(ticklabel(i),'(a,a,i1.1,a,i2.2,a)')
     >                 start(1:3),' (',nf1(il),'-',nf2(il),')'
                    if(nf1(il).gt.9)
     >                 write(ticklabel(i),'(a,a,i2.2,a,i2.2,a)')
     >                 start(1:3),' (',nf1(il),'-',nf2(il),')'
                 enddo
              enddo
              ticklabel(nsdt*nldt+1)=' '
              CALL PSETC('AXIS_ORIENTATION','HORIZONTAL')
              CALL PSETC('AXIS_POSITION','BOTTOM')
              CALL PSETC('AXIS_TYPE','REGULAR')
              CALL PSETC('AXIS_TITLE','ON')
              CALL PSETR('AXIS_TITLE_height',0.6)
              CALL PSETC('AXIS_TITLE_TEXT',
     >           'Start date (Forecast range)')
              CALL PSETR('AXIS_MIN_VALUE',0.)
              CALL PSETR('AXIS_MAX_VALUE',float(nsdt*nldt+1))
              CALL PSETR('AXIS_TICK_INTERVAL',1.)
              CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',1)
              CALL PSETC('AXIS_TICK_LABEL_TYPE','LABEL_LIST')
              CALL PSETC('AXIS_TICK_LABEL_FIRST','OFF')
              CALL PSETC('AXIS_TICK_LABEL_LAST','ON')
              CALL PSETR('AXIS_TICK_LABEL_HEIGHT',0.4)
              CALL PSET1C('AXIS_TICK_LABEL_LIST',ticklabel,nsdt*nldt+2)
              CALL PAXIS
              deallocate (ticklabel,                         stat=istat)
c
              ymax=1.0
              ymin=-1.0
              yint=0.2
              if(dia.eq.'RES')ymin=0.0
              if(dia.eq.'REL')then
                ymin=0.5
                yint=0.1
              endif
              if(dia.eq.'SHA')then
                ymax=0.015
                ymin=0.0
                yint=0.003
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
              if(nmod.eq.1)then
                shift=0.
                xinc=0.
              else
                shift=0.5
                xinc=0.60/nmod
              endif
c
c.... reference lines
c
              CALL PSETC('LEGEND','OFF')
              allocate (x(0:nsdt*nldt+3),                    stat=istat)
              allocate (y1(0:nsdt*nldt+3),                   stat=istat)
              do i=0,nsdt*nldt+3
                 x(i)=float(i)
                 y1(i)=0.
              enddo
              CALL PSETC('GRAPH_TYPE','CURVE')
              CALL PSETC('GRAPH_LINE_COLOUR','BLACK')
              CALL PSETC('GRAPH_LINE_STYLE','SOLID')
              CALL PSETI('GRAPH_LINE_THICKNESS',1)
              CALL PSET1R('GRAPH_CURVE_X_VALUES',x,nsdt*nldt+4)
              CALL PSET1R('GRAPH_CURVE_Y_VALUES',y1,nsdt*nldt+4)
              CALL PGRAPH
              deallocate (x,                                 stat=istat)
              deallocate (y1,                                stat=istat)
              deallocate (score,                             stat=istat)
c
              allocate (x(2),                                stat=istat)
              allocate (y1(2),                               stat=istat)
              CALL PSETC('GRAPH_TYPE','CURVE')
              CALL PSETC('GRAPH_LINE_COLOUR','BLACK')
              CALL PSETC('GRAPH_LINE_STYLE','DOT')
              CALL PSETI('GRAPH_LINE_THICKNESS',1)
              do i=1,nsdt-1
                 x(1)=float(i*nldt)+nmod*xinc-shift/2.
                 x(2)=x(1)
                 y1(1)=ymin
                 y1(2)=ymax
                 CALL PSET1R('GRAPH_CURVE_X_VALUES',x,2)
                 CALL PSET1R('GRAPH_CURVE_Y_VALUES',y1,2)
                 CALL PGRAPH
              enddo
              deallocate (x,                                 stat=istat)
              deallocate (y1,                                stat=istat)
c
              CALL PSETC('GRAPH_TYPE','BAR')
              CALL PSETC('GRAPH_SHADE','ON')
              CALL PSETR('GRAPH_BAR_WIDTH',0.5)
              CALL PSETC('GRAPH_SHADE_STYLE','AREA_FILL')
              CALL PSETC('GRAPH_SHADE_COLOUR','RED')
              CALL PSETC('GRAPH_BAR_COLOUR','WHITE')
              imarker(1)=28
              CALL PSETC('SYMBOL_POSITION_MODE','GRAPH')
              CALL PSETC('SYMBOL_TYPE','MARKER')
c              CALL PSET1I('SYMBOL_INPUT_MARKER_LIST',imarker,1)
              CALL PSETI('SYMBOL_MARKER_INDEX',28)
              CALL PSETC('SYMBOL_COLOUR','BLACK')
c              CALL PSETR('SYMBOL_HEIGHT',0.20)
              CALL PSETR('SYMBOL_HEIGHT',0.5)
              CALL PSETC('LEGEND','OFF')
c
              allocate (x(1),                                stat=istat)
              allocate (y1(1),                               stat=istat)
              allocate (y2(1),                               stat=istat)
              allocate (score(nmod),                         stat=istat)
              do im=1,nsdt
                 do il=1,nldt
                    do imod=1,nmod
                       if(dia.eq.'BSS')score(imod)=bss_ful(im,il,imod,0)
                       if(dia.eq.'BSI')score(imod)=bss_inf(im,il,imod,0)
                       if(dia.eq.'REL')score(imod)=
     >                    bss_rel(im,il,imod,0)
                       if(dia.eq.'RES')score(imod)=
     >                    bss_res(im,il,imod,0)
                       if(dia.eq.'SHA')score(imod)=
     >                    sd_prob(im,il,imod,0)
                       if(dia.eq.'ROC')score(imod)=roc(im,il,imod,0)
                    enddo
                    do imod=1,nmod
c
c.... bars and legends
c
                       if(nmod.gt.1)then
                         CALL PSETC('GRAPH_SHADE_COLOUR',bcolour(imod))
                         CALL PSETC('GRAPH_BAR_COLOUR',bcolour(imod))
                         if((imod.eq.nmod).and.(mult.eq.1))then
                           CALL PSETC('GRAPH_SHADE_COLOUR','RED')
                           CALL PSETC('GRAPH_BAR_COLOUR','RED')
                           write(legend,'(a,a,i3,a)')expm(imod)(2:5),
     >                        ' (',nenm(imod),')'
                           CALL PSETR('GRAPH_BAR_WIDTH',0.04)
c                           CALL PSETR('GRAPH_BAR_WIDTH',0.20)
                         else
                           CALL PSETR('GRAPH_BAR_WIDTH',0.03)
c                           CALL PSETR('GRAPH_BAR_WIDTH',0.15)
                           if(typm(imod).eq.1)then
                             write(legend,'(a,a,i3,a)')expm(imod)(2:5),
     >                          ' (',nenm(imod),')'
                           else
                             write(legend,'(a,a,a,a,i2,a)')
     >                          expm(imod)(5:8),'-',
     >                          expm(imod)(10:13),' (',nenm(imod),')'
                           endif
                         endif
                       else
                         CALL PSETC('GRAPH_SHADE_COLOUR','RED')
                         CALL PSETC('GRAPH_BAR_COLOUR','RED')
                         CALL PSETR('GRAPH_BAR_WIDTH',0.20)
                         CALL PSETC('LEGEND','ON')
                         if(typm(1).eq.1)then
                           write(legend,'(a,a,i2,a)')expm(1)(2:5),
     >                        ' (',nenm(1),')'
                         else
                           write(legend,'(a,a,a,a,i2,a)')
     >                        expm(1)(5:8),'-',
     >                        expm(1)(10:13),' (',nenm(1),')'
                         endif
                       endif
                       CALL PSETC('LEGEND_USER_TEXT',
     >                    legend(1:lena(legend)))
                       write(*,*)legend
                       CALL PSET1R('GRAPH_BAR_X_VALUES',x,1)
                       CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',y1,1)
                       CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',y2,1)
                       CALL PSETC('LEGEND','OFF')
                       if((im.eq.1).and.(il.eq.1))
     >                    CALL PSETC('LEGEND','ON')
c
c.... abscissa
c
                       x(1)=float((im-1)*nldt+il)-shift+imod*xinc
                       if((imod.eq.nmod).and.(mult.eq.1))x(1)=
     >                   float((im-1)*nldt+il)-shift+(nmod+1)*xinc
c
c.... bars with or without confidence intervals
c
                       if(boot.eq.1)then
                         do ib=1,nsam
                            if(dia.eq.'BSS')xarray(ib)=
     >                         bss_ful(im,il,imod,ib)
                            if(dia.eq.'BSI')xarray(ib)=
     >                         bss_inf(im,il,imod,ib)
                            if(dia.eq.'REL')xarray(ib)=
     >                         bss_rel(im,il,imod,ib)
                            if(dia.eq.'RES')xarray(ib)=
     >                         bss_res(im,il,imod,ib)
                            if(dia.eq.'SHA')xarray(ib)=
     >                         sd_prob(im,il,imod,ib)
                            if(dia.eq.'ROC')xarray(ib)=
     >                         roc(im,il,imod,ib)
                         enddo
                         call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                         write(*,'(a,f6.3,a,f6.3,a)')'(',x_bot,',',x_top,')'
                         y1(1)=x_bot
                         y2(1)=x_top
                         CALL PSET1R('GRAPH_BAR_X_VALUES',x,1)
                         CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',y1,1)
                         CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',y2,1)
                         CALL PGRAPH
                        call psetc("legend", "off")
                         y1(1)=score(imod)
                         CALL PSET1R('SYMBOL_INPUT_X_POSITION',x,1)
                         CALL PSET1R('SYMBOL_INPUT_Y_POSITION',y1,1)
                         CALL PSYMB
                       else
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
c                        write(*,*)imod,score(imod),y1(1),y2(1)
                       endif
                    enddo
                 enddo
              enddo
c.... confidence intervals
c
              deallocate (x,                                 stat=istat)
              deallocate (y1,                                stat=istat)
              deallocate (y2,                                stat=istat)
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
