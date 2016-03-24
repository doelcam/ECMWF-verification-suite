c
c----------------------------------------------------------------------c
c     PROGRAM plot_acc_sum                P. Doblas-Reyes 20-Feb-2007  c
c                                                                      c
c     PURPOSE:                                                         c
c     Plots bar diagrams of ACC, perfect-model ACC, ratio spread/RMSE  c
c     spread and MSSS for different start dates and lead times         c
c                                                                      c
c     INPUT:                                                           c
c     DIA_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE_NAMR           c
c                                                          ASCII file  c
c                                                                      c
c     OUTPUT:                                                          c
c     DIA_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_NAMR_sum.ps            c
c                                                             ps file  c
c                                                                      c
c     USAGE:                                                           c
c     plot_acc_sum.x < nlist                                           c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 plot_acc_sum.f tools.f tools_plot.f                c
c     -o plot_acc_sum.x $MAGLIB $EMOSLIB $NAGLIB                       c
c                                                                      c
c     MODS:                                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      PROGRAM plot_acc_sum
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: val(:,:,:,:)
      real, allocatable :: xarray(:)
      real, allocatable :: x(:), y1(:), y2(:), score(:)
      character*20, allocatable :: clist(:)
      character*20, allocatable :: ticklabel(:)
      real, allocatable :: llist(:)
c
c.... hard coded field definitions
c
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
      integer cros, ipar, ilev, anin, nmod, nldt
      integer nreg, boot, nsam, mult
      integer i, nyear, istat
      integer ir, lena, iy, iens
      integer ntop, nbot, nsamin, ib, imod, im, il
      integer exptl, nint
      integer ntest, nens_tot, nskip, nensp1, nensr
      real topth, botth
      real x_top, x_bot
      real xmax, xmin
      real spxl, spyl, sbpxl, sbpyl, sbpxp, sbpyp, pxl, pyl
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
      character*40 form1, form2
      character*21 expt
c
c.... namelist
c
      namelist /control/   nx,   ny, nenm,
     >                   iyy1, iyy2, nsdt,  imm,  idd,  itt,
     >                   cros, ipar, ilev, anin, nldt,  nf1,  nf2,
     >                   expm, typm, nmod, mult,  dia, boot, nsam,
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
      write(*,*)'boot: ',boot
      write(*,*)'nsam: ',nsam
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
      if(nmod.gt.1)then
        nensp1=nmod
      else
        nens=nenm(nmod)
        nensp1=nens+1
        if((dia.eq.'SPR').or.(dia.eq.'RSR'))nensp1=1
      endif
      if(boot.eq.0)nsam=0
c
c.... allocate fields
c
      allocate (val(nsdt,nldt,nensp1,0:nsam),                stat=istat)
      allocate (xarray(nsam),                                stat=istat)
c
c.... main loop over all regions
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
           yofile=yofile(1:lena(yofile))//'_'//namr(ir)
           if(boot.eq.1)then
             write(yofile(35:36),'(a2)')'_S'
             write(yofile(37:41),'(i5.5)')nsam
           endif
           yofile='psfiles/'//yofile(1:lena(yofile))//'_sum.ps'
           write(*,*)'Output file ',yofile(1:lena(yofile)),
     >        ' is a postscript file'
c
c.... loop over the models imod
c
           nens_tot=0
           do imod=1,nmod
              exptl=lena(expm(imod))
              write(form1,'(a,i2.2,a)')'(a',exptl,')'
c
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
c.... loop over the start dates im
c
              do im=1,nsdt
c
c.... loop over the lead times il
c
                 do il=1,nldt
c
c.... open input file
c
                    yifile=
     >               'BRV_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE'
                    write(yifile(       1:       3),'(a3)')dia
                    write(yifile(       5: 4+exptl),form1)
     >               expm(imod)(1:exptl)
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
                    write(yifile(38+exptl:38+exptl),'(a1)')'_'
                    write(yifile(39+exptl:41+exptl),'(i3.3)')nf1(il)
                    write(yifile(42+exptl:42+exptl),'(a1)')'-'
                    write(yifile(43+exptl:45+exptl),'(i3.3)')nf2(il)
                    yifile=yifile(1:lena(yifile))//'_'//namr(ir)
                    write(*,*)'open input file: ',
     >                 yifile(1:lena(yifile))
                    open(10,file=yifile,form='formatted',
     >                 status='unknown')
                    if(boot.eq.1)then
                      yrfile=yifile
                      write(yrfile(51+exptl:52+exptl),'(a2)')'_S'
                      write(yrfile(53+exptl:57+exptl),'(i5.5)')nsam
                      yrfile=yrfile(1:lena(yrfile))//'.ran'
                      write(*,*)'Open input file ',
     >                   yrfile(1:lena(yrfile))
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
c.... reading data
c
                    read(10,*)
                    if(nmod.gt.1)then
                      do iy=1,nyear
                         read(10,*)
                      enddo
                      read(10,form2)val(im,il,imod,0)
                    else
                      do iy=1,nyear
                         read(10,*)
                      enddo
                      read(10,form2)(val(im,il,iens,0),iens=1,nensr)
                    endif
c
                    if(boot.eq.1)then
                      do ib=1,nsam
                         if(nmod.gt.1)then
                           read(11,form2)val(im,il,imod,ib)
                         else
                           read(11,form2)
     >                        (val(im,il,iens,ib),iens=1,nensr)
                         endif
                      enddo !ib loop
                      close(11)
                    endif
                    close(10)
                 enddo !il loop
              enddo !im loop
           enddo !imod loop
c
c.... searching for min/max
c.... required to draw the axes of MSS, SPR and SPR
c
           x_top=0.
           x_bot=0.
           do imod=1,nmod
              do im=1,nsdt
                 do il=1,nldt
                    do ib=0,nsam
                       if(val(im,il,imod,ib).gt.x_top)
     >                    x_top=val(im,il,imod,ib)
                       if(val(im,il,imod,ib).lt.x_bot)
     >                    x_bot=val(im,il,imod,ib)
                    enddo
                 enddo !il loop
              enddo !im loop
           enddo !imod loop
           write(*,*)'Range ',x_bot,x_top

C*********************************************************
C     INITIALISATION DE MAGICS
C*********************************************************
           CALL POPEN
           CALL PSETC('PS_DEVICE','ps_a4')
           CALL PSETC('PS_FILE_NAME',yofile)
C          CALL PSETC('WORKSTATION_1','PS_COL_A4_VERTICAL')
C          CALL PSETC('WORKSTATION_1','PS_COL_A4_HORIZONTAL')
           CALL PSETC('LAYOUT','POSITIONAL')
C
C     PLOTTING SECTION
C
c           CALL PSETR('AXIS_TICK_SIZE',0.1)
           CALL PSETR('AXIS_TICK_SIZE',0.3)
c           CALL PSETR('AXIS_MINOR_TICK_SIZE',0.05)
           CALL PSETR('AXIS_MINOR_TICK_SIZE',0.15)
           CALL PSETR('AXIS_MINOR_TICK_MIN_GAP',0.05)
           CALL PSETC('AXIS_GRID_LINE_STYLE','DOT')
           CALL PSETC('AXIS_GRID_COLOUR','BLACK')

           SPXL=29.5
           SPYL=21.
           CALL PSETR('SUPER_PAGE_X_LENGTH',SPXL)
           CALL PSETR('SUPER_PAGE_Y_LENGTH',SPYL)
           CALL PSETC('PLOT_DIRECTION','HORIZONTAL')
C          CALL PSETC('PLOT_DIRECTION','VERTICAL')
c          CALL PSETC('PLOT_START','TOP')
           CALL PSETC('PLOT_START','BOTTOM')
           CALL PSETR('PAGE_X_GAP',0.)
           CALL PSETR('PAGE_Y_GAP',0.)

           CALL PSETC('PAGE_FRAME','OFF')
           CALL PSETC('PAGE_FRAME_COLOUR','BLACK')
           CALL PSETC('PAGE_ID_LINE','OFF')
           CALL PSETC('SUBPAGE_MAP_PROJECTION','NONE')
           CALL PSETC('SUBPAGE_FRAME_COLOUR','BLACK')

           CALL PSETC('LEGEND','OFF')
           CALL PSETC('LEGEND_BORDER','off')
           CALL PSETC('LEGEND_TEXT_COLOUR','BLACK')
           CALL PSETC('LEGEND_TEXT_QUALITY','HIGH')
           CALL PSETR('legend_text_font_size',0.38)
c           CALL PSETR('LEGEND_TEXT_MAXIMUM_HEIGHT',1.5)
           CALL PSETR('LEGEND_ENTRY_MAXIMUM_WIDTH',8.0)
           CALL PSETR('LEGEND_ENTRY_MAXIMUM_HEIGHT',1.5)
c           CALL PSETR('LEGEND_ENTRY_MAXIMUM_HEIGHT',0.8)
           CALL PSETR('LEGEND_BOX_Y_LENGTH',3.0)
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

C          CALL PSETC('TEXT_MODE','TITLE')
           CALL PSETC('TEXT_MODE','POSITIONAL')
           CALL PSETR('TEXT_BOX_Y_POSITION',SBPYP)
           CALL PSETR('TEXT_BOX_Y_LENGTH',SBPYL)
           CALL PSETR('TEXT_BOX_X_POSITION',SBPXP)
           CALL PSETR('TEXT_BOX_X_LENGTH',SBPXL)

           CALL PSETC('TEXT_COLOUR','BLACK')
           CALL PSETC('TEXT_JUSTIFICATION','LEFT')
c           CALL PSETR('TEXT_REFERENCE_CHARACTER_HEIGHT',0.5)
           CALL PSETC('TEXT_QUALITY','HIGH')
cc           CALL PSETR('TEXT_FONT_SIZE',0.6)
cc           CALL PSETR('TEXT_FONT_SIZE',0.2)
           CALL PSETI('TEXT_LINE_COUNT',4)

           WRITE(CTITG1,'(A,A,A)')
     >        diagnostic(1:lena(diagnostic)),' for ',
     >        regionname(1:lena(regionname))
           if(ilsm(ir).eq.0)mask=' (land and sea points)'
           if(ilsm(ir).eq.1)mask=' (land points only)'
           if(ilsm(ir).eq.-1)mask=' (sea points only)'
           CTITG1=CTITG1(1:lena(CTITG1))//mask(1:lena(mask))
           WRITE(CTITG2,'(A)')variable(1:lena(variable))
           WRITE(CTITG3,'(A,I4.4,A,I4.4)')
     >        'Hindcast period ',iyy1,'-',iyy2
           write(*,*)CTITG1
           write(*,*)CTITG2
           write(*,*)CTITG3

           CALL PSETC('TEXT_COLOUR','BLACK')
           CALL PSETC('TEXT_font_size','0.6')
c           CALL PSETR('TEXT_REFERENCE_CHARACTER_HEIGHT',0.5)
           CALL PSETC('TEXT_LINE_1',CTITG1(1:lena(CTITG1)))
           CALL PSETC('TEXT_LINE_2',CTITG2(1:lena(CTITG2)))
           CALL PSETC('TEXT_LINE_3',CTITG3(1:lena(CTITG3)))
           CALL PSETC('TEXT_LINE_4'," ")
c
           if(boot.eq.1)then
             WRITE(CTITG4,'(a,i2.2,a,i5,a)')'Bars are ',
     >       int(topth-botth),'% conf. intervals computed with ',
     >       nsam,' samples'
             CALL PSETC('TEXT_LINE_4','<font size="0.5">' //
     >                 CTITG4(1:lena(CTITG4))// '</font>')

             write(*,*)CTITG4
c
           endif
           CALL PTEXT

cc      CALL PSETC('PAGE_FRAME','ON')
cc      CALL PSETC('SUBPAGE_FRAME','ON')

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
           allocate (ticklabel(0:nsdt*nldt+1),               stat=istat)
           i=0
           ticklabel(i)=' '
           do im=1,nsdt
              call titles(imm(im), ipar, ilev, namr(ir), dia,
     >           start, variable, regionname,
     >           diagnostic, nint, clist, llist, xmax, xmin)
              do il=1,nldt
                 i=i+1
                 form1='(a,a,i1.1,a,i1.1,a)'
                 if(nf1(il).gt.9)form1='(a,a,i2.2,a,i2.2,a)'
                 if((nf1(il).le.9).and.(nf2(il).gt.9))
     >              form1='(a,a,i1.1,a,i2.2,a)'
                 if((nf1(il).le.9).and.(nf2(il).gt.99))
     >              form1='(a,a,i1.1,a,i3.3,a)'
                 if((nf1(il).gt.9).and.(nf2(il).gt.9))
     >              form1='(a,a,i2.2,a,i2.2,a)'
                 if((nf1(il).gt.9).and.(nf2(il).gt.99))
     >              form1='(a,a,i2.2,a,i3.3,a)'
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
c           CALL PSETR('AXIS_TITLE_height',0.5)
           CALL PSETR('AXIS_TITLE_height',0.6)
           CALL PSETC('AXIS_TITLE_TEXT','Start date (Forecast range)')
           CALL PSETR('AXIS_MIN_VALUE',0.)
           CALL PSETR('AXIS_MAX_VALUE',float(nsdt*nldt+1))
           CALL PSETR('AXIS_TICK_INTERVAL',1.)
           CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',1)
           CALL PSETC('AXIS_TICK_LABEL_TYPE','LABEL_LIST')
           CALL PSETC('AXIS_TICK_LABEL_FIRST','OFF')
           CALL PSETC('AXIS_TICK_LABEL_LAST','ON')
c           CALL PSETR('AXIS_TICK_LABEL_HEIGHT',0.23)
           CALL PSETR('AXIS_TICK_LABEL_HEIGHT',0.4)
           CALL PSET1C('AXIS_TICK_LABEL_LIST',ticklabel,nsdt*nldt+2)
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
             ymin=0.0
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
           if(nmod.eq.1)then
             shift=0.5
             xinc=0.60/nensp1
             if((dia.eq.'SPR').or.(dia.eq.'RSR'))then
               shift=0.
               xinc=0.
             endif
           else
             shift=0.5
             xinc=0.60/nmod
           endif
c
c.... reference line
c
           CALL PSETC('LEGEND','OFF')
           allocate (x(0:nsdt*nldt+3),                       stat=istat)
           allocate (y1(0:nsdt*nldt+3),                      stat=istat)
           do i=0,nsdt*nldt+3
              x(i)=float(i)
              y1(i)=0.
              if((dia.eq.'SPR').or.(dia.eq.'RSR'))y1(i)=1.
           enddo
           CALL PSETC('GRAPH_TYPE','CURVE')
           CALL PSETC('GRAPH_LINE_COLOUR','BLACK')
           CALL PSETC('GRAPH_LINE_STYLE','SOLID')
           CALL PSETI('GRAPH_LINE_THICKNESS',1)
           CALL PSET1R('GRAPH_CURVE_X_VALUES',x,nsdt*nldt+4)
           CALL PSET1R('GRAPH_CURVE_Y_VALUES',y1,nsdt*nldt+4)
           CALL PGRAPH
           deallocate (x,                                    stat=istat)
           deallocate (y1,                                   stat=istat)
c
           allocate (x(2),                                   stat=istat)
           allocate (y1(2),                                  stat=istat)
           CALL PSETC('GRAPH_TYPE','CURVE')
           CALL PSETC('GRAPH_LINE_COLOUR','BLACK')
           CALL PSETC('GRAPH_LINE_STYLE','DOT')
           CALL PSETI('GRAPH_LINE_THICKNESS',1)
           do i=1,nsdt-1
              x(1)=float(i*nldt)+nensp1*xinc-shift/2.
              x(2)=x(1)
              y1(1)=ymin
              y1(2)=ymax
              CALL PSET1R('GRAPH_CURVE_X_VALUES',x,2)
              CALL PSET1R('GRAPH_CURVE_Y_VALUES',y1,2)
              CALL PGRAPH
           enddo
           deallocate (x,                                    stat=istat)
           deallocate (y1,                                   stat=istat)
c
c.... plot the data
c
           CALL PSETC('GRAPH_TYPE','BAR')
           CALL PSETC('GRAPH_SHADE','ON')
           CALL PSETR('GRAPH_BAR_WIDTH',0.5)
           CALL PSETC('GRAPH_SHADE_STYLE','AREA_FILL')
           CALL PSETC('GRAPH_SHADE_COLOUR','RED')
           imarker(1)=28
           CALL PSETC('SYMBOL_POSITION_MODE','GRAPH')
           CALL PSETC('SYMBOL_TYPE','MARKER')
c          CALL PSET1I('SYMBOL_INPUT_MARKER_LIST',imarker,1)
           CALL PSETI('SYMBOL_MARKER_INDEX',28)

           CALL PSETC('SYMBOL_COLOUR','BLACK')
           CALL PSETC('LEGEND','OFF')
c
           allocate (x(1),                                   stat=istat)
           allocate (y1(1),                                  stat=istat)
           allocate (y2(1),                                  stat=istat)
           allocate (score(nensp1),                          stat=istat)
           do im=1,nsdt
              do il=1,nldt
                 do iens=1,nensp1
                    score(iens)=val(im,il,iens,0)
                 enddo
                 do iens=1,nensp1
c
c.... bars and legends
c
                    CALL PSETC('LEGEND','OFF')
                    if(nmod.gt.1)then
                      if((im.eq.1).and.(il.eq.1))
     >                   CALL PSETC('LEGEND','ON')
                      CALL PSETC('GRAPH_BAR_COLOUR',bcolour(iens))
                      CALL PSETC('GRAPH_SHADE_COLOUR',bcolour(iens))
                      if((iens.eq.nensp1).and.(mult.eq.1))then
                         CALL PSETC('GRAPH_BAR_COLOUR','RED')
                         CALL PSETC('GRAPH_SHADE_COLOUR','RED')
                         write(legend,'(a,a,i3,a)')expm(iens)(2:5),
     >                     ' (',nenm(iens),')'
                         CALL PSETR('GRAPH_BAR_WIDTH',0.04)
c                         CALL PSETR('GRAPH_BAR_WIDTH',0.02)
c                        CALL PSETR('GRAPH_BAR_WIDTH',0.20)
                      else
                         if(typm(iens).eq.1)then
                            write(legend,'(a,a,i3,a)')expm(iens)(2:5),
     >                       ' (',nenm(iens),')'
                         else
                            write(legend,'(a,a,a,a,i2,a)')
     >                       expm(iens)(5:8),'-',
     >                       expm(iens)(10:13),' (',nenm(iens),')'
                         endif
                         CALL PSETR('GRAPH_BAR_WIDTH',0.025)
c                         CALL PSETR('GRAPH_BAR_WIDTH',0.016)
c                        CALL PSETR('GRAPH_BAR_WIDTH',0.16)
                        if(nmod.gt.6) CALL PSETR('GRAPH_BAR_WIDTH',0.01)
c                       if(nmod.gt.6) CALL PSETR('GRAPH_BAR_WIDTH',0.10)
                      endif
                      CALL PSETC('LEGEND_USER_TEXT',
     >                   legend(1:lena(legend)))
                      write(*,*)legend
                    else
                      if((im.eq.1).and.(il.eq.1).and.(iens.eq.nensp1))
     >                   CALL PSETC('LEGEND','ON')
                      if(iens.eq.nensp1)then
                         CALL PSETC('GRAPH_BAR_COLOUR','RED')
                         CALL PSETC('GRAPH_SHADE_COLOUR','RED')
                         CALL PSETR('GRAPH_BAR_WIDTH',0.01)
c                         CALL PSETR('GRAPH_BAR_WIDTH',0.002)
c                        CALL PSETR('GRAPH_BAR_WIDTH',0.20)
                         if(typm(1).eq.1)then
                             write(legend,'(a,a,i2,a)')expm(1)(2:5),
     >                       ' (',nenm(1),')'
                         else
                             write(legend,'(a,a,a,a,i2,a)')
     >                       expm(1)(5:8),'-',
     >                       expm(1)(10:13),' (',nenm(1),')'
                         endif
                         CALL PSETC('LEGEND_USER_TEXT',
     >                       legend(1:lena(legend)))
                         write(*,*)legend
                      else
                         CALL PSETC('GRAPH_BAR_COLOUR','TURQUOISE')
                         CALL PSETC('GRAPH_SHADE_COLOUR','TURQUOISE')
                         if(nensp1.le.10)then
                            CALL PSETR('GRAPH_BAR_WIDTH',0.02)
c                            CALL PSETR('GRAPH_BAR_WIDTH',0.004)
c                           CALL PSETR('GRAPH_BAR_WIDTH',0.04)
                         endif
                         if((nensp1.gt.10).and.(nensp1.le.30))then
                            CALL PSETR('GRAPH_BAR_WIDTH',0.0015)
c                           CALL PSETR('GRAPH_BAR_WIDTH',0.015)
                         endif
                         if(nensp1.gt.30)then
                            CALL PSETR('GRAPH_BAR_WIDTH',0.0005)
c                           CALL PSETR('GRAPH_BAR_WIDTH',0.005)
                         endif
                      endif
                    endif
c
c.... abscissa
c
                    x(1)=float((im-1)*nldt+il)-shift+iens*xinc
                    if((iens.eq.nensp1).and.(mult.eq.1))x(1)=
     >                 float((im-1)*nldt+il)-shift+(nensp1+1)*xinc
c
c.... bars with or without confidence intervals
c
                    if(boot.eq.1)then
c                      CALL PSETR('SYMBOL_HEIGHT',0.15)
                       CALL PSETR('SYMBOL_HEIGHT',0.4)
                       do ib=1,nsam
                          xarray(ib)=val(im,il,iens,ib)
                       enddo
                       call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                       write(*,'(a,f6.3,a,f6.3,a)')
     >                   '(',x_bot,',',x_top,')'
                       y1(1)=x_bot
                       y2(1)=x_top
                       CALL PSET1R('GRAPH_BAR_X_VALUES',x,1)
                       CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',y1,1)
                       CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',y2,1)
                       CALL PGRAPH
                       call psetc("legend", "off")
                       y1(1)=score(iens)
                       CALL PSET1R('SYMBOL_INPUT_X_POSITION',x,1)
                       CALL PSET1R('SYMBOL_INPUT_Y_POSITION',y1,1)
                       if(nensp1.le.5)then
c                         CALL PSETR('SYMBOL_HEIGHT',0.20)
                          CALL PSETR('SYMBOL_HEIGHT',0.5)
                       endif
                       if((nensp1.gt.5).and.(nensp1.le.10))then
c                         CALL PSETR('SYMBOL_HEIGHT',0.12)
                          CALL PSETR('SYMBOL_HEIGHT',0.42)
                       endif
                       if((nensp1.gt.10).and.(nensp1.le.30))then
c                         CALL PSETR('SYMBOL_HEIGHT',0.08)
                          CALL PSETR('SYMBOL_HEIGHT',0.38)
                       endif
                       if(nensp1.gt.30)then
c                         CALL PSETR('SYMBOL_HEIGHT',0.05)
                          CALL PSETR('SYMBOL_HEIGHT',0.35)
                       endif
                       CALL PSYMB
                    else
                       if(score(iens).gt.0.)then
                          y1(1)=0.
                          y2(1)=score(iens)
                       else
                          y1(1)=score(iens)
                          y2(1)=0.
                       endif
                       CALL PSET1R('GRAPH_BAR_X_VALUES',x,1)
                       CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',y1,1)
                       CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES',y2,1)
                       CALL PGRAPH
                    endif

                 enddo !iens
              enddo !il
           enddo  !im

c.... confidence intervals
c
           deallocate (x,                                    stat=istat)
           deallocate (y1,                                   stat=istat)
           deallocate (y2,                                   stat=istat)
           deallocate (score,                                stat=istat)
c
           CALL PCLOSE

         endif !rcal=1
      enddo !ir loop
      deallocate (val,                                       stat=istat)
      deallocate (xarray,                                    stat=istat)
c
      goto 999
 998  write(*,*)'Sorry, no success :('
      call abort
 999  write(*,*)'program seems to be successfully finished :)'
c
      end
