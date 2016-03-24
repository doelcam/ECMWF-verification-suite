c
c----------------------------------------------------------------------c
c     PROGRAM cal_acc_reg                 F. Doblas-Reyes, 11-Aug-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     Calculates anomaly correlation coefficient (ACC), perfect-model  c
c     ACC (ACP), ratio between spread (in terms of standard deviation) c
c     and RMSE (SPR), and mean square skill score (MSS) over           c
c     regions for individual ensembles and the ensemble mean.          c
c     It also computes bootstrap estimates.                            c
c                                                                      c
c     INPUT:                                                           c
c     ANOM_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_YYYY GRIB files         c
c                                                                      c
c     OUTPUT:                                                          c
c     DIA_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_REGI ASCII files (reg)   c
c                                                                      c
c     USAGE:                                                           c
c     cal_acc_reg.x < nlist                                            c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 cal_acc_reg.f tools.f -o cal_acc_reg.x             c
c                                                  $EMOSLIB $NAGLIB    c
c                                                                      c
c     MODS:                                                            c
c                                         F. Doblas-Reyes, 02-Feb-2006 c
c     Based on the original cal_acc_con.f from DEMETER                 c
c                                         F. Doblas-Reyes, 26-Jul-2006 c
c     Adapted to the linux machines                                    c
c                                         F. Doblas-Reyes, 16-Feb-2007 c
c     Bootstrap for confidence intervals                               c
c                                         F. Doblas-Reyes, 17-Aug-2007 c
c     Include spread normalized by the interannual standard deviation  c
c     The file named SPR now represent the normalized spread, while    c
c     the ratio spread/RMSE is now referred to as RSR                  c
c                                                                      c
c----------------------------------------------------------------------c
c
      program cal_acc_reg
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable:: ano_mod(:,:,:,:), ano_ref(:,:,:,:)
      real, allocatable:: ref(:), sim(:), weight (:)
      real, allocatable:: r(:,:), rsd(:), spread (:)
      double precision, allocatable:: xran(:,:)
      real, allocatable:: regwgt(:), latwgt(:)
      real, allocatable:: rms_mod(:,:)
      real, allocatable:: rms_ref(:,:)
      real, allocatable:: rms_cov(:,:)
      real, allocatable:: rms_mse(:,:)
      real, allocatable:: rmst_mod(:)
      real, allocatable:: rmst_ref(:)
      real, allocatable:: rmst_cov(:)
      real, allocatable:: rmst_mse(:)
      real, allocatable:: rmsp_mod(:)
      real, allocatable:: rmsp_ref(:)
      real, allocatable:: rmsp_cov(:)
      real, allocatable:: acc(:)
      real, allocatable:: mss(:)
      real, allocatable:: acp(:)
      real, allocatable:: x(:), y(:)
      real, allocatable:: h(:,:)
      real, allocatable:: rlat(:), rlon(:)
      integer, allocatable :: ivfdate(:,:), istep(:,:)
      integer, allocatable:: lsm(:)
      integer, allocatable:: mask(:)
c
c.... GRIB headers
c
      integer ksec0(2), kret
      integer ksec1(1024)
      integer ksec1_sav(1024)
      integer ksec2(1024)
      integer ksec3(2)
      integer ksec4(512)
c
      real psec2(512)
      real psec3(2)
c
c.... other definitions
c
      character yifile*75, yofile*90, yrfile*90
      character*5 form1
      character*30 form2, form3
      character*21 expt, expti
      real topth, botth
      real x_top, x_bot
      real xlats, xlatn, xlonw, xlone
      real sdx, sdy, cov
      real sdxs, sdys, covs
      real spr, mse, spm, rmst_spr, rmst_rmse, norm_spr
      real yhlp, xhlp
      real eps, totwgt
      real ave, adev, sdev, var, skew, curt
      real rmiss
      real rint
      double precision epsdd, cor
      integer nx, ny, nmon, nens
      integer iyy1, iyy2, imm, idd, itt
      integer ipar, ilev, anin, nf1, nf2, nreg, cros, idatetype
      integer boot, nsam
      integer ntop, nbot
      integer lena, istat, klenp, exptl, l, ib
      integer nxny, nensp1, nensm1, nyear, idat
      integer i, nummon, num1, num2
      integer imon, iens, jens, ie, ir, iy, ik
      integer iunit
      integer iilsm, icatch
      integer nlon, nlat
      
      
c.... MAKING IT WORK WITH NAG Mk22 - EXPERIMENTAL!      
      external g05saf,g05kff
      integer MSTATE,MSEED,LSEED,LSTATE,IFAIL
      parameter (MSTATE=633,MSEED=1)
      integer STATE(MSTATE),SEED(MSEED)
      parameter (SEED=1762543,LSTATE=MSTATE,LSEED = MSEED)
      
c.... hard coded field definitions
c
      integer nreg_max
      parameter(nreg_max=48)
      character*4 namr(nreg_max)
      real limn(nreg_max), lims(nreg_max) 
      real limw(nreg_max), lime(nreg_max)
      integer ilsm(nreg_max), rcal(nreg_max)
c
      namelist /control/   nx,   ny, nmon, nens,
     >                   iyy1, iyy2,  imm,  idd,  itt,
     >                   cros, ipar, ilev, anin,  nf1,  nf2,
     >                   expt, boot, nsam,topth,botth, nreg
      namelist /region/  namr, limn, lims, limw, lime, ilsm, rcal
c
c.... set default values and read input namelists
c
      nx = 144
      ny = 71
      nmon = 6
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
      nreg = 12
c
      read(5,control)
      write(*,*)'  nx: ', nx
      write(*,*)'  ny: ', ny
      write(*,*)'nmon: ',nmon
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
      write(*,*)'nreg: ',nreg
c
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
      rmiss=-9999
      nensm1 = nens-1
      nensp1 = nens+1
      nxny=nx*ny
      klenp=nxny
      nyear=iyy2-iyy1+1
      nummon=nf2-nf1+1
c
c
c.... allocate fields
c
      allocate (ano_mod(nxny,nmon,nens,nyear),               stat=istat)
      allocate (ano_ref(nxny,nmon,1,nyear),                  stat=istat)
      allocate (ivfdate(nmon,nyear),                         stat=istat)
      allocate (istep(nmon,nyear),                           stat=istat)
      allocate (regwgt(nxny),                                stat=istat)
      allocate (latwgt(nxny),                                stat=istat)
      allocate (rlon(nxny),                                  stat=istat)
      allocate (rlat(nxny),                                  stat=istat)
      allocate (lsm(nxny),                                   stat=istat)
      allocate (mask(nxny),                                  stat=istat)
      allocate (rms_mod(nyear,nensp1),                       stat=istat)
      allocate (rms_ref(nyear,nensp1),                       stat=istat)
      allocate (rms_cov(nyear,nensp1),                       stat=istat)
      allocate (rms_mse(nyear,nensp1),                       stat=istat)
      allocate (rmst_mod(nensp1),                            stat=istat)
      allocate (rmst_ref(nensp1),                            stat=istat)
      allocate (rmst_cov(nensp1),                            stat=istat)
      allocate (rmst_mse(nensp1),                            stat=istat)
      allocate (rmsp_mod(nensp1),                            stat=istat)
      allocate (rmsp_ref(nensp1),                            stat=istat)
      allocate (rmsp_cov(nensp1),                            stat=istat)
      allocate (acc(nensp1),                                 stat=istat)
      allocate (mss(nensp1),                                 stat=istat)
      allocate (acp(nensp1),                                 stat=istat)
c
c.... reset fields
c
      eps      = 1.e-30
      epsdd    = tiny(epsdd)
      write(*,*)'Smallest value ',epsdd
c
      do i=1,nxny
         mask(i)=0
      enddo
c
c.... loop over reading model and reference data
c....    idat=1 for model
c....    idat=2 for reference
c
      do idat=1,2
         expti=expt
         if(idat.eq.2)then
           expti = 'refe'
         endif
         exptl=lena(expti)
         write(form1,'(a,i2.2,a)')'(a',exptl,')'
c
c.... read all years of anomalies
c
         do iy=1,nyear
            yifile='ANOM_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_YYYY'
            write(yifile(       6: 5+exptl),form1)expti(1:exptl)
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
            if(idat.eq.2)then
              write(yifile(30+exptl:30+exptl),'(i1.1)')0
            else
              write(yifile(30+exptl:30+exptl),'(i1.1)')anin
            endif
            write(yifile(31+exptl:31+exptl),'(a1)')'_'
            write(yifile(32+exptl:34+exptl),'(i3.3)')ipar
            write(yifile(35+exptl:35+exptl),'(a1)')'_'
            write(yifile(36+exptl:38+exptl),'(i3.3)')ilev
            write(yifile(39+exptl:39+exptl),'(a1)')'_'
            write(yifile(40+exptl:43+exptl),'(i4.4)')iyy1+iy-1
            write(*,*)'open input file: ',yifile
            kret=0
            call pbopen(iunit,yifile,'r',kret)
            if(kret.ne.0)then
              write(*,*)'Error in opening file: kret=',kret
              goto 997
            endif
c
c.... read input data and close file
c
            if(idat.eq.1)then
              idatetype=0
              call readgrb(iunit, nxny, nmon, nens, idat,
     >           imm, iyy1, iy, ipar, ilev, idatetype,
     >           ano_mod(1,1,1,iy), ivfdate(1,iy), istep(1,iy),
     >           ksec0, ksec1_sav, ksec2, ksec3, ksec4, psec2, psec3)
            endif
            if(idat.eq.2)then
              idatetype=2
              call readgrb(iunit, nxny, nmon, 1, idat,
     >           imm, iyy1, iy, ipar, ilev, idatetype,
     >           ano_ref(1,1,1,iy), ivfdate(1,iy), istep(1,iy),
     >           ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
            endif
            call pbclose(iunit,kret)
            if(kret.ne.0)then
              write(*,*)'Error in closing file: kret=',kret
              goto 997
            endif
            do i=1,nxny
               do imon=nf1,nf2
                  if(idat.eq.2)then
                    if(ano_ref(i,imon,1,iy).eq.rmiss)mask(i)=1
                  else
                    do iens=1,nens
                       if(ano_mod(i,imon,iens,iy).eq.rmiss)mask(i)=1
                    enddo
                  endif
               enddo
c	       write(*,*) mask(i)
            enddo
         enddo !iy loop
      enddo !idat loop
c
c

c Bootsrapping - random number generators...modified by DM
      IFAIL = 0
      call g05kff(1,1,SEED,LSEED,STATE,LSTATE,IFAIL)
      write(*,*) IFAIL

      if(boot.eq.1)then
        allocate (xran(nsam,nyear),                          stat=istat)
        ntop=int(topth*nsam/100)+1
        nbot=int(botth*nsam/100)
        if(nbot.eq.0)nbot=1
        write(*,'(a,i5,a)')'Bootstrap with ',nsam,' samples'
        do ib=1,nsam
           do iy=1,nyear
	      IFAIL=0      
              call g05saf(1,STATE,xran(ib,iy),IFAIL)
c              xran(ib,iy)=1
           enddo
        enddo
      endif
c
c.... root name for output files
c
      exptl=lena(expt)
      write(form1,'(a,i2.2,a)')'(a',exptl,')'
      yofile='ACC_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE'
      write(yofile(       5: 4+exptl),form1)expt(1:exptl)
      write(yofile( 5+exptl: 5+exptl),'(a1)')'_'
      write(yofile( 6+exptl: 9+exptl),'(i4.4)')iyy1
      write(yofile(10+exptl:10+exptl),'(a1)')'-'
      write(yofile(11+exptl:14+exptl),'(i4.4)')iyy2
      write(yofile(15+exptl:15+exptl),'(a1)')'_'
      write(yofile(16+exptl:17+exptl),'(i2.2)')imm
      write(yofile(18+exptl:19+exptl),'(i2.2)')idd
      write(yofile(20+exptl:21+exptl),'(i2.2)')itt
      write(yofile(22+exptl:24+exptl),'(a3)')'_CV'
      write(yofile(25+exptl:26+exptl),'(i2.2)')cros
      write(yofile(27+exptl:28+exptl),'(a2)')'_I'
      write(yofile(29+exptl:29+exptl),'(i1.1)')anin
      write(yofile(30+exptl:30+exptl),'(a1)')'_'
      write(yofile(31+exptl:33+exptl),'(i3.3)')ipar
      write(yofile(34+exptl:34+exptl),'(a1)')'_'
      write(yofile(35+exptl:37+exptl),'(i3.3)')ilev
      write(yofile(38+exptl:38+exptl),'(a1)')'_'
      write(yofile(39+exptl:41+exptl),'(i3.3)')nf1
      write(yofile(42+exptl:42+exptl),'(a1)')'-'
      write(yofile(43+exptl:45+exptl),'(i3.3)')nf2
c
c.... loop over regions
c
      write(form2,'(a,i2.2,a)')'(i8,',nensp1,'f10.4)'
      if(nensp1.ge.100)write(form2,'(a,i3.3,a)')'(i8,',nensp1,'f10.4)'
      write(form3,'(a)')'(i8,f10.4)'
      write(*,*)'Output format ',form2
      do ir=1,nreg
         if(rcal(ir).eq.1)then
c
c.... select parameters for the region
c
           write(*,*)'Computing for region: ',namr(ir)
           iilsm=ilsm(ir)
           xlats=lims(ir)
           xlatn=limn(ir)
           xlonw=limw(ir)
           xlone=lime(ir)
           icatch = 0
           call get_mask(iilsm,lsm,nxny,expt)
c          write(*,*)'lsm'
c          write(*,'(40i4)')(lsm(i),i=1,nxny)
c          write(*,*) xlats, xlatn, xlonw, xlone
           call get_region(icatch, xlats, xlatn, xlonw, xlone,
     >        nxny, expt, '/vol/demeter/verify', regwgt, latwgt,
     >        rlat, rlon, nlat, nlon, rint)
c          write(*,*)'regwgt'
c          write(*,'(40f4.1)')(regwgt(i),i=1,nxny)
c          write(*,*)'latwgt'
c          write(*,'(20f8.3)')(latwgt(i),i=1,nxny)
           num1=0
           do i=1,klenp
              if((regwgt(i).gt.0.).and.(lsm(i).eq.1).and.(mask(i).eq.0))
     >           num1=num1+1
           enddo
c
c.... open output file for yearly data of regional averaged rmsss
c
           write(yofile(46+exptl:46+exptl),'(a1)')'_'
           write(yofile(47+exptl:50+exptl),'(a4)')namr(ir)
           do i=1,5
              if(i.eq.1)write(yofile(1:3),'(a3)')'ACC'
              if(i.eq.2)write(yofile(1:3),'(a3)')'MSS'
              if(i.eq.3)write(yofile(1:3),'(a3)')'ACP'
              if(i.eq.4)write(yofile(1:3),'(a3)')'RSR'
              if(i.eq.5)write(yofile(1:3),'(a3)')'SPR'
              write(*,*)'open output file: ',yofile
              open(9+i,file=yofile,form='formatted',status='unknown')
              write(9+i,'(2i8)')nf1,nf2
           enddo
           if(boot.eq.1)then
             yrfile=yofile
             write(yrfile(51+exptl:52+exptl),'(a2)')'_S'
             write(yrfile(53+exptl:57+exptl),'(i5.5)')nsam
             yrfile=yrfile(1:lena(yrfile))//'.ran'
             do i=1,5
                if(i.eq.1)write(yrfile(1:3),'(a3)')'ACC'
                if(i.eq.2)write(yrfile(1:3),'(a3)')'MSS'
                if(i.eq.3)write(yrfile(1:3),'(a3)')'ACP'
                if(i.eq.4)write(yrfile(1:3),'(a3)')'RSR'
                if(i.eq.5)write(yrfile(1:3),'(a3)')'SPR'
                write(*,*)'open output file: ',yrfile
                open(19+i,file=yrfile,form='formatted',status='unknown')
                write(19+i,'(i5)')nsam
             enddo
           endif
c
c.... allocations
c
           allocate (r(num1,nyear),                          stat=istat)
           allocate (rsd(num1),                              stat=istat)
           allocate (ref(num1),                              stat=istat)
           allocate (sim(num1),                              stat=istat)
           allocate (weight(num1),                           stat=istat)
           allocate (spread(num1),                           stat=istat)
c
c.... precompute reference standard deviation
c
           do iy=1,nyear
              num2=0
              do i=1,klenp
                 if((regwgt(i).gt.0.).and.(lsm(i).eq.1).and.
     >              (mask(i).eq.0))then
                   num2=num2+1
                   yhlp=0.
                   do imon=nf1,nf2
                      yhlp=yhlp+ano_ref(i,imon,1,iy)
                   enddo !imon
                   r(num2,iy)=yhlp/float(nummon)
                 endif
              enddo !i
              if(num1.ne.num2)then
                write(*,*)'Number of points',num1,num2
                stop
              endif
           enddo !iy
           allocate (y(nyear),                               stat=istat)
           do i=1,num2
              do iy=1,nyear
                 y(iy)=r(i,iy)
              enddo
              call moment(y, nyear, ave, adev, sdev, var, skew, curt)
              rsd(i)=sdev
           enddo !i
           deallocate (y,                                    stat=istat)
c
c.... computation for single members and the ensemble mean
c
           do iens=1,nensp1
              rmst_mod(iens)=0.
              rmst_ref(iens)=0.
              rmst_cov(iens)=0.
              rmst_mse(iens)=0.
              rmsp_mod(iens)=0.
              rmsp_ref(iens)=0.
              rmsp_cov(iens)=0.
           enddo
           rmst_spr=0.
           rmst_rmse=0.
           do i=1,num1
              spread(i)=0.
           enddo !i
           do iy=1,nyear
              do iens=1,nensp1
c
c.... accumulate squared error over months and region for single 
c.... ensemble members and for the ensemble mean (nensp1)
c
                 totwgt=0.
                 num2=0
                 do i=1,klenp
                    if((regwgt(i).gt.0.).and.(lsm(i).eq.1).and.
     >                 (mask(i).eq.0))then
                      num2=num2+1
                      xhlp=0.
                      yhlp=0.
                      do imon=nf1,nf2
                         if(iens.le.nens)then
                           xhlp=xhlp+ano_mod(i,imon,iens,iy)
                         else
                           do ie=1,nens
                              xhlp=xhlp+ano_mod(i,imon,ie,iy)/nens
                           enddo
                         endif
                         yhlp=yhlp+ano_ref(i,imon,1,iy)
                      enddo !imon
                      sim(num2)=xhlp/float(nummon)
                      ref(num2)=yhlp/float(nummon)
                      weight(num2)=lsm(i)*latwgt(i)
                      totwgt=totwgt+weight(num2)
                    endif
                 enddo !i
                 if(num1.ne.num2)then
                   write(*,*)'Number of points',num1,num2
                   stop
                 endif
                 call varcovar(ref, sim, num2, weight, 0, sdx, sdy, cov)
                 rms_mod(iy,iens)=sdy
                 rms_ref(iy,iens)=sdx
                 rms_cov(iy,iens)=cov
                 rmst_mod(iens)=rmst_mod(iens)+rms_mod(iy,iens)**2
                 rmst_ref(iens)=rmst_ref(iens)+rms_ref(iy,iens)**2
                 rmst_cov(iens)=rmst_cov(iens)+rms_cov(iy,iens)
                 allocate (x(num2),                          stat=istat)
                 if((iens.eq.1).and.(iy.eq.1))then
                   allocate (h(num2,nens),                   stat=istat)
                 endif
                 do i=1,num2
                    x(i)=sim(i)-ref(i)
                    if(iens.le.nens)h(i,iens)=sim(i)
                 enddo !i
                 call varcovar(ref, x, num2, weight, 0, sdx, sdy, cov)
                 rms_mse(iy,iens)=sdy
                 rmst_mse(iens)=rmst_mse(iens)+rms_mse(iy,iens)**2
                 mss(iens)=1.-sdy**2/sdx**2
                 deallocate (x,                              stat=istat)
c
c.... calculate ACC
c
                 acc(iens)=rms_cov(iy,iens)/
     >              (rms_ref(iy,iens)*rms_mod(iy,iens)+eps)
              enddo !iens
c
c.... ACP
c
              allocate (x(num2),                             stat=istat)
              allocate (y(num2),                             stat=istat)
              do iens=1,nens
                 do i=1,num2
                    y(i)=0.
                    do jens=1,nens
                       if(iens.ne.jens)then
                         y(i)=y(i)+h(i,jens)/(nens-1)
                       else
                         x(i)=h(i,iens)
                       endif
                    enddo !jens loop
                 enddo !i loop
                 call varcovar(x, y, num2, weight, 1, sdx, sdy, cov)
                 acp(iens)=cov/(sdx*sdy+eps)
                 rms_mod(iy,iens)=sdy
                 rms_ref(iy,iens)=sdx
                 rms_cov(iy,iens)=cov
                 rmsp_mod(iens)=rmsp_mod(iens)+sdy**2
                 rmsp_ref(iens)=rmsp_ref(iens)+sdx**2
                 rmsp_cov(iens)=rmsp_cov(iens)+cov
              enddo !iens loop
              sdx=0.
              sdy=0.
              cov=0.
              do iens=1,nens
                 sdy=sdy+rms_mod(iy,iens)**2
                 sdx=sdx+rms_ref(iy,iens)**2
                 cov=cov+rms_cov(iy,iens)
              enddo
              sdx=sqrt(sdx)
              sdy=sqrt(sdy)
              acp(nensp1)=cov/(sdx*sdy+eps)
              rmsp_mod(nensp1)=rmsp_mod(nensp1)+sdy**2
              rmsp_ref(nensp1)=rmsp_ref(nensp1)+sdx**2
              rmsp_cov(nensp1)=rmsp_cov(nensp1)+cov
c
c.... write results to ascii file
c
              write(10,form2)iyy1+iy-1,(acc(iens),iens=1,nensp1) 
              write(11,form2)iyy1+iy-1,(mss(iens),iens=1,nensp1) 
              write(12,form2)iyy1+iy-1,(acp(iens),iens=1,nensp1) 
              deallocate (x,                                 stat=istat)
              deallocate (y,                                 stat=istat)
c
c.... SPR
c
              allocate (x(num2),                             stat=istat)
              allocate (y(nens),                             stat=istat)
              spr=0.
              norm_spr=0.
              do i=1,num2
                 sim(i)=0.
                 do iens=1,nens
                    y(iens)=h(i,iens)
                    sim(i)=sim(i)+h(i,iens)/nens
                 enddo
                 call moment(y, nens, ave, adev, sdev, var, skew, curt)
                 spr=spr+weight(i)*var
                 norm_spr=norm_spr+weight(i)*sdev/(rsd(i)+eps)
                 spread(i)=spread(i)+var/nyear
                 x(i)=sim(i)-ref(i)
              enddo !i
              spr=spr/totwgt
              rmst_spr=rmst_spr+spr
              norm_spr=norm_spr/totwgt
              call varcovar(ref, x, num2, weight, 0, sdx, sdy, cov)
              rmst_rmse=rmst_rmse+sdy**2
              write(13,form3)iyy1+iy-1,sqrt(spr/(sdy**2+eps))
              write(14,form3)iyy1+iy-1,norm_spr
              deallocate (x,                                 stat=istat)
              deallocate (y,                                 stat=istat)
           enddo !iy loop
           deallocate (h,                                    stat=istat)
c
c... calc time average scores and write to output file
c
           do iens=1,nensp1
              acc(iens)=rmst_cov(iens)/
     >           sqrt(rmst_ref(iens)*rmst_mod(iens)+eps)
              mss(iens)=1.-rmst_mse(iens)/rmst_ref(iens)
              acp(iens)=rmsp_cov(iens)/
     >           sqrt(rmsp_ref(iens)*rmsp_mod(iens)+eps)
              spm=sqrt(rmst_spr/(rmst_rmse+eps))
           enddo
           norm_spr=0.
           do i=1,num2
              norm_spr=norm_spr+
     >           (weight(i)*sqrt(spread(i))/(rsd(i)+eps))
           enddo
           norm_spr=norm_spr/totwgt
           write(10,form2)9999,(acc(iens),iens=1,nensp1)
           write(11,form2)9999,(mss(iens),iens=1,nensp1)
           write(12,form2)9999,(acp(iens),iens=1,nensp1)
           write(13,form3)9999,spm
           write(14,form3)9999,norm_spr
           close(10)
           close(11)
           close(12)
           close(13)
           close(14)
c
c.... computations for bootstrap
c
           if(boot.eq.1)then
             do ib=1,nsam
                write(*,*)'Sample ',ib
                do iens=1,nensp1
                   rmst_mod(iens)=0.
                   rmst_ref(iens)=0.
                   rmst_cov(iens)=0.
                   rmst_mse(iens)=0.
                   rmsp_mod(iens)=0.
                   rmsp_ref(iens)=0.
                   rmsp_cov(iens)=0.
                enddo
                rmst_spr=0.
                rmst_rmse=0.
                do i=1,num1
                   spread(i)=0.
                enddo !i
                do iy=1,nyear
                   l=int(nyear*xran(ib,iy))+1
                   do iens=1,nensp1
                      totwgt=0.
                      num2=0
                      do i=1,klenp
                         if((regwgt(i).gt.0.).and.(lsm(i).eq.1).and.
     >                      (mask(i).eq.0))then
                           num2=num2+1
                           xhlp=0.
                           yhlp=0.
                           do imon=nf1,nf2
                              if(iens.le.nens)then
                                xhlp=xhlp+ano_mod(i,imon,iens,l)
                              else
                                do ie=1,nens
                                   xhlp=xhlp+ano_mod(i,imon,ie,l)/nens
                                enddo
                              endif
                              yhlp=yhlp+ano_ref(i,imon,1,l)
                           enddo !imon
                           sim(num2)=xhlp/float(nummon)
                           ref(num2)=yhlp/float(nummon)
                           weight(num2)=lsm(i)*latwgt(i)
                           totwgt=totwgt+weight(num2)
                         endif
                      enddo !i
                      call varcovar(ref, sim, num1, weight, 0,
     >                   sdx, sdy, cov)
                      rms_mod(iy,iens)=sdy
                      rms_ref(iy,iens)=sdx
                      rms_cov(iy,iens)=cov
                      rmst_mod(iens)=rmst_mod(iens)+rms_mod(iy,iens)**2
                      rmst_ref(iens)=rmst_ref(iens)+rms_ref(iy,iens)**2
                      rmst_cov(iens)=rmst_cov(iens)+rms_cov(iy,iens)
                      allocate (x(num2),                     stat=istat)
                      if((iens.eq.1).and.(iy.eq.1))then
                        allocate (h(num2,nens),              stat=istat)
                      endif
                      do i=1,num2
                         x(i)=sim(i)-ref(i)
                         if(iens.le.nens)h(i,iens)=sim(i)
                      enddo !i
                      call varcovar(ref, x, num2, weight, 0, sdx, sdy,
     >                   cov)
                      rms_mse(iy,iens)=sdy
                      rmst_mse(iens)=rmst_mse(iens)+rms_mse(iy,iens)**2
                      deallocate (x,                         stat=istat)
                   enddo !iens
c
                   allocate (x(num2),                        stat=istat)
                   allocate (y(num2),                        stat=istat)
                   do iens=1,nens
                      do i=1,num2
                         y(i)=0.
                         do jens=1,nens
                            if(iens.ne.jens)then
                              y(i)=y(i)+h(i,jens)/(nens-1)
                            else
                              x(i)=h(i,iens)
                            endif
                         enddo !jens loop
                      enddo !i loop
                      call varcovar(x, y, num2, weight, 1,
     >                   sdx, sdy, cov)
                      rmsp_mod(iens)=rmsp_mod(iens)+sdy**2
                      rmsp_ref(iens)=rmsp_ref(iens)+sdx**2
                      rmsp_cov(iens)=rmsp_cov(iens)+cov
                   enddo !iens loop
                   sdx=0.
                   sdy=0.
                   cov=0.
                   do iens=1,nens
                      sdy=sdy+rmsp_mod(iens)
                      sdx=sdx+rmsp_ref(iens)
                      cov=cov+rmsp_cov(iens)
                   enddo
                   rmsp_mod(nensp1)=rmsp_mod(nensp1)+sdy
                   rmsp_ref(nensp1)=rmsp_ref(nensp1)+sdx
                   rmsp_cov(nensp1)=rmsp_cov(nensp1)+cov
c
                   deallocate (x,                            stat=istat)
                   deallocate (y,                            stat=istat)
                   allocate (x(num2),                        stat=istat)
                   allocate (y(nens),                        stat=istat)
                   spr=0.
                   norm_spr=0.
                   do i=1,num2
                      sim(i)=0.
                      do iens=1,nens
                         y(iens)=h(i,iens)
                         sim(i)=sim(i)+h(i,iens)/nens
                      enddo
                      call moment(y, nens, ave, adev, sdev,
     >                   var, skew, curt)
                      spr=spr+weight(i)*var
                      spread(i)=spread(i)+var/nyear
                      x(i)=sim(i)-ref(i)
                   enddo !i
                   spr=spr/totwgt
                   rmst_spr=rmst_spr+spr
                   call varcovar(ref, x, num2, weight, 0, sdx, sdy, cov)
                   rmst_rmse=rmst_rmse+sdy**2
                   deallocate (x,                            stat=istat)
                   deallocate (y,                            stat=istat)
                enddo !iy loop
                deallocate (h,                               stat=istat)
c
c... calc time average scores and write to output file
c
                do iens=1,nensp1
                   acc(iens)=rmst_cov(iens)/
     >                sqrt(rmst_ref(iens)*rmst_mod(iens)+eps)
                   mss(iens)=1.-rmst_mse(iens)/rmst_ref(iens)
                   acp(iens)=rmsp_cov(iens)/
     >                sqrt(rmsp_ref(iens)*rmsp_mod(iens)+eps)
                   spm=sqrt(rmst_spr/(rmst_rmse+eps))
                enddo
                norm_spr=0.
                do i=1,num2
                   norm_spr=norm_spr+
     >               (weight(i)*sqrt(spread(i))/(rsd(i)+eps))
                enddo
                norm_spr=norm_spr/totwgt
c
                write(20,form2)9999,(acc(iens),iens=1,nensp1)
                write(21,form2)9999,(mss(iens),iens=1,nensp1)
                write(22,form2)9999,(acp(iens),iens=1,nensp1)
                write(23,form3)9999,spm
                write(24,form3)9999,norm_spr
             enddo !ib loop
             close(20)
             close(21)
             close(22)
             close(23)
             close(24)
           endif ! boot=1
c
           deallocate (r,                                    stat=istat)
           deallocate (rsd,                                  stat=istat)
           deallocate (ref,                                  stat=istat)
           deallocate (sim,                                  stat=istat)
           deallocate (weight,                               stat=istat)
           deallocate (spread,                               stat=istat)
         endif
      enddo
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
c
