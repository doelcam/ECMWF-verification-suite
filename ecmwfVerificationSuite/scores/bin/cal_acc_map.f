c
c----------------------------------------------------------------------c
c     PROGRAM cal_acc_map                 F. Doblas-Reyes, 11-Aug-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     Calculates anomaly correlation coefficient (ACC), perfect-model  c
c     ACC (ACP), ratio between spread (in terms of standard deviation) c
c     and RMSE (SPR), and mean square skill score (MSS) at grid        c
c     points.                                                          c
c     It also computes bootstrap estimates.                            c
c                                                                      c
c     INPUT:                                                           c
c     ANOM_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_YYYY GRIB files         c
c                                                                      c
c     OUTPUT:                                                          c
c     DIA_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV GRIB files (global map)  c 
c                                                                      c
c     USAGE:                                                           c
c     cal_acc_map.x < nlist                                            c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 cal_acc_map.f tools.f -o cal_acc_map.x             c
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
      program cal_acc_map
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable:: psec4_acc(:), psec4_acp(:)
      real, allocatable:: psec4_mss(:), psec4_spr(:), psec4_rsr(:)
      real, allocatable:: ano_mod(:,:,:,:), ano_ref(:,:,:,:)
      real, allocatable:: ref(:), reo(:), sim(:), sie(:,:), weight (:)
      real, allocatable:: reo_ran(:), sie_ran(:,:), sim_ran(:)
      real, allocatable:: acc_ran(:), mss_ran(:), spr_ran(:), acp_ran(:)
      real, allocatable:: rsr_ran(:)
      real, allocatable:: acc_sig(:), mss_sig(:), spr_sig(:), acp_sig(:)
      real, allocatable:: rsr_sig(:)
      double precision, allocatable:: xran(:,:)
      real, allocatable:: xarray(:)
      real, allocatable:: x(:)
      real, allocatable:: h(:,:)
      integer, allocatable :: ivfdate(:,:), istep(:,:)
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
      character*21 expt, expti
      real topth, botth
      real x_top, x_bot
      real sdx, sdy, cov
      real sdxs, sdys, covs
      real spr, mse, spm
      real eps, rmiss
      real ave, adev, sdev, var, skew, curt
      double precision epsdd, cor
      integer nx, ny, nmon, nens
      integer iyy1, iyy2, imm, idd, itt
      integer ipar, ilev, anin, nf1, nf2, cros, idatetype
      integer boot, nsam
      integer ntop, nbot
      integer lena, istat, klenp, exptl, l, ib
      integer nxny, nensp1, nensm1, nyear, idat
      integer i, nummon
      integer imon, iens, jens, ie, ir, iy, ik
      integer iunit, ounit1, ounit2, ounit3, ounit4, ounit5
      integer ounit11, ounit12, ounit13, ounit14, ounit15
      integer GRIB_GRIBEX_MODE_ON
c
c.... MAKING IT WORK WITH NAG Mk22 - EXPERIMENTAL!      
      external g05saf,g05kff
      integer MSTATE,MSEED,LSEED,LSTATE,IFAIL
      parameter (MSTATE=633,MSEED=1)
      integer STATE(MSTATE),SEED(MSEED)
      parameter (SEED=1762543,LSTATE=MSTATE,LSEED=MSEED)


      namelist /control/   nx,   ny, nmon, nens,
     >                   iyy1, iyy2,  imm,  idd,  itt,
     >                   cros, ipar, ilev, anin,  nf1,  nf2,
     >                   expt, boot, nsam,topth,botth
c
c.... set default values and read input namelists
c

      GRIB_GRIBEX_MODE_ON=1
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
c
      nensm1 = nens-1
      nensp1 = nens+1
      nxny=nx*ny
      klenp=nxny
      nyear=iyy2-iyy1+1
      nummon=nf2-nf1+1
      rmiss=-9999
c
c
c.... allocate fields
c
      allocate (psec4_acc(klenp),                            stat=istat)
      allocate (psec4_acp(klenp),                            stat=istat)
      allocate (psec4_mss(klenp),                            stat=istat)
      allocate (psec4_spr(klenp),                            stat=istat)
      allocate (psec4_rsr(klenp),                            stat=istat)
      allocate (ano_mod(nxny,nmon,nens,nyear),               stat=istat)
      allocate (ano_ref(nxny,nmon,1,nyear),                  stat=istat)
      allocate (ivfdate(nmon,nyear),                         stat=istat)
      allocate (istep(nmon,nyear),                           stat=istat)
      allocate (mask(nxny),                                  stat=istat)
c
c.... reset fields
c
      eps      = 1.e-20
      epsdd    = tiny(epsdd)
      write(*,*)'Smallest value ',epsdd
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
         enddo !iy loop
      enddo !idat loop
c
c.... calculate acc at every grid point (average of variances/covariances
c.... over nyear before computing ACC)
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
c.... open output files for global maps
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
      yrfile=yofile
      yofile=yofile(1:lena(yofile))//'.grb'
      write(*,*)'open output file: ',yofile
      kret=0
      call pbopen(ounit1,yofile,'w',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
      if(boot.eq.1)then
        write(yrfile(46+exptl:47+exptl),'(a2)')'_S'
        write(yrfile(48+exptl:52+exptl),'(i5.5)')nsam
        yrfile=yrfile(1:lena(yrfile))//'.grb'
        write(*,*)'open output file: ',yrfile(1:lena(yrfile))
        kret=0
        call pbopen(ounit11,yrfile,'w',kret)
        if(kret.ne.0)then
          write(*,*)'Error in opening file: kret=',kret
          goto 997
        endif
      endif
c
      write(yofile(       3:       3),'(a1)')'P'
      write(*,*)'open output file: ',yofile
      kret=0
      call pbopen(ounit2,yofile,'w',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
c     if(boot.eq.1)then
c       write(yrfile(       3:       3),'(a1)')'P'
c       write(*,*)'open output file: ',yrfile(1:lena(yrfile))
c       kret=0
c       call pbopen(ounit12,yrfile,'w',kret)
c       if(kret.ne.0)then
c         write(*,*)'Error in opening file: kret=',kret
c         goto 997
c       endif
c     endif
c
      write(yofile(       1:       3),'(a3)')'SPR'
      write(*,*)'open output file: ',yofile
      kret=0
      call pbopen(ounit3,yofile,'w',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
c     if(boot.eq.1)then
c       write(yrfile(       1:       3),'(a3)')'SPR'
c       write(*,*)'open output file: ',yrfile(1:lena(yrfile))
c       kret=0
c       call pbopen(ounit13,yrfile,'w',kret)
c       if(kret.ne.0)then
c         write(*,*)'Error in opening file: kret=',kret
c         goto 997
c       endif
c     endif
c
      write(yofile(       1:       3),'(a3)')'MSS'
      write(*,*)'open output file: ',yofile
      call pbopen(ounit4,yofile,'w',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
      if(boot.eq.1)then
        write(yrfile(       1:       3),'(a3)')'MSS'
        write(*,*)'open output file: ',yrfile(1:lena(yrfile))
        kret=0
        call pbopen(ounit14,yrfile,'w',kret)
        if(kret.ne.0)then
          write(*,*)'Error in opening file: kret=',kret
          goto 997
        endif
      endif
c
      write(yofile(       1:       3),'(a3)')'RSR'
      write(*,*)'open output file: ',yofile
      kret=0
      call pbopen(ounit5,yofile,'w',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
      if(boot.eq.1)then
        write(yrfile(       1:       3),'(a3)')'RSR'
        write(*,*)'open output file: ',yrfile(1:lena(yrfile))
        kret=0
        call pbopen(ounit15,yrfile,'w',kret)
        if(kret.ne.0)then
          write(*,*)'Error in opening file: kret=',kret
          goto 997
        endif
      endif
c
      allocate (ref(nyear),                                  stat=istat)
      allocate (reo(nyear),                                  stat=istat)
      allocate (sie(nens,nyear),                             stat=istat)
      allocate (sim(nyear),                                  stat=istat)
      allocate (weight(nyear),                               stat=istat)
      allocate (reo_ran(nyear),                              stat=istat)
      allocate (sie_ran(nens,nyear),                         stat=istat)
      allocate (sim_ran(nyear),                              stat=istat)
      allocate (acc_ran(nsam),                               stat=istat)
      allocate (mss_ran(nsam),                               stat=istat)
      allocate (spr_ran(nsam),                               stat=istat)
      allocate (rsr_ran(nsam),                               stat=istat)
      allocate (acp_ran(nsam),                               stat=istat)
      allocate (xarray(nsam),                                stat=istat)
      allocate (acc_sig(nxny),                               stat=istat)
      allocate (mss_sig(nxny),                               stat=istat)
      allocate (spr_sig(nxny),                               stat=istat)
      allocate (rsr_sig(nxny),                               stat=istat)
      allocate (acp_sig(nxny),                               stat=istat)
c
c.... create mask for missing data
c.... points for which one reference or model value
c.... are missing are not taken into account in the
c.... computations
c
      do i=1,klenp
         mask(i)=0
         do iy=1,nyear
            do imon=nf1,nf2
c	       write(*,*) ano_ref(i,imon,1,iy)
               if(ano_ref(i,imon,1,iy).eq.rmiss)mask(i)=1
               do iens=1,nens
                  if(ano_mod(i,imon,iens,iy).eq.rmiss)mask(i)=1
               enddo
            enddo
         enddo
        write(*,'(a,i5,a,i1)')'mask(',i,')=',mask(i)
      enddo
      
      
      
c
c.... compute the scores for the target period
c
      do i=1,klenp
         if(mask(i).eq.0)then
c          write(*,*)'Point with data ',i
c
c.... compute the ensemble-mean correlation
c.... do not substract the climatology of the anomalies
c.... but still compute it to test
c
           do iy=1,nyear
              reo(iy)=0.
              do iens=1,nens
                 sie(iens,iy)=0.
              enddo
              weight(iy)=1.
              do imon=nf1,nf2
                 reo(iy)=reo(iy)+ano_ref(i,imon,1,iy)
c                if(i.eq.1000)
c    >              write(*,*)iy,imon,ano_ref(i,imon,1,iy) 
                 do iens=1,nens
                    sie(iens,iy)=sie(iens,iy)+ano_mod(i,imon,iens,iy)
c                   if(i.eq.1000)
c    >                 write(*,*)iy,imon,iens,ano_mod(i,imon,iens,iy) 
                 enddo
              enddo
              reo(iy)=reo(iy)/nummon
              sim(iy)=0.
              do iens=1,nens
                 sie(iens,iy)=sie(iens,iy)/nummon
                 sim(iy)=sim(iy)+sie(iens,iy)/nens
              enddo
              ref(iy)=reo(iy)
c             if(i.eq.1000)
c    >           write(*,*)'ACC: i=',i,'iy=',iy,ref(iy),sim(iy)
           enddo !iy loop
           call varcovar(ref, sim, nyear, weight, 0, sdx, sdy, cov)
           cor=cov/(sdx*sdy+epsdd)
c          write(*,'(i5,4e12.4)')i,' covariances and correlation ',
c    >        cor, sdx, sdy, cov
           psec4_acc(i)=cor
c
c.... compute the MSSS
c
           allocate (x(nyear),                               stat=istat)
           do iy=1,nyear
              x(iy)=sim(iy)-ref(iy)
           enddo !iy loop
           call varcovar(ref, x, nyear, weight, 0, sdx, sdy, cov)
           mse=sdy**2
	   write(*,*)'Sim is',sim
	   write(*,*)'Ref is',ref
	   write(*,*)'x is',x
	   write(*,*)'mse is',mse
           psec4_mss(i)=1.-mse/(sdx+eps)**2
	   if(psec4_mss(i).lt.-10.) then
	    psec4_mss(i)=-10.
	   endif
	   write(*,*)'psec4_mss is',psec4_mss(i)
           deallocate (x,                                    stat=istat)
c
c.... compute the normalized spread
c
           spr=0.
           allocate (x(nens),                                stat=istat)
           do iy=1,nyear
              do iens=1,nens
                 x(iens)=sie(iens,iy)
              enddo
              call moment(x, nens, ave, adev, sdev, var, skew, curt)
              spr=spr+var/nyear
           enddo !iy loop
           deallocate (x,                                    stat=istat)
           allocate (x(nyear),                               stat=istat)
           do iy=1,nyear
              x(iy)=ref(iy)
           enddo !iy loop
           call moment(x, nyear, ave, adev, sdev, var, skew, curt)
           psec4_spr(i)=sqrt(spr/(var+eps))
           deallocate (x,                                    stat=istat)
c
c.... compute the ratio spread/RMSE
c
           allocate (x(nyear),                               stat=istat)
           do iy=1,nyear
              x(iy)=sim(iy)-ref(iy)
           enddo !iy loop
           call varcovar(ref, x, nyear, weight, 0, sdx, sdy, cov)
           psec4_rsr(i)=sqrt(spr/(sdy**2+eps))
           deallocate (x,                                    stat=istat)
c
c.... compute the perfect-model correlation
c
           covs=0.
           sdxs=0.
           sdys=0.
           do jens=1,nens
c             if(i.eq.1000)write(*,*)'jens=',jens
              do iy=1,nyear
                 sim(iy)=0.
                 do iens=1,nens
                    if(iens.ne.jens)then
                      sim(iy)=sim(iy)+sie(iens,iy)/(nens-1)
                    else
                      ref(iy)=sie(iens,iy)
                    endif
                 enddo !iens loop
c                if(i.eq.1000)
c    >              write(*,*)'ACP: i=',i,'iy=',iy,ref(iy),sim(iy)
              enddo !iy loop
              call varcovar(ref, sim, nyear, weight, 0, sdx, sdy, cov)
              covs=covs+cov
              sdxs=sdxs+sdx**2
              sdys=sdys+sdy**2
           enddo !jens loop
           sdxs=sqrt(sdxs)
           sdys=sqrt(sdys)
           cor=covs/(sdxs*sdys+epsdd)
c          write(*,*)sdxs,sdys,cor
           psec4_acp(i) = cor
c
c.... bootstrap if required (boot=1)
c
           if(boot.eq.1)then
             do ib=1,nsam
c               write(*,*)'Sample ',ib
                do iy=1,nyear
                   l=int(nyear*xran(ib,iy))+1
c                  write(*,'(a,i3,a,f10.3,i3)')
c    >               'Random values for sample ',ib,' are ',x,l
                   reo_ran(iy)=reo(l)
                   do iens=1,nens
                      sie_ran(iens,iy)=sie(iens,l)
                   enddo ! iens loop
                enddo ! iy loop
                do iy=1,nyear
                   sim_ran(iy)=0.
                   do iens=1,nens
                      sim_ran(iy)=sim_ran(iy)+sie_ran(iens,iy)/nens
                   enddo
                enddo
                call varcovar(reo_ran, sim_ran, nyear, weight, 0,
     >             sdx, sdy, cov)
                acc_ran(ib)=cov/(sdx*sdy+epsdd)
c
                allocate (x(nyear),                          stat=istat)
                do iy=1,nyear
                   x(iy)=sim_ran(iy)-reo_ran(iy)
                enddo !iy loop
                call varcovar(reo_ran, x, nyear, weight, 0,
     >             sdx, sdy, cov)
                mse=sdy**2
                mss_ran(ib)=1.-mse/sdx**2
                deallocate (x,                               stat=istat)
c
                spr=0.
                allocate (x(nens),                           stat=istat)
                do iy=1,nyear
                   do iens=1,nens
                      x(iens)=sie_ran(iens,iy)
                   enddo
                   call moment(x, nens, ave, adev, sdev,
     >                var, skew, curt)
                   spr=spr+var/nyear
                enddo !iy loop
                deallocate (x,                               stat=istat)
                allocate (x(nyear),                          stat=istat)
                do iy=1,nyear
                   x(iy)=reo_ran(iy)
                enddo !iy loop
                call moment(x, nyear, ave, adev, sdev, var, skew, curt)
                spr_ran(ib)=sqrt(spr/(var+eps))
                rsr_ran(ib)=sqrt(spr/(mse+eps))
                deallocate (x,                               stat=istat)
c
c               covs=0.
c               sdxs=0.
c               sdys=0.
c               do jens=1,nens
c                  do iy=1,nyear
c                     sim_ran(iy)=0.
c                     do iens=1,nens
c                        if(iens.ne.jens)then
c                          sim_ran(iy)=sim_ran(iy)+
c    >                        sie_ran(iens,iy)/(nens-1)
c                        else
c                          ref(iy)=sie_ran(iens,iy)
c                        endif
c                     enddo !iens loop
c                  enddo !iy loop
c                  call varcovar(ref, sim_ran, nyear, weight, 0,
c    >                sdx, sdy, cov)
c                  covs=covs+cov
c                  sdxs=sdxs+sdx**2
c                  sdys=sdys+sdy**2
c               enddo !jens loop
c               sdxs=sqrt(sdxs)
c               sdys=sqrt(sdys)
c               acp_ran(ib)=covs/(sdxs*sdys+epsdd)
             enddo !ib loop
c
             do ib=1,nsam
                xarray(ib)=acc_ran(ib)
             enddo
             call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
             acc_sig(i)=1.
             if((x_bot.lt.0.).and.(x_top.gt.0.))acc_sig(i)=0.
             do ib=1,nsam
                xarray(ib)=mss_ran(ib)
             enddo
             call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
             mss_sig(i)=1.
             if((x_bot.lt.0.).and.(x_top.gt.0.))mss_sig(i)=0.
c            do ib=1,nsam
c               xarray(ib)=spr_ran(ib)
c            enddo
c            call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
c            spr_sig(i)=1.
             if((x_bot.lt.1.).and.(x_top.gt.1.))spr_sig(i)=0.
             do ib=1,nsam
                xarray(ib)=rsr_ran(ib)
             enddo
             call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
             rsr_sig(i)=1.
             if((x_bot.lt.1.).and.(x_top.gt.1.))rsr_sig(i)=0.
c            do ib=1,nsam
c               xarray(ib)=acp_ran(ib)
c            enddo
c            call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
c            acp_sig(i)=1.
c            if((x_bot.lt.0.).and.(x_top.gt.0.))acp_sig(i)=0.
           endif ! boot=1
         else
           psec4_acc(i)=rmiss
           psec4_mss(i)=rmiss
           psec4_spr(i)=rmiss
           psec4_rsr(i)=rmiss
           psec4_acp(i)=rmiss
           acc_sig(i)=rmiss
           mss_sig(i)=rmiss
c          spr_sig(i)=rmiss
           rsr_sig(i)=rmiss
c          acp_sig(i)=rmiss
           ksec1_sav(5)=192
           psec3(2)=rmiss
         endif
      enddo !i loop
c
c.... write output in GRIB file
c
      do ik=1,1024
         ksec1(ik)=ksec1_sav(ik)
      enddo !ik loop
      write(*,*)'Write ACC'
      call writegrb(ounit1, nxny, psec4_acc,
     >   ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
      write(*,*)'Write ACP'
      call writegrb(ounit2, nxny, psec4_acp,
     >   ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
      write(*,*)'Write Spread'
      call writegrb(ounit3, nxny, psec4_spr,
     >   ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
      write(*,*)'Write MSSS'
      call writegrb(ounit4, nxny, psec4_mss,
     >   ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
      write(*,*)'Write ratio spread/RMSE'
      call writegrb(ounit5, nxny, psec4_rsr,
     >   ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
      if(boot.eq.1)then
        call writegrb(ounit11, nxny, acc_sig,
     >     ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
c       call writegrb(ounit12, nxny, acp_sig,
c    >     ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
c       call writegrb(ounit13, nxny, spr_sig,
c    >     ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
        call writegrb(ounit14, nxny, mss_sig,
     >     ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
        call writegrb(ounit15, nxny, rsr_sig,
     >     ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
      endif
c
      deallocate (reo,                                       stat=istat)
      deallocate (ref,                                       stat=istat)
      deallocate (sie,                                       stat=istat)
      deallocate (sim,                                       stat=istat)
      deallocate (weight,                                    stat=istat)
      deallocate (reo_ran,                                   stat=istat)
      deallocate (sie_ran,                                   stat=istat)
      deallocate (sim_ran,                                   stat=istat)
      deallocate (acc_ran,                                   stat=istat)
      deallocate (mss_ran,                                   stat=istat)
      deallocate (spr_ran,                                   stat=istat)
      deallocate (rsr_ran,                                   stat=istat)
      deallocate (acp_ran,                                   stat=istat)
      deallocate (xarray,                                    stat=istat)
      deallocate (acc_sig,                                   stat=istat)
      deallocate (mss_sig,                                   stat=istat)
      deallocate (spr_sig,                                   stat=istat)
      deallocate (rsr_sig,                                   stat=istat)
      deallocate (acp_sig,                                   stat=istat)
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
