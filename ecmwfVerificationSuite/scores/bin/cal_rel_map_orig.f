c
c----------------------------------------------------------------------c
c     PROGRAM cal_rel_map                 P. Doblas-Reyes 25-Sep-2006  c
c                                                                      c
c     PURPOSE:                                                         c
c     Computes reliability, ROC area, Brier scores and potential value c
c     for grid points                                                  c
c                                                                      c
c     INPUT:                                                           c
c     INPUT:                                                           c
c     ANOM_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_YYYY GRIB files         c
c                                                                      c
c     OUTPUT:                                                          c
c     DIA_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_THR_BIN.grb GRIB files   c
c                                                                      c
c     USAGE:                                                           c
c     cal_rel_map.x < nlist                                            c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 cal_rel_map.f tools.f -o cal_rel_map.x $EMOSLIB $NAGLIB    c
c                                                                      c
c     MODS:                                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      program cal_rel_map
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: ano_mod(:,:,:,:), ano_ref(:,:,:,:)
      real, allocatable :: arrh(:), arrr(:)
      real, allocatable :: tr(:)
      real, allocatable :: thresh(:,:), thresr(:,:)
      real, allocatable :: dat_ref(:,:), dat_mod(:,:,:)
      real, allocatable :: dat_ref_ran(:,:), dat_mod_ran(:,:,:)
      real, allocatable :: regwgt(:), latwgt(:)
      real, allocatable :: thr_mod(:), thr_ref(:)
      double precision, allocatable :: obs_occ(:,:), obs_noc(:,:)
      double precision, allocatable :: xprob(:,:)
      double precision, allocatable :: sample(:,:)
      double precision, allocatable ::bss_ful(:), bss_red(:), bss_inf(:)
      double precision, allocatable :: rel(:), bss_rel(:)
      double precision, allocatable :: res(:), bss_res(:)
      double precision, allocatable :: unc(:), totocc_rel(:)
      real, allocatable :: yhr(:), yfr(:)
      real, allocatable :: hr(:,:), fr(:,:)
      real, allocatable :: ave_prob(:), sd_prob(:), yprob(:,:)
      real, allocatable :: roc(:), rocsup(:), roclow(:)
      real, allocatable :: roc_nbin(:)
      real, allocatable :: bss(:), bssinf(:), bssrel(:), bssres(:)
      real, allocatable :: bsssha(:), rocss(:)
      real, allocatable :: alpha(:), venv(:)
      real, allocatable :: xarray(:)
      real, allocatable :: bss_ran(:,:), bssinf_ran(:,:)
      real, allocatable :: bssrel_ran(:,:), bssres_ran(:,:)
      real, allocatable :: bsssha_ran(:,:), rocss_ran(:,:)
      real, allocatable :: bss_sig(:), bssinf_sig(:), rocss_sig(:)
      integer, allocatable :: ipt(:), lsm(:)
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
      real, allocatable :: psec4(:)
c
c.... other definitions
c
      character*110 yifile, yofile, yrfile
      character form1*20
      character*21 expt, expti
      integer nx, ny, nmon, nens
      integer iyy1, iyy2, imm, idd, itt
      integer cros, ipar, ilev, anin,  nf1,  nf2
      integer nevt, boot, permeth, nsam, iprb, npro
      integer idatetype
      integer lena, istat, exptl
      integer nxny, nensp1, nensm1, nyear, idat, nummon
      integer iy, iens, i, icla, imon, iensp1, ik
      integer iunit, ounit1, ounit2
      integer inmod, inref, iilsm, icatch, npoint
      integer ir, il, nbin, ncla, ib
      integer narea, it, nprob, ievt, iprob
      integer j, jcount, itag, l
      integer ntop, nbot
      real topth, botth
      real xlatn, xlats, xlonw, xlone, sum, xpoint, zdlat
      real x1, x2, y1, y2, rect, tria
      real xpr, pev
      real eps
      real x_top, x_bot
      real rmiss
      double precision x
      double precision epsdd
c.... external functions
c      external g05saf
c      double precision g05saf
c... external routines
c      external g05kff
c
c.... hard coded field definitions
c
      integer nevt_max
      parameter(nevt_max=10)
      integer pevt(nevt_max)
c
c.... namelist
c
      namelist /control/   nx,   ny, nmon, nens,
     >                   iyy1, iyy2,  imm,  idd,  itt,
     >                   cros, ipar, ilev, anin,  nf1,  nf2,
     >                   expt, nevt, pevt, permeth,
     >                   boot, nsam, iprb, npro,
     >                  topth,botth
c
c.... set default values and read input namelist
c
      nx    = 144
      ny    = 71
      nens  = 9
      iyy1  = 1980
      iyy2  = 2001
      imm   = 11
      expt  = 'scwf'
c
c.... Check the order of the parameters for the indices
c
c   nx:     number of longitudes
c   ny:     number of latitudes
c   nens:   number of ensemble members
c   iyy1:   start year
c   iyy2:   end year
c   imm:    forecast starting month
c   expt:   model id
c
      read(5,control)
      write(*,*)'nx:',nx
      write(*,*)'ny:',ny
      write(*,*)'nmon:',nmon
      write(*,*)'nens: ',nens
      write(*,*)'iyy1: ',iyy1
      write(*,*)'iyy2: ',iyy2
      write(*,*)' imm: ',imm
      write(*,*)'ipar: ',ipar
      write(*,*)'ilev: ',ilev
      write(*,*)'expt: ',expt
      write(*,*)'nf1: ',nf1
      write(*,*)'nf2: ',nf2
      write(*,*)'nevt: ',nevt
      write(*,*)'pevt: ',(pevt(i),i=1,nevt)
      write(*,*)'boot: ',boot
      write(*,*)'nsam: ',nsam
      write(*,*)'iprb: ',iprb
      write(*,*)'npro: ',npro
      write(*,*)'topth: ',topth
      write(*,*)'botth: ',botth
c
      rmiss=-1.e30
      nensm1 = nens-1
      nensp1 = nens+1
      nxny=nx*ny
      nyear=iyy2-iyy1+1
      nummon=nf2-nf1+1
c
c.... Initialization of the random number generator (reproducible)
c
c      call g05kff(0)
c
      eps      = 1.e-30
      epsdd    = tiny(epsdd)
      write(*,*)'Smallest value ',epsdd
c
c.... iprb controls the way the probabilities are binned
c.... if 1, the number of probability bins nprob is the ensemble size plus 1
c.... if not 1, the number of bins nprob is the parameter npro
c.... nbin is used in the potential economic value
c
      nprob=npro
      if(iprb.eq.1)nprob=nens+1
      nbin=nens+1
      write(*,*)'Parameters'
      write(*,'(3i3.3)')iprb,nprob,nbin
c
c.... allocate fields
c
      allocate (ano_mod(nxny,nmon,nens,nyear),               stat=istat)
      allocate (ano_ref(nxny,nmon,1,nyear),                  stat=istat)
      allocate (ivfdate(nmon,nyear),                         stat=istat)
      allocate (istep(nmon,nyear),                           stat=istat)
      allocate (regwgt(nxny),                                stat=istat)
      allocate (latwgt(nxny),                                stat=istat)
      allocate (lsm(nxny),                                   stat=istat)
      allocate (mask(nxny),                                  stat=istat)
      allocate (thresr(nxny,nevt),                           stat=istat)
      allocate (thresh(nxny,nevt),                           stat=istat)
      allocate (thr_mod(nxny),                               stat=istat)
      allocate (thr_ref(nxny),                               stat=istat)
      allocate (dat_ref(nxny,nyear),                         stat=istat)
      allocate (dat_mod(nxny,nyear,nens),                    stat=istat)
      allocate (dat_ref_ran(nxny,nyear),                     stat=istat)
      allocate (dat_mod_ran(nxny,nyear,nens),                stat=istat)
      allocate (alpha(nbin),                                 stat=istat)
      allocate (venv(nbin),                                  stat=istat)
      allocate (ipt(nbin),                                   stat=istat)
      allocate (psec4(nxny),                                 stat=istat)
c
      iilsm=0
      call get_mask(iilsm,lsm,nxny,expt)
      do i=1,nxny
         regwgt(i)=1.
         latwgt(i)=1.
      enddo
c
c.... loop over reading model and ref data
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
     >            imm, iyy1, iy, ipar, ilev, idatetype,
     >            ano_mod(1,1,1,iy), ivfdate(1,iy), istep(1,iy),
     >            ksec0, ksec1_sav, ksec2, ksec3, ksec4, psec2, psec3)
            endif
            if(idat.eq.2)then
              idatetype=2
              call readgrb(iunit, nxny, nmon, 1, idat,
     >            imm, iyy1, iy, ipar, ilev, idatetype,
     >            ano_ref(1,1,1,iy), ivfdate(1,iy), istep(1,iy),
     >            ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
            endif
            call pbclose(iunit,kret)
            if(kret.ne.0)then
              write(*,*)'Error in closing file: kret=',kret
              goto 997
            endif
         enddo !iy loop
      enddo !idat loop
c
c.... create mask for missing data
c.... points for which one reference or model value
c.... are missing are not taken into account in the
c.... computations
c
      do i=1,nxny
         mask(i)=0
         do iy=1,nyear
            do imon=nf1,nf2
               if(ano_ref(i,imon,1,iy).eq.rmiss)mask(i)=1
               do iens=1,nens
                  if(ano_mod(i,imon,iens,iy).eq.rmiss)mask(i)=1
               enddo
            enddo
         enddo
c        write(*,*)'mask(',i,')=',mask(i)
      enddo
c
c.... create model- and ref-data field in averaging over forecast period
c.... length of 1d-data vector at every gridpoint: inmod = nyears*nensa(for model data)
c.... length of 1d-data vector at every gridpoint: inref = nyears      (for ref   data)
c
      allocate (arrr(nyear),                                 stat=istat)
      allocate (arrh(nyear*nens),                            stat=istat)
      do i=1,nxny
c        write(*,*)'Working with point ',i
         inmod=0
         inref=0
         do iy=1,nyear
            do iens=1,nens
               sum = 0.
               do imon=nf1,nf2
                  sum = sum+ano_mod(i,imon,iens,iy)
               enddo !imon
               dat_mod(i,iy,iens)=sum/float(nummon)
               inmod = inmod+1
               arrh(inmod)= sum/float(nummon)
            enddo !iens
            sum = 0.
            do imon=nf1,nf2
               sum = sum+ano_ref(i,imon,1,iy)
            enddo !imon
            inref = inref+1
            dat_ref(i,iy)=sum/float(nummon)
            arrr(inref)=sum/float(nummon)
         enddo !iy
c        write(*,*)'Point ',i,'; Size of arrays ',inref,inmod
c        write(*,*)(arrr(iy),iy=1,inref)
c        write(*,*)(arrh(iy),iy=1,inmod)
c
c.... compute thresholds for binary categories
c
         ncla=100
         allocate (tr(ncla-1),                               stat=istat)
c        write(*,*)'Compute percentiles'
         call percen(arrr,inref,inref,tr,ncla,permeth)
         do ievt=1,nevt
            thresr(i,ievt)=tr(pevt(ievt))
c           write(*,'(a,e12.4)')'Reference threshold ',tr(pevt(ievt))
         enddo
         call percen(arrh,inmod,inmod,tr,ncla,permeth)
         do ievt=1,nevt
            thresh(i,ievt)=tr(pevt(ievt))
c           write(*,'(a,e12.4)')'Model threshold ',tr(pevt(ievt))
         enddo
         deallocate (tr,                                     stat=istat)
      enddo !i
      deallocate (arrh,                                      stat=istat)
      deallocate (arrr,                                      stat=istat)
c
c.... loop over regions
c
      narea=nxny
      allocate (obs_occ(narea,nprob),                        stat=istat)
      allocate (obs_noc(narea,nprob),                        stat=istat)
      allocate (hr(narea,nbin),                              stat=istat)
      allocate (fr(narea,nbin),                              stat=istat)
      allocate (yhr(narea),                                  stat=istat)
      allocate (yfr(narea),                                  stat=istat)
      allocate (xprob(narea,nprob),                          stat=istat)
      allocate (sample(narea,nprob),                         stat=istat)
      allocate (yprob(narea,nprob),                          stat=istat)
      allocate (bss_ful(narea),                              stat=istat)
      allocate (bss_red(narea),                              stat=istat)
      allocate (bss_inf(narea),                              stat=istat)
      allocate (rel(narea),                                  stat=istat)
      allocate (bss_rel(narea),                              stat=istat)
      allocate (res(narea),                                  stat=istat)
      allocate (bss_res(narea),                              stat=istat)
      allocate (unc(narea),                                  stat=istat)
      allocate (totocc_rel(narea),                           stat=istat)
      allocate (ave_prob(narea),                             stat=istat)
      allocate (sd_prob(narea),                              stat=istat)
      allocate (roc(narea),                                  stat=istat)
      allocate (rocsup(narea),                               stat=istat)
      allocate (roclow(narea),                               stat=istat)
      allocate (roc_nbin(narea),                             stat=istat)
      allocate (bss(narea),                                  stat=istat)
      allocate (bssinf(narea),                               stat=istat)
      allocate (bssrel(narea),                               stat=istat)
      allocate (bssres(narea),                               stat=istat)
      allocate (bsssha(narea),                               stat=istat)
      allocate (rocss(narea),                                stat=istat)
      if(boot.eq.1)then
        allocate (xarray(nsam),                              stat=istat)
        allocate (bss_ran(nxny,nsam),                        stat=istat)
        allocate (bssinf_ran(nxny,nsam),                     stat=istat)
        allocate (bssrel_ran(nxny,nsam),                     stat=istat)
        allocate (bssres_ran(nxny,nsam),                     stat=istat)
        allocate (bsssha_ran(nxny,nsam),                     stat=istat)
        allocate (rocss_ran(nxny,nsam),                      stat=istat)
        allocate (bss_sig(nxny),                             stat=istat)
        allocate (bssinf_sig(nxny),                          stat=istat)
        allocate (rocss_sig(nxny),                           stat=istat)
      endif
c
c.... probabilistic score computation
c
      do ievt=1,nevt
c
c.... open output file
c
         exptl=lena(expt)
         write(form1,'(a,i2.2,a)')'(a',exptl,')'
         yofile=
     >'BRV_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_THR_MM_BIN_AAA-EEE'
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
         write(yofile(38+exptl:39+exptl),'(a2)')'_T'
         write(yofile(40+exptl:41+exptl),'(i2.2)')pevt(ievt)
         write(yofile(42+exptl:43+exptl),'(a2)')'_M'
         write(yofile(44+exptl:44+exptl),'(i1.1)')permeth
         write(yofile(45+exptl:46+exptl),'(a2)')'_B'
         write(yofile(47+exptl:48+exptl),'(i2.2)')nprob
         write(yofile(49+exptl:49+exptl),'(a1)')'_'
         write(yofile(50+exptl:52+exptl),'(i3.3)')nf1
         write(yofile(53+exptl:53+exptl),'(a1)')'-'
         write(yofile(54+exptl:56+exptl),'(i3.3)')nf2
         yofile=yofile(1:lena(yofile))//'.grb'
         write(*,*)'open output file: ',yofile(1:lena(yofile))
         kret=0
         call pbopen(ounit1,yofile,'w',kret)
         if(kret.ne.0)then
           write(*,*)'Error in opening file: kret=',kret
           goto 997
         endif
         if(boot.eq.1)then
           yrfile=yofile
           write(yrfile(57+exptl:58+exptl),'(a2)')'_S'
           write(yrfile(59+exptl:63+exptl),'(i5.5)')nsam
           yrfile=yrfile(1:lena(yrfile))//'.grb'
           write(*,*)'open output file ',yrfile(1:lena(yrfile))
           kret=0
           call pbopen(ounit2,yrfile,'w',kret)
           if(kret.ne.0)then
             write(*,*)'Error in opening file: kret=',kret
             goto 997
           endif
           ntop=int(topth*nsam/100)+1
           nbot=int(botth*nsam/100)
           if(nbot.eq.0)nbot=1
         endif
c
c.... thresholds
c
         pev=pevt(ievt)/float(ncla)
         if(pevt(ievt).gt.50)pev=(100-pevt(ievt))/float(ncla)
         do i=1,nxny
            thr_mod(i)=thresh(i,ievt)
            thr_ref(i)=thresr(i,ievt)
         enddo
c
c.... consider anomalies below the threshold for the lower tercile
c
         itag=2
         if(pevt(ievt).lt.50)itag=1
         write(*,*)'Call scores with percentile=',pevt(ievt)
         call bsrelroc(nxny, iprb, nyear, nens, nprob, pev,
     >      regwgt, latwgt, lsm, narea, nbin,
     >      dat_mod, dat_ref, thr_mod, thr_ref, itag,
     >      obs_occ, obs_noc, hr, fr, yhr, yfr, xprob,
     >      sample, yprob, bss_ful, bss_red, bss_inf,
     >      rel, res, bss_rel, bss_res,
     >      unc, totocc_rel, ave_prob, sd_prob,
     >      roc, rocsup, roclow, ipt, alpha, venv, jcount)
c
c.... compute ROC with nbin=nens+1
c
         do i=1,nxny
            roc_nbin(i)=0.
            do iprob=1,nbin
               if(iprob.eq.1)then
                 x1=0.
                 x2=fr(i,1)
                 y1=0.
                 y2=hr(i,1)
               else
                 x1=fr(i,iprob-1)
                 x2=fr(i,iprob)
                 y1=hr(i,iprob-1)
                 y2=hr(i,iprob)
               endif
               rect  = (x2-x1)*y1
               tria  = 0.5*(x2-x1)*(y2-y1)
               roc_nbin(i)=roc_nbin(i)+rect+tria
            enddo
         enddo
c
c.... write scores
c
         do ik=1,1024
            ksec1(ik)=ksec1_sav(ik)
         enddo !ik loop
         do i=1,nxny
            if(mask(i).eq.0)then
              bss(i)=bss_ful(i)
              bssinf(i)=bss_inf(i)
              bssrel(i)=bss_rel(i)
              bssres(i)=bss_res(i)
              bsssha(i)=sd_prob(i)
              rocss(i)=2*roc(i)-1
              rocss(i)=2*roc_nbin(i)-1
            else
              bss(i)=rmiss
              bssinf(i)=rmiss
              bssrel(i)=rmiss
              bssres(i)=rmiss
              bsssha(i)=rmiss
              rocss(i)=rmiss
              ksec1(5)=192
              psec3(2)=rmiss
            endif
c           write(*,'(i5,6e12.2)')i,
c    >         bss(i),bssinf(i),bssrel(i),bssres(i),bsssha(i),rocss(i)
         enddo
         write(*,*)'Write BSS'
         do i=1,nxny
            psec4(i)=bss(i)
         enddo
         call writegrb(ounit1, nxny, psec4,
     >      ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
         write(*,*)'Write BSS infinite'
         do i=1,nxny
            psec4(i)=bssinf(i)
         enddo
         call writegrb(ounit1, nxny, psec4,
     >      ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
         write(*,*)'Write BSS_rel'
         do i=1,nxny
            psec4(i)=bssrel(i)
         enddo
         call writegrb(ounit1, nxny, psec4,
     >      ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
         write(*,*)'Write BSS_res'
         do i=1,nxny
            psec4(i)=bssres(i)
         enddo
         call writegrb(ounit1, nxny, psec4,
     >      ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
         write(*,*)'Write sharpness'
         do i=1,nxny
            psec4(i)=bsssha(i)
         enddo
         call writegrb(ounit1, nxny, psec4,
     >      ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
         write(*,*)'Write ROC skill score'
         do i=1,nxny
            psec4(i)=rocss(i)
         enddo
         call writegrb(ounit1, nxny, psec4,
     >      ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
         kret=1
         call pbclose(ounit1,kret)
         if(kret.lt.0)then
           write(*,*)'Error in closing file: kret=',kret
           goto 997
         endif
c
c.... bootstrap if required (boot=1)
c
         if(boot.eq.1)then
           write(*,'(a,i5,a)')'Bootstrap with ',nsam,' samples'
           do ib=1,nsam
              write(*,*)'Sample ',ib
              do iy=1,nyear
c                 x=g05saf(x)
                 x=1
                 l=int(nyear*x)+1
c                write(*,'(a,i3,a,f10.3,i3)')
c    >             'Random values for sample ',ib,' are ',x,l
                 do i=1,nxny
                    dat_ref_ran(i,iy)=dat_ref(i,l)
                    do iens=1,nens
                       dat_mod_ran(i,iy,iens)=dat_mod(i,l,iens)
                    enddo ! iens loop
                 enddo ! i loop
              enddo ! iy loop
              itag=2
              if(pevt(ievt).lt.50)itag=1
              call bsrelroc(nxny, iprb, nyear, nens, nprob, pev,
     >           regwgt, latwgt, lsm, narea, nbin,
     >           dat_mod_ran, dat_ref_ran, thr_mod, thr_ref, itag,
     >           obs_occ, obs_noc, hr, fr, yhr, yfr, xprob,
     >           sample, yprob, bss_ful, bss_red, bss_inf,
     >           rel, res, bss_rel, bss_res,
     >           unc, totocc_rel, ave_prob, sd_prob,
     >           roc, rocsup, roclow, ipt, alpha, venv, jcount)
              do i=1,nxny
                 bss_ran(i,ib)=bss_ful(i)
                 bssinf_ran(i,ib)=bss_inf(i)
                 bssrel_ran(i,ib)=bss_rel(i)
                 bssres_ran(i,ib)=bss_res(i)
                 bsssha_ran(i,ib)=sd_prob(i)
                 rocss_ran(i,ib)=2*roc(i)-1.
c
c.... compute ROC with nbin=nens+1
c
                 roc_nbin(i)=0.
                 do iprob=1,nbin
                    if(iprob.eq.1)then
                      x1=0.
                      x2=fr(i,1)
                      y1=0.
                      y2=hr(i,1)
                    else
                      x1=fr(i,iprob-1)
                      x2=fr(i,iprob)
                      y1=hr(i,iprob-1)
                      y2=hr(i,iprob)
                    endif
                    rect  = (x2-x1)*y1
                    tria  = 0.5*(x2-x1)*(y2-y1)
                    roc_nbin(i)=roc_nbin(i)+rect+tria
                 enddo
                 rocss_ran(i,ib)=2*roc_nbin(i)-1.
              enddo
           enddo !ib loop
c
           do i=1,nxny
              if(mask(i).eq.0)then
                do ib=1,nsam
                   xarray(ib)=bss_ran(i,ib)
                enddo
                call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                bss_sig(i)=1.
                if((x_bot.lt.0.).and.(x_top.gt.0.))bss_sig(i)=0.
c               write(*,'(a,4f10.3)')'BSS ',
c    >             bss(i),x_bot,x_top,bss_sig(i)
c
                do ib=1,nsam
                   xarray(ib)=bssinf_ran(i,ib)
                enddo
                call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                bssinf_sig(i)=1.
                if((x_bot.lt.0.).and.(x_top.gt.0.))bssinf_sig(i)=0.
c               write(*,'(a,4f10.3)')'BSS infinite ',
c    >             bssinf(i),x_bot,x_top,bssinf_sig(i)
c
                do ib=1,nsam
                   xarray(ib)=rocss_ran(i,ib)
                enddo
                call edge(xarray,nsam,ntop,nbot,x_top,x_bot)
                rocss_sig(i)=1.
                if((x_bot.lt.0.).and.(x_top.gt.0.))rocss_sig(i)=0.
c               write(*,'(a,4f10.3)')'ROCSS ',
c    >             rocss(i),x_bot,x_top,rocss_sig(i)
              else
                bss_sig(i)=rmiss
                bssinf_sig(i)=rmiss
                rocss_sig(i)=rmiss
                ksec1(5)=192
                psec3(2)=rmiss
              endif
           enddo
c
           write(*,*)'Write BSS significance'
           call writegrb(ounit2, nxny, bss_sig,
     >        ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
           write(*,*)'Write BSS infinite significance'
           call writegrb(ounit2, nxny, bssinf_sig,
     >        ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
           write(*,*)'Write ROC significance'
           call writegrb(ounit2, nxny, rocss_sig,
     >        ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
         endif
      enddo !ievt
c
      deallocate (obs_occ,                                   stat=istat)
      deallocate (obs_noc,                                   stat=istat)
      deallocate (xprob,                                     stat=istat)
      deallocate (hr,                                        stat=istat)
      deallocate (fr,                                        stat=istat)
      deallocate (yhr,                                       stat=istat)
      deallocate (yfr,                                       stat=istat)
      deallocate (sample,                                    stat=istat)
      deallocate (yprob,                                     stat=istat)
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
      deallocate (roc_nbin,                                  stat=istat)
      deallocate (bss,                                       stat=istat)
      deallocate (bssinf,                                    stat=istat)
      deallocate (bssrel,                                    stat=istat)
      deallocate (bssres,                                    stat=istat)
      deallocate (bsssha,                                    stat=istat)
      deallocate (rocss,                                     stat=istat)
      if(boot.eq.1)then
        deallocate (xarray,                                  stat=istat)
        deallocate (bss_ran,                                 stat=istat)
        deallocate (bssinf_ran,                              stat=istat)
        deallocate (bssrel_ran,                              stat=istat)
        deallocate (bssres_ran,                              stat=istat)
        deallocate (bsssha_ran,                              stat=istat)
        deallocate (rocss_ran,                               stat=istat)
        deallocate (bss_sig,                                 stat=istat)
        deallocate (bssinf_sig,                              stat=istat)
        deallocate (rocss_sig,                               stat=istat)
      endif
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
