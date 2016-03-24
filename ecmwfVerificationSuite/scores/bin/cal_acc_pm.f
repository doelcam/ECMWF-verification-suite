c
c----------------------------------------------------------------------c
c     PROGRAM cal_acc_pm                  P.Doblas-Reyes 14-Apr-2004   c
c                                                                      c
c     PURPOSE:                                                         c
c     calculates anomaly correlation coefficients at grid points and   c
c     averaged over regions (for individual ensembles and the ensemble c
c     mean for the perfect model approach                              c
c                                                                      c
c     INPUT:                                                           c
c     ANOM_EXPT_YYYY-YYYY_YYYY-YYYY_YYYY-YYYY_MM_PAR_LEV_YYYY          c
c                                                     GRIB files       c
c                                                                      c
c     OUTPUT:                                                          c
c     ACP_EXPT_YYYY-YYYY_YYYY-YYYY_YYYY-YYYY_MM_PAR_LEV                c
c                                 GRIB files (global map of avg acc)   c
c     ACP_EXPT_YYYY-YYYY_YYYY-YYYY_YYYY-YYYY_MM_PAR_LEV_rII            c
c                                   ASCII files (yearly reg avg acc)   c
c                                                                      c
c     USAGE:                                                           c
c     cal_acc_pm.x < nlist                                             c
c                                                                      c
c     COMPILING:                                                       c
c     xlf90 -qfixed -g -qsigtrap -O3 -qarch=pwr3 -C -qextchk           c
c     -bmaxdata:0x70000000 -qmaxmem=8192                               c
c     -qflttrap=overflow:zerodivide:invalid:enable                     c
c     cal_acc_pm.f tools.f -o cal_acc_pm.x $EMOSLIB $NAGLIB            c
c                                                                      c
c     MODS:                                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      program cal_acc_pm
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable:: ano_mod(:,:,:,:)
      real, allocatable:: ano_mod_s(:,:,:,:)
      real, allocatable:: ano_era(:,:,:)
      real, allocatable:: regwgt(:), latwgt(:)
      real, allocatable:: cov(:), varr(:), varp(:)
      integer, allocatable:: lsm(:)
      integer, allocatable:: iyear(:)
c
c.... definitions for GRIBEX call
c
      integer kleng
      parameter (kleng=200000)
c
      integer ksec0(2)
      integer ksec0_sav(2)
      integer ksec1(1024)
      integer ksec1_sav(1024)
      integer ksec2(1024)
      integer ksec2_sav(1024)
      integer ksec3(2)
      integer ksec3_sav(2)
      integer ksec4(512)
      integer ksec4_sav(512)
      integer kgrib(kleng)
      integer klenp, kword, koutlen, kret
c
      real psec2(512)
      real psec2_sav(512)
      real psec3(2)
      real psec3_sav(2)
c
      real, allocatable:: psec4(:)
c
c.... other definitions
c
      character yfile*110 
      character ybasedisk*80
      character yidisk*80, ymdisk*80, yedisk*80, yodisk*80
      character yifile*55, yofile*60
      character*30 form1
      character*4 expt, expti, verif
      real xlats, xlatn, xlonw, xlone
      real ensmean, climate, rms_mod, rms_ref
      real ref, rms_cov, rms_m, rms_c, rms_ctr
      real var_mod, yhlp, xhlp
      real eps, rmiss, totwgt
      real ref_tot, mod_tot
      real rrms_ref, rrms_cov, rrms_mod, acc
      double precision epsdd, cor
      integer nx, ny, nxny, nmon, nens, nmod
      integer iyy1, iyy2, iyy3, iyy4, iyy5, iyy6
      integer imm, ipar, ilev, nreg
      integer ifield, ilevel, lena, istat
      integer nensp1, nensa, nyear, idat, iread
      integer i, ifcp, ivf, nummon, num1, num2
      integer imon, iens, ie, iensp1, ir
      integer iyys, iyye, iy, nensm1
      integer iunit, ounit, iilsm, icatch
      integer nelem, ielem, iielem
      integer iiens, imod, iimod
c
c.... hard coded field definitions
c
      integer nreg_max, nfcp
      parameter(nreg_max=12, nfcp=2)
      real limn(nreg_max), lims(nreg_max) 
      real limw(nreg_max), lime(nreg_max)
      integer ilsm(nreg_max)
      integer nf1(nfcp), nf2(nfcp)
c
      data nf1/2,4/
      data nf2/4,6/
c
      namelist /control/   nx,   ny, nmon, nens,
     >                   iyy1, iyy2, iyy3, iyy4, iyy5, iyy6,
     >                    imm, ipar, ilev, expt, nmod,
     >                  verif, ybasedisk,
     >                   nreg, limn, lims, limw, lime, ilsm
c
c.... set default values and read input namelist
c
      nx = 144
      ny = 71
      nmon = 6
      nens = 9 
      iyy1 = 0
      iyy2 = 0
      iyy3 = 0
      iyy4 = 0
      iyy5 = 0
      iyy6 = 0
      imm  = 11
      ipar = 139
      ilev = 000
      expt = 'scwc'
      verif= 'er40'
      nmod = 1
      ybasedisk = '/vol/demeter/verify/'
      nreg = 8
c
      read(5,control)
      write(*,*)'  nx: ', nx
      write(*,*)'  ny: ', ny
      write(*,*)'nmon: ',nmon
      write(*,*)'nens: ',nens
      write(*,*)'iyy1: ',iyy1
      write(*,*)'iyy2: ',iyy2
      write(*,*)'iyy3: ',iyy3
      write(*,*)'iyy4: ',iyy4
      write(*,*)'iyy5: ',iyy5
      write(*,*)'iyy6: ',iyy6
      write(*,*)' imm: ',imm
      write(*,*)'ipar: ',ipar
      write(*,*)'ilev: ',ilev
      write(*,*)'expt: ',expt
      write(*,*)'nmod: ',nmod
      write(*,*)'verif: ',verif
      write(*,*)'ybasedisk: ',ybasedisk
      write(*,*)'nreg: ',nreg
      write(*,*)'limn: ',(limn(i),i=1,nreg)
      write(*,*)'lims: ',(lims(i),i=1,nreg)
      write(*,*)'limw: ',(limw(i),i=1,nreg)
      write(*,*)'lime: ',(lime(i),i=1,nreg)
      write(*,*)'ilsm: ',(ilsm(i),i=1,nreg)
c
c.... set nens according to experiment
c
      nensa=nens*nmod
      nensm1 = nensa-1
      nensp1 = nensa+1
c
      nxny=nx*ny
      klenp=nxny
c
c.... check how many years are available (nyear)
c
      if (iyy5.ne.0) iyys = iyy5
      if (iyy3.ne.0) iyys = iyy3
      if (iyy1.ne.0) iyys = iyy1
      if (iyy2.ne.0) iyye = iyy2
      if (iyy4.ne.0) iyye = iyy4
      if (iyy6.ne.0) iyye = iyy6
      nyear = 0
      do iy = iyys,iyye
         if ((iy.ge.iyy1 .and. iy.le.iyy2) .or. 
     >       (iy.ge.iyy3 .and. iy.le.iyy4) .or.
     >       (iy.ge.iyy5 .and. iy.le.iyy6)) nyear=nyear+1
      enddo
c
c.... allocate fields
c
      allocate (regwgt(nxny),                    stat=istat)
      allocate (latwgt(nxny),                    stat=istat)
      allocate (lsm(nxny),                       stat=istat)
      allocate (cov(nxny),                       stat=istat)
      allocate (varr(nxny),                      stat=istat)
      allocate (varp(nxny),                      stat=istat)
      allocate (ano_era(klenp,nmon,nyear),       stat=istat)
      allocate (ano_mod(klenp,nmon,nyear,nensa), stat=istat)
      allocate (psec4(klenp),                    stat=istat)
      allocate (iyear(nyear),                    stat=istat)
c
c.... write available years to iyear field
c
      nyear = 0
      do iy = iyys,iyye
         if ((iy.ge.iyy1 .and. iy.le.iyy2) .or. 
     >       (iy.ge.iyy3 .and. iy.le.iyy4) .or.
     >       (iy.ge.iyy5 .and. iy.le.iyy6)) then            
            nyear = nyear+1
            iyear(nyear) = iy
            write(*,*)'year: ',nyear,iyear(nyear)
         endif
      enddo
c
c.... set missing data indicators and reset fields
c
      rmiss    = -1.e30
      psec3(2) = rmiss
      eps      = 1.e-30
      epsdd    = tiny(epsdd)
      write(*,*)'Smallest value ',epsdd
c
c.... set disknames
c
      ymdisk = ybasedisk(1:lena(ybasedisk))//'data/'//expt//'/anom/'
      yedisk = ybasedisk(1:lena(ybasedisk))//'data/'//verif//'/anom/'
      yodisk = ybasedisk(1:lena(ybasedisk))//
     >'data/'//expt//'/anom/acc/'
c
c.... loop over reading model and era data
c....    idat=1 for model
c....    idat=2 for er40
c
      do idat=1,1
         expti  = expt
         yidisk = ymdisk
         if (idat.eq.2) then
            expti = verif
            yidisk = yedisk
         endif
c
c.... read all years of anomalies
c
         do iy=1,nyear
      yifile='ANOM_EXPT_YYYY-YYYY_YYYY-YYYY_YYYY-YYYY_MM_PAR_LEV_YYYY'
            write(yifile( 6: 9),'(a4)')expti
            write(yifile(11:14),'(i4.4)')iyy1
            write(yifile(16:19),'(i4.4)')iyy2
            write(yifile(21:24),'(i4.4)')iyy3
            write(yifile(26:29),'(i4.4)')iyy4
            write(yifile(31:34),'(i4.4)')iyy5
            write(yifile(36:39),'(i4.4)')iyy6
            write(yifile(41:42),'(i2.2)')imm
            write(yifile(44:46),'(i3.3)')ipar
            write(yifile(48:50),'(i3.3)')ilev
            write(yifile(52:55),'(i4.4)')iyear(iy)
            yfile=yidisk(1:lena(yidisk))//yifile
            write(*,*)'open input file: ',yfile
            kret=0
            call pbopen(iunit,yfile,'r',kret)
            if(kret.ne.0)then
              write(*,*)'Error in opening file: kret=',kret
              goto 997
            endif
c
c.... check correct parameter and read all nmon months and nensa ensemble member
c
            iread=0
            imon=0
10          continue
            iread=iread+1
c            write(*,*)'hello', iread
            kret=1
            call pbgrib(iunit,kgrib,kleng,koutlen,kret)
            if(kret.lt.0)then
              if(kret.eq.-1)then
                write(*,*)'Sorry, did not find all correct fields...'
                write(*,*)'...now end of file'
                write(*,*)'Number of read fields =',(iread-1)
                goto 997
              else
                write(*,*)'Error in reading input file: kret=',kret
                write(*,*)'after ',iread,' products'
                goto 997
              endif
            endif
            kret=1
            call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,
     >                  psec4,klenp,kgrib,kleng,kword,'D',kret)
            if(kret.gt.0)then
              write(6,*)'Error decoding data: kret=',kret
              goto 997
            endif
            ifield = ksec1(6)
            ilevel = ksec1(8)
c            write(*,*)'ifield',iread,ifield,ipar
c            write(*,*)'ilevel',iread,ilevel,ilev
            if (ifield.ne.ipar .or. ilevel.ne.ilev) then
              goto 10
            else
              if (idat.eq.1) then
                 iens   = ksec1(42)
                 iensp1 = iens+1
                 ivf=ksec1(46)
                 imon=ivf-int(ivf/100.)*100
                 if(imon.lt.imm)then
                   imon=imon+12-imm+1
                 else
                   imon=imon-imm+1
                 endif
                 if(iens.eq.0)then
                   do i=1,1024
                      ksec1_sav(i)=ksec1(i)
                      ksec2_sav(i)=ksec2(i)
                   enddo
                   ksec0_sav(1)=ksec0(1)
                   ksec0_sav(2)=ksec0(2)
                   ksec3_sav(1)=ksec3(1)
                   ksec3_sav(2)=ksec3(2)
                   psec3_sav(1)=psec3(1)
                   psec3_sav(2)=psec3(2)
                   do i=1,512
                      ksec4_sav(i)=ksec4(i)
                      psec2_sav(i)=psec2(i)
                   enddo
                 endif
                 do i=1,klenp
                    ano_mod(i,imon,iy,iensp1)=psec4(i)
                 enddo
c                 write(*,*)imon,nmon,iens,nensm1
                 if(imon.eq.nmon .and. iens.eq.nensm1)then
                   goto 20
                 else
                   goto 10
                 endif
              endif
              if (idat.eq.2) then
                 imon=imon+1
                 do i=1,klenp
                    ano_era(i,imon,iy)=psec4(i)
                 enddo
                 if(imon.eq.nmon)then
                   goto 20
                 else
                   goto 10
                 endif
              endif
            endif
c
20          continue
c
            call pbclose(iunit,kret)
         enddo !iy loop
      enddo !idat loop
c
c.... calculate acc at every grid point (average over nyear)
c.... loop over different lead times
c
      do ifcp=1,nfcp
         nummon=nf2(ifcp)-nf1(ifcp)+1
c
c.... open output file for global map of avg ACC
c
      yofile='ACP_EXPT_YYYY-YYYY_YYYY-YYYY_YYYY-YYYY_MM_PAR_LEV_I-L'
         write(yofile( 5: 8),'(a4)')expt
         write(yofile(10:13),'(i4.4)')iyy1
         write(yofile(15:18),'(i4.4)')iyy2
         write(yofile(20:23),'(i4.4)')iyy3
         write(yofile(25:28),'(i4.4)')iyy4
         write(yofile(30:33),'(i4.4)')iyy5
         write(yofile(35:38),'(i4.4)')iyy6
         write(yofile(40:41),'(i2.2)')imm
         write(yofile(43:45),'(i3.3)')ipar
         write(yofile(47:49),'(i3.3)')ilev
         write(yofile(51:51),'(i1.1)')nf1(ifcp)
         write(yofile(53:53),'(i1.1)')nf2(ifcp)
         yfile=yodisk(1:lena(yodisk))//yofile
         write(*,*)'open output file: ',yfile
         kret=0
         call pbopen(ounit,yfile,'w',kret)
         if(kret.ne.0)then
            write(*,*)'Error in opening file: kret=',kret
           goto 997
         endif
c
         do imod=1,nmod+2
            if(imod.le.nmod)then
              allocate(ano_mod_s(klenp,nmon,nyear,nens),     stat=istat)
              do iens=1,nens
                 iiens=(imod-1)*nens+iens
                 do iy=1,nyear
                    do imon=nf1(ifcp),nf2(ifcp)
                       do i=1,klenp
                          ano_mod_s(i,imon,iy,iens)=
     >                       ano_mod(i,imon,iy,iiens)
                       enddo
                    enddo
                 enddo
              enddo
              nelem=nens
            elseif(imod.eq.(nmod+1))then
              allocate(ano_mod_s(klenp,nmon,nyear,nensa),    stat=istat)
              do iens=1,nensa
                 do iy=1,nyear
                    do imon=nf1(ifcp),nf2(ifcp)
                       do i=1,klenp
                          ano_mod_s(i,imon,iy,iens)=
     >                       ano_mod(i,imon,iy,iens)
                       enddo
                    enddo
                 enddo
              enddo
              nelem=nensa
            elseif(imod.eq.(nmod+2))then
              allocate(ano_mod_s(klenp,nmon,nyear,nmod),     stat=istat)
              do iimod=1,nmod
                 do iy=1,nyear
                    do imon=nf1(ifcp),nf2(ifcp)
                       do i=1,klenp
                          ano_mod_s(i,imon,iy,iimod)=0.
                       enddo
                    enddo
                 enddo
                 do iens=1,nens
                    iiens=(iimod-1)*nens+iens
                    do iy=1,nyear
                       do imon=nf1(ifcp),nf2(ifcp)
                          do i=1,klenp
                             ano_mod_s(i,imon,iy,iimod)=
     >                          ano_mod_s(i,imon,iy,iimod)+
     >                          ano_mod(i,imon,iy,iiens)/nens
                          enddo
                       enddo
                    enddo
                 enddo
              enddo
              nelem=nmod
            endif
c
            do i=1,klenp
               cov(i)=0.
               varr(i)=0.
               varp(i)=0.
               do ielem=1,nelem
                  mod_tot=0.
                  ref_tot=0.
                  do iy=1,nyear
                     ensmean=0.
                     ref=0.
                     do imon=nf1(ifcp),nf2(ifcp)
                        ref = ref+ano_mod_s(i,imon,iy,ielem)
                        do iielem=1,nelem
                           if(iielem.ne.ielem)then
                             ensmean=ensmean+
     >                          ano_mod_s(i,imon,iy,iielem)
                           endif
                        enddo
                     enddo
                     ensmean = ensmean/float((nelem-1)*nummon)
                     ref = ref/nummon
                     ref_tot=ref_tot+ref/nyear
                     mod_tot=mod_tot+ensmean/nyear
                  enddo
c                 write(*,'(a,i6,2e12.4)')'average climatologies ',i,
c    >               ref_tot,mod_tot
                  rms_m  =0.
                  rms_c  =0.
                  rms_mod=0.
                  rms_ctr=0.
                  rms_cov=0.
                  do iy=1,nyear
                     ensmean=0.
                     ref=0.
                     do imon=nf1(ifcp),nf2(ifcp)
                        ref = ref+ano_mod_s(i,imon,iy,ielem)
                        do iielem=1,nelem
                           if(iielem.ne.ielem)then
                             ensmean=ensmean+
     >                          ano_mod_s(i,imon,iy,iielem)
                           endif
                        enddo
                     enddo
                     ensmean = ensmean/float((nelem-1)*nummon)
                     ref = ref/nummon
                     ref=ref-ref_tot
                     ensmean=ensmean-mod_tot
c
                     rms_m   = rms_m   + ensmean/nyear
                     rms_c   = rms_c   + ref/nyear
                     rms_mod = rms_mod + ensmean**2
                     rms_ctr = rms_ctr + ref**2
                     rms_cov = rms_cov + ensmean*ref
                  enddo
c                 write(*,*)rms_c,rms_m
                  cov(i)=cov(i)+rms_cov
                  varr(i)=varr(i)+rms_ctr
                  varp(i)=varp(i)+rms_mod
               enddo
               cov(i)=cov(i)/nelem
               varr(i)=varr(i)/nelem
               varp(i)=varp(i)/nelem
               cor = cov(i)/sqrt(varr(i)*varp(i)+epsdd)
c              write(*,'(a,i6,4e12.4)')'covariances ',i,
c    >            cov(i),varr(i),varp(i),cor
c
c.... write out acc
c
               psec4(i) = cor
            enddo
c
c.... write output
c
            do i=1,1024
               ksec1(i)=ksec1_sav(i)
               ksec2(i)=ksec2_sav(i)
            enddo
            ksec0(1)=ksec0_sav(1)
            ksec0(2)=ksec0_sav(2)
            ksec3(1)=ksec3_sav(1)
            ksec3(2)=ksec3_sav(2)
            psec3(1)=psec3_sav(1)
            psec3(2)=psec3_sav(2)
            do i=1,512
               ksec4(i)=ksec4_sav(i)
               psec2(i)=psec2_sav(i)
            enddo
            do i=1,klenp
               kgrib(i)=0
            enddo
            write(*,*)'Encoding field for model ',imod
            call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,
     >         psec4,klenp,kgrib,kleng,kword,'C',kret)
            if(kret.ne.0)then
              write(*,*)'Error encoding data: kret=',kret
              goto 997
            endif
            kret=1
            call pbwrite(ounit,kgrib,kword*4,kret)
            if(kret.lt.0)then
              write(*,*)'Error in writing file: kret=',kret
              goto 997
            endif
c
c.... loop over regions
c
            write(form1,'(a)')'(i4,f10.2)'
            write(*,*)form1
            do ir=1,nreg
               iilsm = ilsm(ir)
               icatch = 0
               call get_mask(iilsm,lsm,nxny,expt)
c              write(*,*)'lsm'
c              write(*,'(40i4)')(lsm(i),i=1,nxny)
               xlats = lims(ir)
               xlatn = limn(ir)
               xlonw = limw(ir)
               xlone = lime(ir)
               write(*,*) xlats, xlatn, xlonw, xlone
               call get_region(icatch,xlats,xlatn,xlonw,xlone,
     >            nxny,expt,ybasedisk,regwgt,latwgt)
c              write(*,*)'regwgt'
c              write(*,'(40f4.1)')(regwgt(i),i=1,nxny)
c              write(*,*)'latwgt'
c              write(*,'(20f8.3)')(latwgt(i),i=1,nxny)
c
c.... open output file for yearly data of regional averaged rmsss
c
      yofile='ACP_EXPT_YYYY-YYYY_YYYY-YYYY_YYYY-YYYY_MM_PAR_LEV_I-L_rII'
               write(yofile( 5: 8),'(a4)')expt
               write(yofile(10:13),'(i4.4)')iyy1
               write(yofile(15:18),'(i4.4)')iyy2
               write(yofile(20:23),'(i4.4)')iyy3
               write(yofile(25:28),'(i4.4)')iyy4
               write(yofile(30:33),'(i4.4)')iyy5
               write(yofile(35:38),'(i4.4)')iyy6
               write(yofile(40:41),'(i2.2)')imm
               write(yofile(43:45),'(i3.3)')ipar
               write(yofile(47:49),'(i3.3)')ilev
               write(yofile(51:51),'(i1.1)')nf1(ifcp)
               write(yofile(53:53),'(i1.1)')nf2(ifcp)
               write(yofile(56:57),'(i2.2)')ir
               yfile=yodisk(1:lena(yodisk))//yofile
               write(*,*)'open output file: ',yfile
               if(imod.eq.1)then
                 open(10,file=yfile,form='formatted',status='unknown')
               else
                 open(10,file=yfile,form='formatted',status='old',
     >              position='append')
               endif
c
c.... loop over nfcp forecast periods, nyear years and nensa ensemble members:
c
               write(*,*)'from ',nf1(ifcp),' to ',nf2(ifcp)
               nummon=nf2(ifcp)-nf1(ifcp)+1
c
c.... do calculation for ensmean
c.... accumulate square error over months and region
c
               rrms_mod=0.
               rrms_ref=0.
               rrms_cov=0.
               totwgt=0.
               do i=1,klenp
                  if(regwgt(i).gt.0.)then
                    rrms_mod=rrms_mod+varp(i)*lsm(i)*latwgt(i)
                    rrms_ref=rrms_ref+varr(i)*lsm(i)*latwgt(i)
                    rrms_cov=rrms_cov+cov(i)*lsm(i)*latwgt(i)
                  endif
               enddo !i
c
c.... calc acc
c
               acc=rrms_cov/sqrt(rrms_ref*rrms_mod+eps)
c
c.... write results to ascii file
c
               write(10,form1)imod,acc
               close(10)
            enddo !ir
c
c.... write time average ACC to ascii file
c
            deallocate(ano_mod_s,                            stat=istat)
         enddo !imod
         kret=1
         call pbclose(ounit,kret)
         if(kret.lt.0)then
           write(*,*)'Error in closing file: kret=',kret
           goto 997
         endif
      enddo !ifcp
c
      goto 999
 997  continue
      if(kret.ne.0)then
        write(*,*)'Sorry, no success :('
        call abort
      else
        write(*,*)'program seems to be successfully finished :)'
      endif
 999  continue
c
      end
c
