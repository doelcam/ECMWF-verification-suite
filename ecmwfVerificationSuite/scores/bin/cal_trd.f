c
c----------------------------------------------------------------------c
c     PROGRAM cal_trd                     F. Doblas-Reyes 02-Jul-2008  c
c                                                                      c
c     PURPOSE:                                                         c
c     Computes the linear trend for the ensemble mean at each grid     c
c     It outputs the slope normalized by the sd of the residuals       c
c                                                                      c
c     INPUT:                                                           c
c     ANOM_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_YYYY GRIB files         c
c                                                                      c
c     OUTPUT:                                                          c
c     TRD_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_III-EEE.grb              c
c                                                          GRIB files  c
c                                                                      c
c     USAGE:                                                           c
c     cal_trd.x < nlist                                                c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 cal_trd.f tools.f -o cal_trd.x $EMOSLIB $NAGLIB            c
c                                                                      c
c     MODS:                                                            c
c     Based on the original cal_trd.f from DEMETER                     c
c                                                                      c
c----------------------------------------------------------------------c
c
      program cal_trd
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: ano_mod(:,:,:,:), ano_ref(:,:,:,:)
      real, allocatable :: dat_mod(:,:,:), dat_ref(:,:)
      real, allocatable :: xhlp(:), xhlp_res(:), xvec(:)
      integer, allocatable :: lsm(:)
      integer, allocatable :: ivfdate(:,:), istep(:,:)
      integer, allocatable :: maskr(:), maskh(:)
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
      character*110 yifile, yofile
      character form1*20
      character*21 expt, expti
      integer nx, ny, nmon, nens
      integer iyy1, iyy2, imm, idd, itt
      integer cros, ipar, ilev, anin,  nf1,  nf2, ndat
      integer idatetype
      integer lena, istat, exptl
      integer nxny, nensp1, nensm1, nyear, nummon
      integer iunit, ounit
      integer i, iy, idat, mask, iilsm, imon, iens, ic, ik
      real rmiss, ensmean, sum
      real a, b, ytrd
      real ave, adev, sdev, var, skew, curt
c
c.... namelist
c
      namelist /control/   nx,   ny, nmon, nens,
     >                   iyy1, iyy2,  imm,  idd,  itt,
     >                   cros, ipar, ilev, anin,  nf1,  nf2,
     >                   expt, ndat
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
      write(*,*)' idd: ',idd
      write(*,*)' itt: ',itt
      write(*,*)'cros: ',cros
      write(*,*)'ipar: ',ipar
      write(*,*)'ilev: ',ilev
      write(*,*)'anin: ',anin
      write(*,*)'nf1: ',nf1
      write(*,*)'nf2: ',nf2
      write(*,*)'expt: ',expt
      write(*,*)'ndat: ',ndat
c
      rmiss=-1.e30
      nensm1 = nens-1
      nensp1 = nens+1
      nxny=nx*ny
      nyear=iyy2-iyy1+1
      nummon=nf2-nf1+1
c
c.... allocate fields
c
      allocate (ano_mod(nxny,nmon,nens,nyear),               stat=istat)
      allocate (ano_ref(nxny,nmon,1,nyear),                  stat=istat)
      allocate (ivfdate(nmon,nyear),                         stat=istat)
      allocate (istep(nmon,nyear),                           stat=istat)
      allocate (lsm(nxny),                                   stat=istat)
      allocate (maskr(nxny),                                 stat=istat)
      allocate (maskh(nxny),                                 stat=istat)
      allocate (dat_ref(nxny,nyear),                         stat=istat)
      allocate (dat_mod(nxny,nyear,nens),                    stat=istat)
      allocate (psec4(nxny),                                 stat=istat)
      allocate (xhlp(nyear),                                 stat=istat)
      allocate (xvec(nyear),                                 stat=istat)
c
      iilsm=0
      call get_mask(iilsm,lsm,nxny,expt)
c
c.... loop over reading model and era data
c....    idat=1 for model
c....    idat=2 for refe
c
      do idat=1,ndat
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
c.... create model- and ref-data field in averaging over forecast period
c
      do i=1,nxny
c        write(*,*)'Working with point ',i
         do iy=1,nyear
            do iens=1,nens
               sum = 0.
               do imon=nf1,nf2
                  if((sum.eq.rmiss).or.
     >               (ano_mod(i,imon,iens,iy).eq.rmiss))then
                    sum=rmiss
                  else
                    sum = sum+ano_mod(i,imon,iens,iy)
                  endif
               enddo !imon
               if(sum.ne.rmiss)then
                 dat_mod(i,iy,iens)=sum/float(nummon)
               else
                 dat_mod(i,iy,iens)=rmiss
               endif
            enddo !iens
            if(ndat.eq.2)then
              sum = 0.
              do imon=nf1,nf2
                 if((sum.eq.rmiss).or.(ano_ref(i,imon,1,iy).eq.rmiss))
     >              then
                   sum=rmiss
                 else
                   sum=sum+ano_ref(i,imon,1,iy)
                 endif
              enddo !imon
              if(sum.ne.rmiss)then
                dat_ref(i,iy)=sum/float(nummon)
              else
                dat_ref(i,iy)=rmiss
              endif
            endif
         enddo !iy
c
c.... create mask for missing data
c.... points for which one reference or model value
c.... are missing are not taken into account in the
c.... computations
c
         maskh(i)=0
         do iy=1,nyear
            do iens=1,nens
               if(dat_mod(i,iy,iens).eq.rmiss)maskh(i)=1
            enddo
         enddo
         if(idat.eq.2)then
           maskr(i)=0
           do iy=1,nyear
              if(dat_ref(i,iy).eq.rmiss)maskr(i)=1
           enddo
         endif
      enddo !i
c
c.... calculate trend (over nyear) at every grid point
c
c
c.... open output file for global map of TRD
c
      do idat=1,ndat
         expti=expt
         if(idat.eq.2)then
            expti = 'refe'
            allocate (xhlp_res(nyear),                       stat=istat)
         else
            allocate (xhlp_res(nyear*nens),                  stat=istat)
         endif
         exptl=lena(expti)
         write(form1,'(a,i2.2,a)')'(a',exptl,')'
         yofile=
     >'TRD1_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE'
         write(yofile(       6: 5+exptl),form1)expti(1:exptl)
         write(yofile( 6+exptl: 6+exptl),'(a1)')'_'
         write(yofile( 7+exptl:10+exptl),'(i4.4)')iyy1
         write(yofile(11+exptl:11+exptl),'(a1)')'-'
         write(yofile(12+exptl:15+exptl),'(i4.4)')iyy2
         write(yofile(16+exptl:16+exptl),'(a1)')'_'
         write(yofile(17+exptl:18+exptl),'(i2.2)')imm
         write(yofile(19+exptl:20+exptl),'(i2.2)')idd
         write(yofile(21+exptl:22+exptl),'(i2.2)')itt
         write(yofile(23+exptl:25+exptl),'(a3)')'_CV'
         write(yofile(26+exptl:27+exptl),'(i2.2)')cros
         write(yofile(28+exptl:29+exptl),'(a2)')'_I'
         if(idat.eq.2)then
           write(yofile(30+exptl:30+exptl),'(i1.1)')0
         else
           write(yofile(30+exptl:30+exptl),'(i1.1)')anin
         endif
         write(yofile(31+exptl:31+exptl),'(a1)')'_'
         write(yofile(32+exptl:34+exptl),'(i3.3)')ipar
         write(yofile(35+exptl:35+exptl),'(a1)')'_'
         write(yofile(36+exptl:38+exptl),'(i3.3)')ilev
         write(yofile(39+exptl:39+exptl),'(a1)')'_'
         write(yofile(40+exptl:42+exptl),'(i3.3)')nf1
         write(yofile(43+exptl:43+exptl),'(a1)')'-'
         write(yofile(44+exptl:46+exptl),'(i3.3)')nf2
         yofile=yofile(1:lena(yofile))//'.grb'
         write(*,*)'open output file: ',yofile(1:lena(yofile))
         kret=0
         call pbopen(ounit,yofile,'w',kret)
         if(kret.ne.0)then
           write(*,*)'Error in opening file: kret=',kret
           goto 997
         endif
c
         do i=1,nxny
c
c.... create data vector at every gridpoint to calculate trend
c
            do iy=1,nyear
               if(idat.eq.1)then
                 ensmean=0.
                 do iens=1,nens
                    ensmean=ensmean+dat_mod(i,iy,iens)/nens
                 enddo
                 xhlp(iy)=ensmean
                 if(maskh(i).eq.1)mask=1
               endif
               if(idat.eq.2)then
                 xhlp(iy)=dat_ref(i,iy)
                 if(maskr(i).eq.1)mask=1
               endif
               xvec(iy)=iy
c              if(idat.eq.2)write(*,*)i,iy,xvec(iy),xhlp(iy)
            enddo
c
c.... calculate trend
c
            call fit1(xvec,xhlp,nyear,a,b)
c
c.... create vectors with differences wrt the trend
c
            ic=0
            do iy=1,nyear
               ytrd=b*iy+a
               if(idat.eq.1)then
                 do iens=1,nens
                    ic=ic+1
                    xhlp_res(ic)=dat_mod(i,iy,iens)-ytrd
                 enddo
               endif
               if(idat.eq.2)then
                 ic=ic+1
                 xhlp_res(ic)=dat_ref(i,iy)-ytrd
               endif
            enddo
c
c.... calculate stdev of "trend-anomaly" vectors
c
            call moment(xhlp_res,ic,ave,adev,sdev,var,skew,curt)
            psec4(i)=b/sdev
            if(mask.eq.1)psec4(i)=rmiss
         enddo !klenp
         deallocate (xhlp_res,                               stat=istat)
c
c.... write output
c
         do ik=1,1024
            ksec1(ik)=ksec1_sav(ik)
         enddo !ik loop
         write(*,*)'Write slopes'
         call writegrb(ounit, nxny, psec4,
     >      ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
         kret=1
         call pbclose(ounit,kret)
         if(kret.lt.0)then
           write(*,*)'Error in closing file: kret=',kret
           goto 997
         endif
      enddo !idat loop
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
