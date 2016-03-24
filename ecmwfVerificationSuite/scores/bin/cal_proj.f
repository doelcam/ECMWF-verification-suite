c
c----------------------------------------------------------------------c
c     PROGRAM cal_proj                    P. Doblas-Reyes 02-Apr-2008  c
c                                                                      c
c     PURPOSE:                                                         c
c     Computes projections of anomalies onto pre-computed spatial      c
c     patterns                                                         c
c                                                                      c
c     INPUT:                                                           c
c     ANOM_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_YYYY GRIB files         c
c                                                                      c
c     OUTPUT:                                                          c
c     TS_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_III-EEE_PRON ASCII files  c
c                                                                      c
c     USAGE:                                                           c
c     cal_proj.x < nlist                                               c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 cal_proj.f tools.f -o cal_proj.x $EMOSLIB $NAGLIB          c
c                                                                      c
c     MODS:                                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      program cal_proj
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: ano_mod(:,:,:,:), ano_ref(:,:,:,:)
      real, allocatable :: regwgt(:), latwgt(:)
      real, allocatable :: rlat(:), rlon(:)
      real, allocatable :: ss(:), s(:,:)
      real, allocatable :: arrr(:), arrh(:,:)
      real, allocatable :: x(:), y(:), w(:)
      real, allocatable :: pr(:), ph(:,:)
      integer, allocatable :: mask(:), lsm(:)
      integer, allocatable :: ivfdate(:,:), istep(:,:)
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
      character*110 yifile, yofile, yrfile
      character form1*20
      character*21 expt, expti
      integer nx, ny, nmon, nens
      integer iyy1, iyy2, imm, idd, itt, nlon, nlat
      integer cros, ipar, ilev, anin,  nf1,  nf2
      integer nevt, boot, permeth, nsam, iprb, npro
      integer idatetype
      integer lena, istat, exptl
      integer nxny, nensp1, nensm1, nyear, idat, nummon
      integer iy, im, iens, i, imon, iensp1, ip, refind
      integer iunit, ounit
      integer iilsm, icatch, npoint, ipoint
      real xlatn, xlats, xlonw, xlone, sum, xpoint, zdlat
      real eps
      real rmiss
      real rint
      real sdx, sdy, cov
      real indexem
      real ave, adev, sdev, var, skew, curt
c
c.... hard coded field definitions
c
      integer npro_max
      parameter(npro_max=10)
      character*6 namp(npro_max)
      real limn(npro_max), lims(npro_max)
      real limw(npro_max), lime(npro_max)
      integer iyys(npro_max), iyye(npro_max)
      integer ilsm(npro_max), rcal(npro_max)
      integer par(npro_max), lev(npro_max)
c
c.... namelist
c
      namelist /control/   nx,   ny, nmon, nens,
     >                   iyys, iyye,  imm,  idd,  itt,
     >                   cros, anin,  nf1,  nf2,
     >                   expt, npro
      namelist /timeseries/    namp,  par,  lev,
     >                   limn, lims, limw, lime,
     >                   ilsm, rcal
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
      write(*,*)' imm: ',imm
      write(*,*)'nf1: ',nf1
      write(*,*)'nf2: ',nf2
      write(*,*)'expt: ',expt
      write(*,*)'npro: ',npro
c
      if(npro.gt.npro_max)then
        write(*,*)'Problems with the number of projections'
        goto 998
      endif
      write(*,*)'iyys: ',(iyys(i),i=1,npro)
      write(*,*)'iyye: ',(iyye(i),i=1,npro)
      read(5,timeseries)
      write(*,*)'namp: ',(namp(i),i=1,npro)
      write(*,*)' par: ',(par(i),i=1,npro)
      write(*,*)' lev: ',(lev(i),i=1,npro)
      write(*,*)'limn: ',(limn(i),i=1,npro)
      write(*,*)'lims: ',(lims(i),i=1,npro)
      write(*,*)'limw: ',(limw(i),i=1,npro)
      write(*,*)'lime: ',(lime(i),i=1,npro)
      write(*,*)'ilsm: ',(ilsm(i),i=1,npro)
      write(*,*)'rcal: ',(rcal(i),i=1,npro)
c
      rmiss=-1.e30
      nensm1 = nens-1
      nensp1 = nens+1
      nxny=nx*ny
      nummon=nf2-nf1+1
c
      eps      = 1.e-30
c
c.... allocate fields
c
      allocate (regwgt(nxny),                                stat=istat)
      allocate (latwgt(nxny),                                stat=istat)
      allocate (rlon(nxny),                                  stat=istat)
      allocate (rlat(nxny),                                  stat=istat)
      allocate (lsm(nxny),                                   stat=istat)
      allocate (mask(nxny),                                  stat=istat)
c
c.... loop over patterns
c
      do ip=1,npro
         if(rcal(ip).eq.1)then
           refind=0
           if(namp(ip).eq.'Z5PNAP')refind=1
           if(namp(ip).eq.'Z5NAOP')refind=2
           if(namp(ip).eq.'SLAONP')refind=3
           if(refind.eq.0)then
             write(*,*)'This projection can not be computed ',namp(ip)
             goto 998
           endif
           ipar=par(ip)
           ilev=lev(ip)
           iyy1=iyys(ip)
           iyy2=iyye(ip)
           nyear=iyy2-iyy1+1
c
           allocate (ano_mod(nxny,nmon,nens,nyear),          stat=istat)
           allocate (ano_ref(nxny,nmon,1,nyear),             stat=istat)
           allocate (ivfdate(nmon,nyear),                    stat=istat)
           allocate (istep(nmon,nyear),                      stat=istat)
           allocate (pr(nyear),                              stat=istat)
           allocate (ph(nyear,nens),                         stat=istat)
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
                 yifile=
     >              'ANOM_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_YYYY'
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
     >                 imm, iyy1, iy, ipar, ilev, idatetype,
     >                 ano_mod(1,1,1,iy), ivfdate(1,iy), istep(1,iy),
     >                 ksec0, ksec1_sav, ksec2, ksec3, ksec4, psec2,
     >                 psec3)
                 endif
                 if(idat.eq.2)then
                   idatetype=2
                   call readgrb(iunit, nxny, nmon, 1, idat,
     >                 imm, iyy1, iy, ipar, ilev, idatetype,
     >                 ano_ref(1,1,1,iy), ivfdate(1,iy), istep(1,iy),
     >                 ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
                 endif
                 call pbclose(iunit,kret)
                 if(kret.ne.0)then
                   write(*,*)'Error in closing file: kret=',kret
                   goto 997
                 endif
              enddo !iy loop
           enddo !idat loop
c
c.... open file with reference pattern (usually an EOF)
c.... patterns have been computed using monthly data;
c.... for each calendar month all the available NCEP
c.... monthly data of the target month plus those for
c.... the previous and next month are used
c
           yrfile='ncep_1948-2000_PAR_LEV_iII.eof.1.mon'
           write(yrfile(16:18),'(i3.3)')ipar
           write(yrfile(20:22),'(i3.3)')ilev
           write(yrfile(25:26),'(i2.2)')refind
           if(refind.eq.3)write(yrfile(32:32),'(i1.1)')0
           write(*,*)'Pattern file ',yrfile,' is an ASCII file '
           close(99)
           open(99,file=yrfile,form='formatted')
c
c.... select parameters for the corresponding region
c
           write(*,*)'Computing for region: ',namp(ip)
           iilsm=ilsm(ip)
           xlats=lims(ip)
           xlatn=limn(ip)
           xlonw=limw(ip)
           xlone=lime(ip)
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
           npoint=0
           xpoint=0.
           do i=1,nxny
              if(mask(i).eq.1)regwgt(i)=0.
              npoint=npoint+int(regwgt(i))
              xpoint=xpoint+regwgt(i)
           enddo
           write(*,*)'the region has got ',npoint,'  int grid points'
           write(*,*)'the region has got ',xpoint,' real grid points'
c
c.... read data for the signature
c
           allocate (ss(npoint),                             stat=istat)
           allocate (s(npoint,12),                           stat=istat)
           allocate (arrr(npoint),                           stat=istat)
           allocate (arrh(nens,npoint),                      stat=istat)
           allocate (w(npoint),                              stat=istat)
           allocate (x(npoint),                              stat=istat)
           allocate (y(npoint),                              stat=istat)
           do im=1,12
              write(*,*)'Reading pattern for month ',im
              do i=1,npoint
                 read(99,'(e12.3)')ss(i)
              enddo
              do i=1,npoint
c                write(*,*)i,ss(i)
                 s(i,im)=ss(i)
              enddo
           enddo
           close(99)
c
c.... compute model and ref projections over forecast period
c
           do iy=1,nyear
              write(*,*)'Covariance for year ',iy
              do iens=1,nens
                 ph(iy,iens)=0.
              enddo !iens
              pr(iy)=0.
              do imon=nf1,nf2
                 ipoint=0
                 do i=1,nxny
c                   write(*,*)'Working with point ',i
                    if((regwgt(i).gt.0.).and.(mask(i).eq.0))then
                      ipoint=ipoint+1
                      arrr(ipoint)=ano_ref(i,imon,1,iy)
                      if(arrr(ipoint).eq.rmiss)then
                        w(ipoint)=0.
                      else
                        w(ipoint)=latwgt(i)
                      endif
                      do iens=1,nens
                         arrh(iens,ipoint)=ano_mod(i,imon,iens,iy)
                         if(arrh(iens,ipoint).eq.rmiss)w(ipoint)=0.
                      enddo !iens
                    endif
                 enddo !i
                 if(ipoint.ne.npoint)then
                   write(*,*)
     >                'Missmatch in the number of points ',ipoint,npoint
                   goto 998
                 endif
                 im=imon+imm-1
                 im=im-((im-1)/12)*12
                 write(*,*)'Covariance for forecast month ',imon
                 write(*,*)'Actual month ',im
                 do i=1,npoint
                    y(i)=s(i,im)
                 enddo !i
                 do iens=1,nens
                    do i=1,npoint
                       x(i)=arrh(iens,i)
                    enddo
                    call varcovar(x, y, npoint, w, 0, sdx, sdy, cov)
                    ph(iy,iens)=ph(iy,iens)+cov/nummon
                    write(*,*)'Covariance for ',iens,' member ',cov
                 enddo !iens
                 do i=1,npoint
                    x(i)=arrr(i)
                 enddo
                 call varcovar(x, y, npoint, w, 0, sdx, sdy, cov)
                 pr(iy)=pr(iy)+cov/nummon
                 write(*,*)'Covariance for reference ',cov
              enddo !imon
           enddo !iy
           deallocate (ss,                                   stat=istat)
           deallocate (s,                                    stat=istat)
           deallocate (arrr,                                 stat=istat)
           deallocate (arrh,                                 stat=istat)
           deallocate (x,                                    stat=istat)
           deallocate (y,                                    stat=istat)
           deallocate (w,                                    stat=istat)
c
c.... open output file
c
           allocate (x(nens*nyear),                          stat=istat)
           do idat=1,2
              expti=expt
              if(idat.eq.2)then
                expti = 'refe'
              endif
              exptl=lena(expti)
              write(form1,'(a,i2.2,a)')'(a',exptl,')'
              yofile=
     >          'TS_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE'
              write(yofile(       4: 3+exptl),form1)expti(1:exptl)
              write(yofile( 4+exptl: 4+exptl),'(a1)')'_'
              write(yofile( 5+exptl: 8+exptl),'(i4.4)')iyy1
              write(yofile( 9+exptl: 9+exptl),'(a1)')'-'
              write(yofile(10+exptl:13+exptl),'(i4.4)')iyy2
              write(yofile(14+exptl:14+exptl),'(a1)')'_'
              write(yofile(15+exptl:16+exptl),'(i2.2)')imm
              write(yofile(17+exptl:18+exptl),'(i2.2)')idd
              write(yofile(19+exptl:20+exptl),'(i2.2)')itt
              write(yofile(21+exptl:23+exptl),'(a3)')'_CV'
              write(yofile(24+exptl:25+exptl),'(i2.2)')cros
              write(yofile(26+exptl:27+exptl),'(a2)')'_I'
              if(idat.eq.2)then
                write(yofile(28+exptl:28+exptl),'(i1.1)')0
              else
                write(yofile(28+exptl:28+exptl),'(i1.1)')anin
              endif
              write(yofile(29+exptl:29+exptl),'(a1)')'_'
              write(yofile(30+exptl:32+exptl),'(i3.3)')ipar
              write(yofile(33+exptl:33+exptl),'(a1)')'_'
              write(yofile(34+exptl:36+exptl),'(i3.3)')ilev
              write(yofile(37+exptl:37+exptl),'(a1)')'_'
              write(yofile(38+exptl:40+exptl),'(i3.3)')nf1
              write(yofile(41+exptl:41+exptl),'(a1)')'-'
              write(yofile(42+exptl:44+exptl),'(i3.3)')nf2
              yofile=yofile(1:lena(yofile))//'_'//namp(ip)
              write(*,*)'open output file: ',yofile(1:lena(yofile))
              open(10,file=yofile,form='formatted',status='unknown')
c
c.... indexm is the ensemble mean
c
              if(idat.eq.2)then
                write(form1,'(a,i1.1,a)')'(i4,e12.4)'
                write(*,*)form1
                call moment(pr, nyear, ave, adev, sdev, var, skew, curt)
                write(*,*)ave,sdev
                do iy=1,nyear
                   pr(iy)=(pr(iy)-ave)/sdev
                enddo
                do iy=1,nyear
                   write(10,form1)iyy1+iy-1,pr(iy)
                enddo
              else
                write(form1,'(a,i2.2,a)')'(i4,',nensp1,'e12.3)'
                if(nensp1.ge.100)write(form1,'(a,i3.3,a)')
     >             '(i4,',nensp1,'e12.3)'
                write(*,*)form1
                i=0
                do iy=1,nyear
                   do iens=1,nens
                      i=i+1
                      x(i)=ph(iy,iens)
                   enddo
                enddo
                call moment(x, nyear*nens, 
     >             ave, adev, sdev, var, skew, curt)
                write(*,*)ave,sdev
                do iy=1,nyear
                   do iens=1,nens
                      ph(iy,iens)=(ph(iy,iens)-ave)/sdev
                   enddo
                enddo
                do iy=1,nyear
                   indexem=0.
                   do iens=1,nens
                      indexem=indexem+ph(iy,iens)/nens
                   enddo
                   write(10,form1)iyy1+iy-1,
     >                (ph(iy,iens),iens=1,nens),indexem
                enddo
              endif
              close(10)
           enddo !idat
         endif
         deallocate (ano_mod,                                stat=istat)
         deallocate (ano_mod,                                stat=istat)
         deallocate (ano_ref,                                stat=istat)
         deallocate (ivfdate,                                stat=istat)
         deallocate (istep,                                  stat=istat)
         deallocate (pr,                                     stat=istat)
         deallocate (ph,                                     stat=istat)
      enddo !ip loop
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
