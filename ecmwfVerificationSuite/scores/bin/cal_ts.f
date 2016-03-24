c
c----------------------------------------------------------------------c
c     PROGRAM cal_ts                      P. Doblas-Reyes 04-Dec-2008  c
c                                                                      c
c     PURPOSE:                                                         c
c     Calculates different indices using the anomaly files             c
c     Deals with land or ocean grid points only when necessary         c
c                                                                      c
c     INPUT:                                                           c
c     ANOM_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_YYYY GRIB files         c
c                                                                      c
c     OUTPUT:                                                          c
c     TS_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_III-EEE_INCODE            c
c                                                   ASCII files        c
c                                                                      c
c     USAGE:                                                           c
c     cal_ts.x   < nlist                                               c
c                                                                      c
c     COMPILING:                                                       c
c     COMPILING:                                                       c
c     pgf90 cal_ts.f tools.f -o cal_ts.x $EMOSLIB $NAGLIB              c
c                                                                      c
c     MODS:                                                            c
c                                         P. Doblas-Reyes 04-Dec-2008  c
c     Based on the original cal_ts.f from DEMETER                      c
c                                                                      c
c----------------------------------------------------------------------c
c
      program cal_ts
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: ano_mod(:,:,:,:), ano_ref(:,:,:,:)
      real, allocatable :: regwgt(:), latwgt(:)
      real, allocatable :: rlat(:), rlon(:)
      real, allocatable :: aver(:,:,:,:), index(:,:,:)
      real, allocatable :: x(:)
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
      integer cros, par, ilev, anin,  nf1,  nf2
      integer nevt, boot, permeth, nsam, iprb, nts
      integer idatetype
      integer lena, istat, exptl
      integer nxny, nensp1, nyear, idat, nummon, nensa
      integer iy, iens, i, imon, ip, il
      integer iunit
      integer iilsm, icatch, npoint, ipoint
      real xlatn, xlats, xlonw, xlone, xpoint
      real eps
      real rmiss
      real rint
      real rlsm, indexem
      real ave, adev, sdev, var, skew, curt
c
c.... hard coded field definitions
c
      integer nts_max
      parameter(nts_max=80)
      character*6 namt(nts_max)
      real lini(nts_max), linl(nts_max)
      real lisi(nts_max), lisl(nts_max)
      real liwi(nts_max), liwl(nts_max)
      real liei(nts_max), liel(nts_max)
      real xdif(nts_max)
      integer iyys(nts_max), iyye(nts_max)
      integer ilsm(nts_max), rcal(nts_max)
      integer ipar(nts_max), ilei(nts_max), ilel(nts_max)
      integer icdf(nts_max), stnd(nts_max)
c
c.... namelist
c
      namelist /control/   nx,   ny, nmon, nens,
     >                   iyys, iyye,  imm,  idd,  itt,
     >                   cros, anin,  nf1,  nf2,
     >                   expt,  nts
      namelist /timeseries/    namt, ipar, ilei, ilel,
     >                   ilsm, icdf, xdif, stnd,
     >                   lini, linl, lisi, lisl,
     >                   liwi, liwl, liei, liel,
     >                   rcal
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
c   iyys:   start year
c   iyye:   end year
c   imm:    forecast starting month
c   expt:   model id
c   ipar:  variable to be analysed
c   ilevi:  initial level
c   ilevl:  end level
c   stad:   month of the corresponding starting date (0 for all dates)
c   moni:   initial month for the averaging (0 for several lead times)
c   monl:   end month for the averaging
c   ilsm:   use of land sea mask
c       1     land points
c       0     no land-sea mask applied
c      -1     sea points
c   icdef: flag for shape of region
c       0     rectangular area 
c       1     non rectangular area (river catchment area)
c   xdif:   compute differences or sums between levels or two different regions
c      -1.    computes differences
c       0.5   does averages
c       0.    takes the value of the last computed level
c   stnd:   standardization of the variables before computing the index (as in SOI)
c       1     performs standardization
c       0     does not
c   limni,limnl,limsi,limsl,limwi,limwl,limei,limel
c           boundaries of the corresponding region:
c               Order: North - South - West - East
c               Given that the regional area for level 1 and
c               level 2 might be different (or to define different
c               areas for the same level) the region has to be
c               defined for each level separately.
c               i stands for first level, l for second one
c
      read(5,control)
      write(*,*)'  nx: ',nx
      write(*,*)'  ny: ',ny
      write(*,*)'nmon: ',nmon
      write(*,*)'nens: ',nens
      write(*,*)' imm: ',imm
      write(*,*)' idd: ',idd
      write(*,*)' itt: ',itt
      write(*,*)'cros: ',cros
      write(*,*)'anin: ',anin
      write(*,*)' nf1: ',nf1
      write(*,*)' nf2: ',nf2
      write(*,*)'expt: ',expt
      write(*,*)' nts: ',nts
c DM this is replicated here, compiles with error, so commenting it to see if this fixes the error
c      namelist /timeseries/    namt, ipar, ilei, ilel,
c     >                   ilsm, icdf, xdif, stnd,
c     >                   lini, linl, lisi, lisl,
c     >                   liwi, liwl, liei, liel,
c     >                   rcal
      if(nts.gt.nts_max)then
        write(*,*)'Problems with the number of time series'
        goto 998
      endif
      write(*,*)'iyys: ',(iyys(i),i=1,nts)
      write(*,*)'iyye: ',(iyye(i),i=1,nts)
      read(5,timeseries)
      write(*,*)'namt: ',(namt(i),i=1,nts)
      write(*,*)'ipar: ',(ipar(i),i=1,nts)
      write(*,*)'ilei: ',(ilei(i),i=1,nts)
      write(*,*)'ilel: ',(ilel(i),i=1,nts)
      write(*,*)'ilsm: ',(ilsm(i),i=1,nts)
      write(*,*)'icdf: ',(icdf(i),i=1,nts)
      write(*,*)'xdif: ',(xdif(i),i=1,nts)
      write(*,*)'stnd: ',(stnd(i),i=1,nts)
      write(*,*)'lini: ',(lini(i),i=1,nts)
      write(*,*)'linl: ',(linl(i),i=1,nts)
      write(*,*)'lisi: ',(lisi(i),i=1,nts)
      write(*,*)'lisl: ',(lisl(i),i=1,nts)
      write(*,*)'liwi: ',(liwi(i),i=1,nts)
      write(*,*)'liwl: ',(liwl(i),i=1,nts)
      write(*,*)'liei: ',(liei(i),i=1,nts)
      write(*,*)'liel: ',(liel(i),i=1,nts)
      write(*,*)'rcal: ',(rcal(i),i=1,nts)
c
      rmiss=-1.e30
      eps=1.e-30
      nensp1 = nens+1
      nxny=nx*ny
      nummon=nf2-nf1+1
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
c.... loop over indices
c
      do ip=1,nts
         if(rcal(ip).eq.1)then
           par=ipar(ip)
           iyy1=iyys(ip)
           iyy2=iyye(ip)
           nyear=iyy2-iyy1+1
c
           allocate (ano_mod(nxny,nmon,nens,nyear),          stat=istat)
           allocate (ano_ref(nxny,nmon,1,nyear),             stat=istat)
           allocate (ivfdate(nmon,nyear),                    stat=istat)
           allocate (istep(nmon,nyear),                      stat=istat)
           allocate (aver(2,2,nyear,nens),                   stat=istat)
           allocate (index(2,nyear,nens),                    stat=istat)
c
           write(*,*)'Computing for region: ',namt(ip)
           iilsm=ilsm(ip)
           call get_mask(iilsm,lsm,nxny,expt)
c          write(*,*)'lsm'
c          write(*,'(40i4)')(lsm(i),i=1,nxny)
c
c.... select parameters for the corresponding region
c.... two levels are considered for those indices which are
c.... computed as the difference of the variable over two 
c.... different regions
c
           do il=1,2
              if(il.eq.1)then
                ilev=ilei(ip)
                xlatn=lini(ip)
                xlats=lisi(ip)
                xlone=liei(ip)
                xlonw=liwi(ip)
              else
                ilev=ilel(ip)
                xlatn=linl(ip)
                xlats=lisl(ip)
                xlone=liel(ip)
                xlonw=liwl(ip)
              endif
c             write(*,*)'region boundaries ',
c    >           xlats,xlatn,xlonw,xlone
              icatch=icdf(ip)
              call get_region(icatch, xlats, xlatn, xlonw, xlone,
     >           nxny, expt, '', regwgt, latwgt,
     >           rlat, rlon, nlat, nlon, rint)
c             write(*,*)'regwgt'
c             write(*,'(40f4.1)')(regwgt(i),i=1,nxny)
c             write(*,*)'latwgt'
c             write(*,'(20f8.3)')(latwgt(i),i=1,nxny)
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
c.... loop over reading model and ref data
c....    idat=1 for model
c....    idat=2 for reference
c
              do idat=1,2
                 expti=expt
                 nensa=nens
                 if(idat.eq.2)then
                   expti = 'refe'
                   nensa=1
                 endif
                 exptl=lena(expti)
                 write(form1,'(a,i2.2,a)')'(a',exptl,')'
c
c.... read all years of anomalies
c
                 do iy=1,nyear
                    yifile=
     >                 'ANOM_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_YYYY'
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
                    write(yifile(32+exptl:34+exptl),'(i3.3)')par
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
     >                    imm, iyy1, iy, par, ilev, idatetype,
     >                    ano_mod(1,1,1,iy), ivfdate(1,iy), istep(1,iy),
     >                    ksec0, ksec1_sav, ksec2, ksec3, ksec4, psec2,
     >                    psec3)
                    endif
                    if(idat.eq.2)then
                      idatetype=2
                      call readgrb(iunit, nxny, nmon, 1, idat,
     >                    imm, iyy1, iy, par, ilev, idatetype,
     >                    ano_ref(1,1,1,iy), ivfdate(1,iy), istep(1,iy),
     >                    ksec0, ksec1, ksec2, ksec3, ksec4, psec2,
     >                    psec3)
                    endif
                    call pbclose(iunit,kret)
                    if(kret.ne.0)then
                      write(*,*)'Error in closing file: kret=',kret
                      goto 997
                    endif
c
                    do iens=1,nensa
                       aver(il,idat,iy,iens)=0.
                       do imon=nf1,nf2
                          ave=0.
                          rlsm=0.
                          do i=1,nxny
                             if(idat.eq.1)then
                               ave=ave+regwgt(i)*lsm(i)*
     >                            ano_mod(i,imon,iens,iy)
                             else
                               ave=ave+regwgt(i)*lsm(i)*
     >                            ano_ref(i,imon,1,iy)
                             endif
                             rlsm=rlsm+regwgt(i)*lsm(i)
                          enddo
                          ave=ave/rlsm
                          aver(il,idat,iy,iens)=
     >                       aver(il,idat,iy,iens)+ave/nummon
                       enddo
                    enddo
                 enddo !iy loop
c
c.... standardizing when required
c
                 if(stnd(ip).eq.1)then
                   allocate (x(nyear*nensa),                 stat=istat)
                   i=0
                   do iy=1,nyear
                      do iens=1,nensa
                         i=i+1
                         x(i)=aver(il,idat,iy,iens)
                      enddo
                   enddo
                   call moment(x, nyear*nensa,
     >                ave, adev, sdev, var, skew, curt)
                   do iy=1,nyear
                      do iens=1,nensa
                         aver(il,idat,iy,iens)=
     >                      (aver(il,idat,iy,iens)-ave)/sdev
                      enddo
                   enddo
                   deallocate (x,                            stat=istat)
                 endif
              enddo !idat loop
           enddo ! il loop
c
c.... computing the indices
c
           do iy=1,nyear
              do idat=1,2
                 nensa=nens
                 if(idat.eq.2)then
                   nensa=1
                 endif
                 do iens=1,nensa
                    if(xdif(ip).lt.0.)then
c
c.... indices as differences among two levels
c
                      index(idat,iy,iens)=aver(1,idat,iy,iens)+
     >                   xdif(ip)*aver(2,idat,iy,iens)
                      write(*,*)'Index values - Lev 1=',
     >                   aver(1,idat,iy,iens),
     >                   ' Lev 2=',aver(2,idat,iy,iens)
                    elseif(xdif(ip).eq.0.5)then
c
c.... indices as averages of two levels
c
                      index(idat,iy,iens)=
     >                   xdif(ip)*(aver(1,idat,iy,iens)+
     >                   aver(2,idat,iy,iens))
                      write(*,*)'Index values - Lev 1=',
     >                   aver(1,idat,iy,iens),
     >                   ' Lev 2=',aver(2,idat,iy,iens)
                    else
                      index(idat,iy,iens)=aver(2,idat,iy,iens)
                      write(*,*)'Value for just one level=',
     >                   aver(2,idat,iy,iens)
                    endif
c
c.... precipitation in mm/day
c
                    write(*,*)'index ',ip,' for year ',iy,
     >                 ' and member ',iens,'=',index(idat,iy,iens)
                 enddo !iens loop
              enddo !idat loop
           enddo !iy loop
c
c.... open output file
c
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
              write(yofile(30+exptl:32+exptl),'(i3.3)')par
              write(yofile(33+exptl:33+exptl),'(a1)')'_'
              write(yofile(34+exptl:36+exptl),'(i3.3)')ilev
              write(yofile(37+exptl:37+exptl),'(a1)')'_'
              write(yofile(38+exptl:40+exptl),'(i3.3)')nf1
              write(yofile(41+exptl:41+exptl),'(a1)')'-'
              write(yofile(42+exptl:44+exptl),'(i3.3)')nf2
              yofile=yofile(1:lena(yofile))//'_'//namt(ip)
              write(*,*)'open output file: ',yofile(1:lena(yofile))
              open(10,file=yofile,form='formatted',status='unknown')
c
c.... indexm is the ensemble mean
c
              if(idat.eq.2)then
                write(form1,'(a,i1.1,a)')'(i4,e12.4)'
                write(*,*)form1
                do iy=1,nyear
                   write(10,form1)iyy1+iy-1,index(idat,iy,1)
                enddo
              else
                write(form1,'(a,i2.2,a)')'(i4,',nensp1,'e12.3)'
                if(nensp1.ge.100)write(form1,'(a,i3.3,a)')
     >             '(i4,',nensp1,'e12.3)'
                write(*,*)form1
                do iy=1,nyear
                   indexem=0.
                   do iens=1,nens
                      indexem=indexem+index(idat,iy,iens)/nens
                   enddo
                   write(10,form1)iyy1+iy-1,
     >                (index(idat,iy,iens),iens=1,nens),indexem
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
         deallocate (aver,                                   stat=istat)
         deallocate (index,                                  stat=istat)
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
