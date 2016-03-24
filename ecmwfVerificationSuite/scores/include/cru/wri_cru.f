c
c----------------------------------------------------------------------c
c     PROGRAM wri_cru                     F. Doblas-Reyes, 29-Feb-2008 c
c                                                                      c
c     PURPOSE:                                                         c
c     Reads the CRU data from the ASCII files and writes them in GRIB  c
c                                                                      c
c                                                                      c
c     INPUT:                                                           c
c                                                                      c
c     OUTPUT:                                                          c
c     MM_scru_YYYYMM                                                   c
c                                                        GRIB files    c
c                                                                      c
c     USAGE:                                                           c
c     wri_cru.x                                                        c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -g -O3 wri_cru.f -o wri_cru.x $EMOSLIB                     c
c                                                                      c
c     MODS:                                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      program wri_cru
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable:: psec4(:)
      real, allocatable:: psec4c(:)
c
c.... definitions for GRIBEX call
c
      integer kleng
      parameter (kleng=260000)
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
c.... other definitions
c
      character yifile*60, yofile*60
      character*8 exptid8
      integer iunit, ounit, missing
      integer ilon, ilat, i, im, iyy, ilevel, iens, imon
      integer iyy2, nx, ny, imm, nxny
      integer iread, ifield, lena, ngpoint, istat, j
      integer nyear
      real rmiss, t
c
c.... hard coded field definitions
c
      integer nlat, nlon, nmax
      parameter(nlat=360, nlon=720, nmax=100000)
      integer x1(nmax),y1(nmax)
      integer a(nlon,nlat,10,12)
      integer nday(12)
      real b(nlon,nlat,10,12)
      data nday/31,28,31,30,31,30,31,31,30,31,30,31/
c
c.... set default values and read input namelist
c
      nx = 144
      ny = 71
c
      write(*,*)'  nx: ', nx
      write(*,*)'  ny: ', ny
c
      nxny=nx*ny
      klenp=nxny
      ngpoint=nlon*nlat
c
c.... allocate fields
c
      allocate (psec4(klenp),         stat=istat)
      allocate (psec4c(ngpoint),      stat=istat)
c
c.... set missing data indicators and reset fields
c
      rmiss    = -1.e30
      missing  = -999
      psec3(2) = rmiss
c
c.... reading model data
c
      yifile='MM_in'
      write(*,*)'open input file: ',yifile(1:lena(yifile))
      kret=0
      call pbopen(iunit,yifile,'r',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
c
c.... check correct parameter and read all nmon months and nensa ensemble member
c
      iread=0
      imon=0
10    continue
      iread=iread+1
      kret=1
      call pbgrib(iunit,kgrib,kleng,koutlen,kret)
      if(kret.lt.0)then
        if(kret.eq.-1)then
          write(*,*)'...now end of file'
          write(*,*)'Number of read fields =',(iread-1)
          goto 20
        else
          write(*,*)'Error in reading input file: kret=',kret
          write(*,*)'after ',iread,' products'
          goto 997
        endif
      endif
      kret=1
      call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,
     >   psec4,klenp,kgrib,kleng,kword,'D',kret)
      if(kret.gt.0)then
        write(6,*)'Error decoding data: kret=',kret
        goto 997
      endif
      ifield = ksec1(6)
      ilevel = ksec1(8)
      iens   = ksec1(42)
      if(ifield.ne.228.or.ilevel.ne.0)then
        goto 10
      else
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
        goto 20
      endif
c
20    call pbclose(iunit,kret)
c
c.... read CRU data
c
      yifile='cru_ts_2_10.1951-1960.pre'
      do i=1,6
         iyy=1950+(i-1)*10+1
         write(yifile(13:16),'(i4.4)')iyy
         iyy=1950+i*10
         if(i.eq.6)iyy=2002
         write(yifile(18:21),'(i4.4)')iyy
         write(*,*)'open input file: ',yifile
         open(1,file=yifile)
         read(1,*)
         read(1,*)
         read(1,*)
         read(1,*)
         read(1,*)
c
         nyear=10
         if(i.eq.6)nyear=2
         do iyy=1,nyear
            do im=1,12
               do ilat=1,nlat
                  do ilon=1,nlon
                     b(ilon,ilat,iyy,im)=psec3_sav(2)
                  enddo
               enddo
            enddo
         enddo
c
         do j=1,nmax
c           write(*,*)'Read time=',j
            read(1,'(9x,i4,1x,i4)',end=1)x1(j),y1(j)
            do iyy=1,nyear
               read(1,'(12i5)',end=1)(a(x1(j),y1(j),iyy,im),im=1,12)
            enddo
            do im=1,12
               if(a(x1(j),y1(j),1,im).eq.a(x1(j),y1(j),2,im))then
                 do iyy=1,nyear
                    a(x1(j),y1(j),iyy,im)=missing
                 enddo
               endif
            enddo
            do iyy=1,nyear
               do im=1,12
                  if(a(x1(j),y1(j),iyy,im).eq.missing)then
                    b(x1(j),y1(j),iyy,im)=psec3_sav(2)
                  else
                    b(x1(j),y1(j),iyy,im)=a(x1(j),y1(j),iyy,im)/
     >                 (nday(im)*10.)
                  endif
               enddo
            enddo
         enddo
1        continue
c
         do iyy=1,nyear
            iyy2=1950+(i-1)*10+iyy
            write(*,*)iyy2
            do im=1,12
c
c.... open output file
c
               yofile='228_000_YYYYMM_m'
               write(yofile( 9:12),'(i4.4)')iyy2
               write(yofile(13:14),'(i2.2)')im
               write(*,*)'open output file: ',yofile
               kret=0
               call pbopen(ounit,yofile,'w',kret)
               if(kret.ne.0)then
                 write(*,*)'Error in opening file: kret=',kret
                 goto 997
               endif
c
c.... write output
c
               do j=1,1024
                  ksec1(j)=ksec1_sav(j)
                  ksec2(j)=ksec2_sav(j)
               enddo
               ksec0(1)=ksec0_sav(1)
               ksec0(2)=ksec0_sav(2)
               ksec1(1)=128
               ksec1(2)=98
               ksec1(3)=1
               ksec3(1)=ksec3_sav(1)
               ksec3(2)=ksec3_sav(2)
               psec3(1)=psec3_sav(1)
               psec3(2)=psec3_sav(2)
               do j=1,512
                  ksec4(j)=ksec4_sav(j)
                  psec2(j)=psec2_sav(j)
               enddo
               do j=1,kleng
                  kgrib(j)=0
               enddo
c
               ksec1(5)=192
               if(iyy2.le.2000)then
                 ksec1(10)=iyy2-1900
                 ksec1(21)=20
               else
                 ksec1(10)=iyy2-2000
                 ksec1(21)=21
               endif
               ksec1(11)=im
               ksec1(15)=1
               ksec1(16)=nday(im)*24
               ksec1(17)=0
               ksec1(18)=10
               ksec1(22)=98
               ksec1(37)=16
               ksec1(38)=11
               ksec1(39)=80
               ksec1(40)=1221
               ksec1(42)=0
               ksec1(43)=0
               ksec1(44)=0
               ksec1(45)=0
               ksec1(46)=iyy2*100+im
               exptid8="    "//'scru'
               read(exptid8,'(a8)')ksec1(41)
c
               ksec2(2)=nlon
               ksec2(3)=nlat
               ksec2(4)=89750
c              ksec2(4)=90000
               ksec2(5)=0
               ksec2(7)=-89750
c              ksec2(7)=-90000
               ksec2(8)=359500
               ksec2(9)=500
               ksec2(10)=500
c
               ksec4(1)=ngpoint
c
c.... positioning the field from North to South and from Greenwhich
c
c              write(*,*)'Re-ordering'
               do ilon=1,nlon
                  do ilat=1,nlat/2
                     t=b(ilon,ilat,iyy,im)
                     b(ilon,ilat,iyy,im)=b(ilon,nlat-ilat+1,iyy,im)
                     b(ilon,nlat-ilat+1,iyy,im)=t
                  enddo
               enddo
               do ilat=1,nlat
                  do ilon=1,nlon/2
                     t=b(ilon,ilat,iyy,im)
                     b(ilon,ilat,iyy,im)=b(ilon+nlon/2,ilat,iyy,im)
                     b(ilon+nlon/2,ilat,iyy,im)=t
                  enddo
               enddo
c
c              write(*,*)'Writing'
               do ilon=1,nlon
                  do ilat=1,nlat
                     j=ilon+(ilat-1)*nlon
                     psec4c(j)=b(ilon,ilat,iyy,im)
                  enddo
               enddo
               kret=1
               call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,
     >             psec4c,ngpoint,kgrib,kleng,kword,'C',kret)
               if(kret.ne.0)then
                 write(6,*)'Error encoding data: kret=',kret
                 goto 997
               endif
               kret=1
               call pbwrite(ounit,kgrib,kword*4,kret)
               if(kret.lt.0)then
                 write(*,*)'Error in writing file: kret=',kret
                 goto 997
               endif
               call pbclose(ounit,kret)
            enddo
         enddo
      enddo
c
      if(kret.eq.0)goto 998
 997  continue
      write(*,*)'Sorry, no success :('
      goto 999
 998  continue
      write(*,*)'program seems to be successfully finished :)'
 999  continue
c
      end
c
