c
c----------------------------------------------------------------------c
c     PROGRAM refmm_an                    P. Doblas-Reyes, 11-Jan-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     Reads daily analyses and calculates the corresponding monthly    c
c     mean values                                                      c
c                                                                      c
c     INPUT:                                                           c
c     PAR_LEV_CCYYMM files                                             c
c                                                                      c
c     OUTPUT:                                                          c
c     PAR_LEV_CCYYMM.mm files                                          c
c                                                                      c
c     USAGE:                                                           c
c     refmm_an.exe < nlist                                             c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 refmm_an.f -o refmm_an.x $EMOSLIB                  c
c                                                                      c
c     MODS:                                                            c
c                                         F. Doblas-Reyes, 12-Jan-2006 c
c     Based on the original e40mm_an.f from DEMETER                    c
c                                         F. Doblas-Reyes, 26-Jul-2006 c
c     Adapted to the linux machines                                    c
c                                                                      c
c----------------------------------------------------------------------c
c
      program refmm_an
c
      implicit none
c
c.... data definitions
c
      integer nx, ny, nxny
      parameter(nx=144,ny=71)
      parameter(nxny=nx*ny) 
c
c.... other stuff
c
      character yifile*14, yofile*17
      character yfile*80
      integer icc, iyy, imm, ipar, ilev
      integer iccr, immr, iddr, ihhr, iyyr
      integer koutlen, iunit, ounit
      integer idat, i, idd, iread
      real rmiss
c
c.... definitions for GRIBEX call
c
      integer jpack, klenp, kleng, jbyte, kword, kret
      parameter (jpack=200000,klenp=nxny,kleng=jpack)
      parameter (jbyte=4) !the number of bytes in an integer
c
      integer ksec0(2)
      integer ksec1(1024)
      integer ksec2(1024)
      integer ksec3(2)
      integer ksec4(512)
c
      real psec2(512)
      real psec3(2)
      real psec4(klenp)
      real psec4sum(klenp)
      integer kgrib(kleng)
c
      common /grbdef/ ksec0,ksec1,ksec2,ksec3,ksec4,
     >                psec2,psec3,psec4,kgrib
c
      namelist /control/ icc, iyy, imm, idd, ipar, ilev
c
c.... set default values and read input namelist
c
      icc  = 19
      iyy  = 86
      imm  = 11
      idd  = 30
      ipar = 129
      ilev = 500
c
      read(5,control)
      write(*,*)'processing date:'
      write(*,*)'CC = ', icc
      write(*,*)'YY = ', iyy
      write(*,*)'MM = ', imm
      write(*,*)'DD = ', idd
      write(*,*)'PAR= ', ipar
      write(*,*)'LEV= ', ilev
c
c.... set missing data indicators and reset psec fields
c
c      rmiss    = -1.e30
      rmiss    = -9999
c.... DMac made change for soil moisture missing values (-9999 is default in grib)
      
      psec3(2) = rmiss
      do i=1,klenp
         psec4(i)    = 0.
         psec4sum(i) = 0.
      enddo
c
c.... open input file
c
      yifile='PAR_LEV_CCYYMM'
      write(yifile( 1: 3),'(i3.3)')ipar
      write(yifile( 5: 7),'(i3.3)')ilev
      write(yifile( 9:10),'(i2.2)')icc
      write(yifile(11:12),'(i2.2)')iyy
      write(yifile(13:14),'(i2.2)')imm
      yfile=yifile
      write(*,*)'open input file: ',yfile
      kret=0
      call pbopen(iunit,yfile,'r',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
c
c.... read all fields from input file
c
      iread=0
 10   continue
      iread=iread+1
      kret=1
      call pbgrib(iunit,kgrib,kleng,koutlen,kret)
      if(kret.lt.0)then
        if(kret.eq.-1)then
          write(*,*)'End of file'
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
     >            psec4,klenp,kgrib,kleng,kword,'D',kret)
      if(kret.gt.0)then
        write(6,*)'Error decoding data: kret=',kret
        goto 997
      endif
      if(iread.eq.1)then
        iyyr=ksec1(10)
        immr=ksec1(11)
        iddr=ksec1(12)
        ihhr=ksec1(13)
        iccr=ksec1(21)
      endif
c
c.... accumulate data
c
      do i=1,klenp
         psec4sum(i)=psec4sum(i)+psec4(i)
      enddo
      i=1
      write(*,*)iread, ksec1(6), psec4(klenp/2), psec4sum(klenp/2)
c
      goto 10
c
20    continue
      call pbclose(iunit,kret)
c
c.... check whether all data in the particular month are available
c
      idat = iread-1
      if (idat.lt.idd) then
         write(6,*)'not enough dates available...'
         goto 997
      endif
c
c.... convert accumulation over month to monthly mean
c
      do i=1,klenp
         psec4(i)=psec4sum(i)/float(iread-1)
      enddo
c
c.... divide radiation flux data by seconds of day
c
      if((ipar.eq.169).or.(ipar.eq.175).or.(ipar.eq.176).or.
     >   (ipar.eq.177).or.(ipar.eq.178).or.(ipar.eq.179))then
         do i=1,klenp
            psec4(i)=psec4(i)/86400.
         enddo
      endif
c
c.... set grib headers
c.... year of reference time
      ksec1(10)=iyyr
c.... month of reference time
      ksec1(11)=immr
c.... day of reference time
      ksec1(12)=iddr
c.... hour of reference time
      ksec1(13)=ihhr
c.... time unit
      ksec1(15)=1
c.... time range one (P1)
      ksec1(16)=0
c.... time range two (P2)
      ksec1(17)=24
c.... time range indicator 
      ksec1(18)=113
c.... number of products in an average 
      ksec1(19)=iread-1
c.... century
      ksec1(21)=icc+1
      ksec1(21)=iccr
c.... ECMWF local GRIB usage definition identifier
      ksec1(37)=8
c.... interval between reference times
      ksec1(42)=24
c
c.... finally open output file and write data to output file
c
      yofile='PAR_LEV_CCYYMM.mm'
      write(yofile( 1: 3),'(i3.3)')ipar
      write(yofile( 5: 7),'(i3.3)')ilev
      write(yofile( 9:10),'(i2.2)')icc
      write(yofile(11:12),'(i2.2)')iyy
      write(yofile(13:14),'(i2.2)')imm
      yfile=yofile
      write(*,*)'open output file: ',yfile
      kret=0
      call pbopen(ounit,yfile,'w',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
      call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,
     >            psec4,klenp,kgrib,kleng,kword,'C',kret)
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
