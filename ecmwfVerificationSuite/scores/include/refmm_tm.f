c
c----------------------------------------------------------------------c
c     PROGRAM refmm_tm                    P. Doblas-Reyes, 11-Jan-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     Reads 6-hourly analysis data for T2m, calculates Tmax and Tmin   c
c     and then computes the corresponding monthly mean values          c
c                                                                      c
c     INPUT:                                                           c
c     167_000_CCYYMM files                                             c
c                                                                      c
c     OUTPUT:                                                          c
c     201_000_CCYYMM.mm files                                          c
c     202_000_CCYYMM.mm files                                          c
c                                                                      c
c     USAGE:                                                           c
c     refmm_tm.exe < nlist                                             c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 refmm_tm.f -o refmm_tm.x $EMOSLIB                  c
c                                                                      c
c     MODS:                                                            c
c                                         F. Doblas-Reyes, 26-Jul-2006 c
c     Adapted to the linux machines                                    c
c                                                                      c
c----------------------------------------------------------------------c
c
      program refmm_tm
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
      integer icc, iyy, imm, idd, ipar
      integer iday, iccr, immr, iddr, ihhr, iyyr, istep
      integer koutlen, iunit, ounit
      integer i, iread, ihour, ivar
      real tmax, tmin, rmiss
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
      real psec4_6(klenp,31,4)
      integer kgrib(kleng)
c
      common /grbdef/ ksec0,ksec1,ksec2,ksec3,ksec4,
     >                psec2,psec3,psec4,kgrib
c
      namelist /control/ icc, iyy, imm, idd, ipar
c
c.... set default values and read input namelist
c
      icc = 19
      iyy = 86
      imm = 11
      idd = 30
c
      read(5,control)
      write(*,*)'processing date:'
      write(*,*)'CC = ', icc
      write(*,*)'YY = ', iyy
      write(*,*)'MM = ', imm
      write(*,*)'DD = ', idd
c
c.... set missing data indicators and reset psec fields
c
      rmiss    = -1.e30
      psec3(2) = rmiss
      do i=1,klenp
         psec4(i)    = 0.
      enddo
      ivar=ipar-200
c
c.... open input file
c
      yifile='167_000_CCYYMM'
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
          iday = (iread-1)/4+1
          write(*,*)'Number of days =',iday
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
      if(nxny.ne.ksec4(1))then
        write(6,*)'Error in the number of points ',nxny,ksec4
        goto 997
      endif
      iyyr=ksec1(10)
      immr=ksec1(11)
      iddr=ksec1(12)
      ihhr=ksec1(13)
      iccr=ksec1(21)
      istep = ksec1(16)
c
c.... write the 4 6-hourly data of the day
c
      if(ihhr.eq.0)then
         do i=1,klenp
            psec4_6(i,iddr,1)=psec4(i)
         enddo
         i=1
         write(*,*)iyyr, immr, iddr, ihhr, istep
      endif
      if(ihhr.eq.6)then
         do i=1,klenp
            psec4_6(i,iddr,2)=psec4(i)
         enddo
         i=1
         write(*,*)iyyr, immr, iddr, ihhr, istep
      endif
      if(ihhr.eq.12)then
         do i=1,klenp
            psec4_6(i,iddr,3)=psec4(i)
         enddo
         i=1
         write(*,*)iyyr, immr, iddr, ihhr, istep
      endif
      if(ihhr.eq.18)then
         do i=1,klenp
            psec4_6(i,iddr,4)=psec4(i)
         enddo
         i=1
         write(*,*)iyyr, immr, iddr, ihhr, istep
      endif
c
      goto 10
c
20    continue
      call pbclose(iunit,kret)
c
c.... check whether all data in the particular month are available
c
      if (iday.lt.idd) then
         write(6,*)'not enough dates available...'
         goto 997
      endif
c
c.... compute max/min over 1 day
c
      do iday=1,idd
         do i=1,klenp
            tmax=0.
            tmin=500.
            do ihour=1,4
               tmax=max(tmax,psec4_6(i,iday,ihour))
               tmin=min(tmin,psec4_6(i,iday,ihour))
            enddo
            psec4_6(i,iday,1)=tmax
            psec4_6(i,iday,2)=tmin
         enddo
      enddo
c
c.... set grib headers
c
c.... Variable name
      ksec1(6)=200+ivar
c.... year of reference time
      ksec1(10)=iyyr
c.... month of reference time
      ksec1(11)=immr
c.... day of reference time
      ksec1(12)=1
c.... hour of reference time
      ksec1(13)=0
c.... time unit
      ksec1(15)=2
c.... time range one (P1)
      ksec1(16)=0
c.... time range two (P2)
      ksec1(17)=idd*24
c.... time range indicator 
      ksec1(18)=2
c.... number of products in an average 
      ksec1(19)=4
c.... century
      ksec1(21)=icc+1
      ksec1(21)=iccr
c.... ECMWF local GRIB usage definition identifier
      ksec1(37)=8
c.... interval between reference times (in time units)
      ksec1(42)=6
c
c.... finally open output file and write data to output file
c
      yofile='PAR_000_CCYYMM'
      write(yofile( 1: 3),'(i3.3)')200+ivar
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
c
c.... compute monthly mean
c
      do i=1,klenp
         psec4(i)=0.
         do iday=1,idd
            psec4(i)=psec4(i)+psec4_6(i,iday,ivar)
         enddo
         psec4(i)=psec4(i)/idd
      enddo
c
c.... enconde and write
c
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
