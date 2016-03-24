c
c----------------------------------------------------------------------c
c                                                                      c
c     COLLECTION OF SUBROUTINES FOR AUTOMATED VERIFICATION SYSTEM      c
c                                                                      c
c----------------------------------------------------------------------c
c
c----------------------------------------------------------------------c
c     SUBROUTINE read1grb                 F. Doblas-Reyes, 17-Aug-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     reads GRIB files with any number of fields                       c
c                                                                      c
c     INPUT:                                                           c
c     iunit:    logical unit of the GRIB file                          c
c     npoint:   number of points                                       c
c                                                                      c
c     OUTPUT:                                                          c
c     psec4:    data array                                             c
c     ?sec?:    GRIB headers (required to write output files in GRIB   c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE read1grb(iunit, npoint, psec4,
     >                   ksec0, ksec1, ksec2, ksec3, 
     >                   ksec4, psec2, psec3)
c
      implicit none
c
      integer iunit, npoint
c
c.... definitions for GRIBEX call
c
      integer kleng, kret, klenp, kword, koutlen
      parameter(kleng=200000)
      integer ksec0(2)
      integer ksec1(1024)
      integer ksec2(1024)
      integer ksec3(2)
      integer ksec4(512)
      integer kgrib(kleng)
      real psec2(512)
      real psec3(2)
      real psec4(npoint)
      real rmiss
c
      rmiss=-1.e30
c
      kret=1
      call pbgrib(iunit,kgrib,kleng,koutlen,kret)
      if(kret.lt.0)then
        if(kret.eq.-1)then
          write(*,*)'Sorry, did not find all expected fields...'
          goto 997
        else
          write(*,*)'Error in reading input file: kret=',kret
          goto 997
        endif
      endif
c
      kret=1
      call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,
     >   psec4,klenp,kgrib,kleng,kword,'I',kret)
      if(kret.gt.0)then
        write(*,*)'Error decoding data: kret=',kret
        goto 997
      endif
      if(kret.eq.-2)then
        write(*,*)'kret=',kret
        write(*,*)'Warning: A bitmap was found with all bits set to 1'
      endif
      if(kret.eq.-3)then
        write(*,*)'kret=',kret
        write(*,*)'Warning: A predefined bitmap was found'
      endif
      if(kret.eq.-4)then
        write(*,*)'kret=',kret
        write(*,*)'Warning: A bitmap was found and missing data ',
     >     'inserted'
      endif
      if((ksec1(5).eq.64).or.(ksec1(5).eq.192))then
        write(*,*)'ksec1(5)=',ksec1(5)
        psec3(2)=rmiss
        call grprs3(ksec0,ksec3,psec3)
      else
        psec3(2)=0.
      endif
c
      klenp=npoint
      kret=1
      call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,
     >   psec4,klenp,kgrib,kleng,kword,'D',kret)
      if(kret.gt.0)then
        write(6,*)'Error decoding data: kret=',kret
        goto 997
      endif
c     call grprs1(ksec0,ksec1)
c     call grprs2(ksec0,ksec2,psec2)
c     call grprs3(ksec0,ksec3,psec3)
c     call grprs4(ksec0,ksec4,psec4)
c
      goto 998
 997  continue
      write(*,*)'Sorry, no success :('
      stop
 998  continue
      write(*,*)'the read1grb routine has successfully finished :)'
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE writegrb                 F. Doblas-Reyes, 09-Jan-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     writes GRIB files with any number of fields                      c
c                                                                      c
c     INPUT:                                                           c
c     ounit:    logical unit of the GRIB file                          c
c     npoint:   number of points                                       c
c     psec4:    data array                                             c
c     ?sec?:    GRIB headers (required to write output files in GRIB   c
c                                                                      c
c     OUTPUT:                                                          c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE writegrb(ounit, npoint, psec4,
     >                   ksec0, ksec1, ksec2, ksec3, 
     >                   ksec4, psec2, psec3)
c
      implicit none
c
      integer ounit, npoint
c
c.... definitions for GRIBEX call
c
      integer kleng, kret, klenp, kword
      parameter(kleng=200000)
      integer ksec0(2)
      integer ksec1(1024)
      integer ksec2(1024)
      integer ksec3(2)
      integer ksec4(512)
      integer kgrib(kleng)
      real psec2(512)
      real psec3(2)
      real psec4(npoint)
c
      klenp=npoint
c
      call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,
     >   psec4,klenp,kgrib,kleng,kword,'C',kret)
      if(kret.ne.0)then
        write(6,*)'Error encoding data: kret=',kret
        goto 997
      endif
c     call grprs1(ksec0,ksec1)
c     call grprs2(ksec0,ksec2,psec2)
c     call grprs4(ksec0,ksec4,psec4)
      kret=1
      call pbwrite(ounit,kgrib,kword*4,kret)
      if(kret.eq.-1)then
        write(*,*)'Error writing to the file: kret=',kret
        goto 997
      endif
c
      goto 998
 997  continue
      write(*,*)'Sorry, no success :('
      stop
 998  continue
      write(*,*)'the writegrb routine has successfully finished :)'
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE readgrb                  F. Doblas-Reyes, 06-Jan-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     reads GRIB files with any number of fields                       c
c                                                                      c
c     INPUT:                                                           c
c     iunit:    logical unit of the GRIB file                          c
c     npoint:   number of points                                       c
c     nmon:     number of months                                       c
c     nens:     number of ensemble members (1 for reference data)      c
c     idat:     type of data (1 for simulation, 2 for reference)       c
c     imm:      forecast start month                                   c
c     iyy1:     forecast start year                                    c
c     ipar:     parameter to be read                                   c
c     ilev:     level of the parameter to be read                      c
c     idatetype: 1 if the dates are taken from the ksec1(10-11) and    c
c                2 if taken from ksec1(46); 0 for model data           c
c                                                                      c
c     OUTPUT:                                                          c
c     field:    data array                                             c
c     ?sec?:    GRIB headers (required to write output files in GRIB   c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE readgrb(iunit, npoint, nmon, nens, idat,
     >                   imm, iyy1, iy, ipar, ilev, idatetype,
     >                   field, ivfdate, istep,
     >                   ksec0, ksec1_sav, ksec2, ksec3, 
     >                   ksec4, psec2, psec3)
c
      implicit none
c
      integer npoint, nmon, nens, idat, imm, iyy1, idatetype
      integer nfield
      integer ikept, iread, iunit
      integer imon, i, iy, ivfmon, istmon, ivfyear, istyear, ik
      integer ilev, ipar, iens, nensm1, iensm1, ilevel, ifield
      integer ivfdate(nmon), istep(nmon)
      real field(npoint,nmon,nens)
c
c.... definitions for GRIBEX call
c
      integer kleng, koutlen, kret, klenp, kword
      parameter(kleng=200000)
      integer ksec0(2)
      integer ksec1(1024)
      integer ksec1_sav(1024)
      integer ksec2(1024)
      integer ksec3(2)
      integer ksec4(512)
      integer kgrib(kleng)
      real psec2(512)
      real psec3(2)
      real psec4(npoint)
      real rmiss
c
      rmiss=-1.e30
      nfield=nmon*nens
      klenp=npoint
      nensm1=nens-1
c
c.... search for correct fields
c
      iread=0
      ikept=0
10    continue
      iread=iread+1
      kret=1
      call pbgrib(iunit,kgrib,kleng,koutlen,kret)
      if(kret.lt.0)then
        if(kret.eq.-1)then
          write(*,*)'Sorry, did not find all expected fields...'
          write(*,*)'...now end of file'
          write(*,*)'Number of fields read =',(iread-1)
          goto 997
        else
          write(*,*)'Error in reading input file: kret=',kret
          write(*,*)'after ',iread,' products'
          goto 997
        endif
      endif
      kret=1
      call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,
     >   psec4,klenp,kgrib,kleng,kword,'I',kret)
      if(kret.gt.0)then
        write(*,*)'Error decoding data: kret=',kret
        goto 997
      endif
      if((ksec1(5).eq.64).or.(ksec1(5).eq.192))then
        write(*,*)'ksec1(5)=',ksec1(5)
        write(*,*)'psec3(2)=',psec3(2)
        do i=1,klenp
           if(psec4(i).eq.psec3(2))then
             psec4(i)=rmiss
           endif
        enddo
        psec3(2)=rmiss
        call grprs3(ksec0,ksec3,psec3)
      else
        psec3(2)=0.
      endif
c     call grprs1(ksec0,ksec1)
c     call grprs2(ksec0,ksec2,psec2)
c     call grprs3(ksec0,ksec3,psec3)
c     call grprs4(ksec0,ksec4,psec4)
      if(ksec2(2)*ksec2(3).ne.npoint)then
        write(*,*)'Error in the number of points expected ',
     >     ksec2(2)*ksec2(3),npoint
        goto 997
      endif
      ifield=ksec1(6)
      ilevel=ksec1(8)
      if((ifield.eq.167).and.(ilevel.ne.0))then
        write(*,*)'Level changed from ',ilevel,' to 0'
        ilevel=0
      endif
      iensm1=ksec1(42)
      iens=ksec1(42)+1
      if(idat.eq.2)then
        iens=1
        iensm1=0
      endif
      if((ifield.ne.ipar).or.(ilevel.ne.ilev).or.
     >   (iensm1.gt.nensm1))then
        goto 10
      else
        kret=1
        call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,
     >     psec4,klenp,kgrib,kleng,kword,'D',kret)
        if(kret.gt.0)then
          write(*,*)'Error decoding data: kret=',kret
          goto 997
        endif
        if(kret.eq.-2)then
          write(*,*)'kret=',kret
          write(*,*)'Warning: A bitmap was found with all bits set to 1'
        endif
        if(kret.eq.-3)then
          write(*,*)'kret=',kret
          write(*,*)'Warning: A predefined bitmap was found'
        endif
        if(kret.eq.-4)then
          write(*,*)'kret=',kret
          write(*,*)'Warning: A bitmap was found and missing data ',
     >       'inserted'
        endif
        if(kret.eq.-5)then
          write(*,*)'kret=',kret
          write(*,*)'Warning: A bitmap was found and data not decoded'
        endif
        if(kret.eq.-6)then
          write(*,*)'kret=',kret
          write(*,*)'Warning: ECMWF pseudo-GRIB data was found'
        endif
        kret=0
c       write(*,'(a,i7,a,i3)')'imon=',imon,' iens=',iens
c
c.... special fields for model data
c
        if(idat.eq.1)then
          istyear=(ksec1(21)-1)*100+ksec1(10)
          istmon=ksec1(11)
          ivfyear=ksec1(46)/100
          ivfmon=ksec1(46)-ivfyear*100
          if(ivfmon.lt.istmon)then
            ivfmon=ivfmon+12
            ivfyear=ivfyear-1
          endif
          imon=(ivfyear-istyear)*12+ivfmon-istmon+1
          if(imon.gt.nmon)goto10
          ikept=ikept+1
          if(iensm1.eq.0)then
            ivfdate(imon)=ksec1(46)
            istep(imon)=ksec1(16)
          endif
c         ivfdate(imon)=((ksec1(21)-1)*100+ksec1(10))*100+ksec1(11)
          write(*,'(a,6i7,a,i3)')'imon=',imon,ivfyear,istyear,
     >       ivfmon,istmon,ivfdate(imon),' iens=',iens
          do i=1,klenp
             field(i,imon,iens)=psec4(i)
          enddo
          if((imon.eq.1).and.(iy.eq.1).and.(iens.eq.1))then
            do ik=1,1024
               ksec1_sav(ik)=ksec1(ik)
            enddo
          endif
          if((imon.eq.nmon).and.(iensm1.eq.nensm1))then
            goto 20
          else
            goto 10
          endif
        endif
c
c.... special fields for reference data
c
        if(idat.eq.2)then
          istyear=iyy1+iy-1
          istmon=imm
          if(idatetype.eq.1)then
            ivfyear=(ksec1(21)-1)*100+ksec1(10)
            ivfmon=ksec1(11)
          elseif(idatetype.eq.2)then
            ivfyear=ksec1(46)/100
            ivfmon=ksec1(46)-ivfyear*100
          endif
          imon=(ivfyear-istyear)*12+ivfmon-istmon+1
          if(imon.gt.nmon)goto10
c         ivfdate(imon)=((ksec1(21)-1)*100+ksec1(10))*100+ksec1(11)
          ivfdate(imon)=ivfyear*100+ivfmon
          write(*,'(a,6i7)')'imon=',imon,ivfyear,istyear,
     >       ivfmon,istmon,ivfdate(imon)
          ikept=ikept+1
          do i=1,klenp
             field(i,imon,1)=psec4(i)
          enddo
          if((imon.eq.1).and.(iy.eq.1))then
            do ik=1,1024
               ksec1_sav(ik)=ksec1(ik)
            enddo
          endif
          if(imon.eq.nmon)then
            goto 20
          else
            goto 10
          endif
        endif
      endif
20    continue
      if(ikept.ne.nfield)then
        write(*,*)'Wrong number of fields read ',ikept,nfield
        goto 997
      endif
      if(kret.eq.0)goto 998
 997  continue
      write(*,*)'Sorry, no success :('
      stop
 998  continue
      write(*,*)'the readgrb routine has successfully finished :)'
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE moment                        R.Hagedorn, 07-Jan-2002 c
c                                                                      c
c     PURPOSE:                                                         c
c     computes basic statistical parameter of a given field            c
c                                                                      c
c     INPUT:                                                           c
c     data: data vector                                                c
c     n   : size of data                                               c
c                                                                      c
c     OUTPUT:                                                          c
c     ave : mean                                                       c
c     adev: average deviation                                          c
c     sdev: standard deviation                                         c
c     var : variance                                                   c
c     skew: skewness                                                   c
c     curt: curtosis                                                   c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE moment(data, n, ave, adev, sdev, var, skew, curt)
c
      implicit none
c
      integer n, i
      real*4 data(n)
      real*4 ave, adev, sdev, var, skew, curt
      real*8 eps, hlp, sum, ano
      real*8 sdev_d, var_d, skew_d, curt_d
c
      eps = 1.d-30
      sum = 0.
      do i=1,n
         sum = sum+data(i)
      enddo
      ave = sum/float(n)
c
      adev   = 0.
      var_d  = 0.
      skew_d = 0.
      curt_d = 0.
      sum    = 0.
      do i=1,n
         ano    = data(i)-ave
         sum    = sum+ano
         adev   = adev+abs(ano)
         hlp    = ano*ano
         var_d  = var_d+hlp
         hlp    = ano*ano*ano
         skew_d = skew_d+hlp
         hlp    = ano*ano*ano*ano
         curt_d = curt_d+hlp
      enddo
      adev = adev/float(n)
      var_d  = (var_d-sum**2/n)/float(n-1)
      var_d  = max(eps,var_d)
      sdev_d = sqrt(var_d)
      skew_d = skew_d/(n*sdev_d**3)
      curt_d = curt_d/(n*var_d**2)-3.
c
      sdev = max(eps,sdev_d)
      var  = max(eps, var_d)
      skew = max(eps,skew_d)
      curt = max(eps,curt_d)
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE get_mask                      R.Hagedorn, 12-Sep-2001 c
c                                                                      c
c     PURPOSE:                                                         c
c     reads model specific land sea mask and                           c
c     and prepares lsm(nxny) according to iilsm flag                   c
c                                                                      c
c     INPUT:                                                           c
c     iilsm: flag for use of land or sea data                          c
c           1 = only land points                                       c
c           0 = land + sea points                                      c
c          -1 = only sea points                                        c
c     nxny: size of rlsm                                               c
c     expt: experiment identifier                                      c
c                                                                      c
c     OUTPUT:                                                          c
c     lsm: (0/1) mask                                                  c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE get_mask(iilsm, lsm, nxny, expt)
c
      integer lsm(nxny)
      integer iilsm
      integer lena
      character*21 expt
      character*120 ylsmfile
c
c.... definitions for GRIBEX call
c
      parameter (kleng=200000)
c
      integer ksec0(2)
      integer ksec1(1024)
      integer ksec2(1024)
      integer ksec3(2)
      integer ksec4(512)
c
      real psec2(512)
      real psec3(2)
      real psec4(nxny)
      integer kgrib(kleng)
c
      kret=0
      klenp=nxny
      if(iilsm.eq.0)then
        write(*,*)'setting land-sea mask to 1.'
        do i=1,klenp
           lsm(i)=1
        enddo
      else
        write(*,*)'Reading land-sea mask:'
        ylsmfile='LSM_'
        ylsmfile=ylsmfile(1:lena(ylsmfile))//expt(1:lena(expt))
        write(*,*)'Land-sea mask file ',ylsmfile(1:lena(ylsmfile))
        kret=1
        call pbopen(ilsmunit,ylsmfile,'r',kret)
        if(kret.ne.0)then
          write(*,*)'Error in opening file: kret=',kret
          goto 997
        endif
        kret=1
        call pbgrib(ilsmunit,kgrib,kleng,koutlen,kret)
        if(kret.ne.0)then
          write(*,*)'Error in the fields: kret=',kret
          goto 997
        endif
        kret=1
        call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,
     >       ksec4,psec4,klenp,kgrib,kleng,kword,'D',kret)
        if(kret.gt.0)then
          write(6,*)'Error decoding data: kret=',kret
          goto 997
        endif
        call pbclose(ilsmunit,kret)
c
c.... to ensure that land grid points are taken as 1, 0.499 is added up
c
        if(iilsm.eq.1)then
          do i=1,klenp
             lsm(i)=int(psec4(i)+0.499)
          enddo
        endif
        if(iilsm.eq.-1)then
          do i=1,klenp
             lsm(i)=1-int(psec4(i)+0.499)
          enddo
        endif
      endif
c
      if(kret.eq.0)goto 998
 997  continue
      write(*,*)'Sorry, no success :('
      goto 999
 998  continue
      write(*,*)'get_mask seems to be successfully finished :)'
      return
 999  continue
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE get_region                    R.Hagedorn, 12-Sep-2001 c
c                                                                      c
c     PURPOSE:                                                         c
c     creates a mask for specific regions, either                      c
c     - by given lat/lon boundaries (for rect areas) or                c
c     - by a given catchment area mask                                 c
c                                                                      c
c     INPUT:                                                           c
c     icatch: flag whether region is                                   c
c             rectangular area (icatch=0) or                           c
c             number of catchment area (icatch>0)                      c
c     rlats:  lower latitude boundary                                  c
c     rlatn:  upper latitude boundary                                  c
c     rlonw:  left  longitude boundary                                 c
c     rlone:  right longitude boundary                                 c
c     nxny:   size of regwgt and latwgt                                c
c     expt:   experiment identifier                                    c
c     ybasedisk: disk basename                                         c
c                                                                      c
c     OUTPUT:                                                          c
c     regwgt: weight of the grid point (0. to 1. depending on what     c
c             part of the grid box lays inside the region              c
c     latwgt: weight of the grid point (0. to 1. depending on          c
c             the latitude of the grid point (cos theta)               c
c     xlat:   array with the latitudes of the selected points          c
c     xlon:   array with the longitudes of the selected points         c
c     nlat:   number of latitudes                                      c
c     nlon:   number of longitudes                                     c
c     zdlat:  latitude increment                                       c
c                                                                      c
c     MODS:                                                            c
c                                         F. Doblas-Reyes, 02-Jun-2003 c
c     Array with latitudes and longitudes and number of latitudes      c
c     and longitudes included                                          c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE get_region(icatch, rlats, rlatn, rlonw, rlone,
     >                      nxny, expt, ybasedisk, regwgt, latwgt,
     >                      xlat, xlon, nlat, nlon, zdlat)
c
      implicit none
c
      real    rlats, rlatn, rlonw, rlone
      integer ilats, ilatn, ilonw, ilone
      integer ilonw1, ilonw2, ilone1, ilone2
      integer nxny, nlon, nlat, nlon_new, nlat_new, lat, lon
      integer icatch, lena
      integer i, i2reg, ilsmunit, icunit
      real    pi, regwgt(nxny), latwgt(nxny), xlat(nxny), xlon(nxny)
      real xfac, zlatn, zlonw, zdlat, zdlon
      real rlone1, rlone2, rlonw1, rlonw2, rlatdeg, rlatbog
      character*21 expt
      character*120 ylsmfile
      character ybasedisk*80
c
c.... definitions for GRIBEX call
c
      integer kleng, klenp, kret, koutlen, kword
      parameter (kleng=200000)
c
      integer ksec0(2)
      integer ksec1(1024)
      integer ksec2(1024)
      integer ksec3(2)
      integer ksec4(512)
c
      real psec2(512)
      real psec3(2)
      real psec4(nxny), psec4c(nxny)
      integer kgrib(kleng)
c
      klenp = nxny
      pi    = 4.*asin(1./sqrt(2.))
c
c.... reset wgt fields
c
      do i=1,klenp
         regwgt(i) = 0.
         latwgt(i) = 0.
      enddo
c
c.... read grid definitions
c
c     write(*,*)'Reading land sea mask file for grid definitions:'
      ylsmfile='LSM_'
      ylsmfile=ylsmfile(1:lena(ylsmfile))//expt(1:lena(expt))
      write(*,*)'Land-sea mask file ',ylsmfile(1:lena(ylsmfile))
      kret=1
      call pbopen(ilsmunit,ylsmfile,'r',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
      kret=1
      call pbgrib(ilsmunit,kgrib,kleng,koutlen,kret)
      if(kret.ne.0)then
        write(*,*)'Error in the fields: kret=',kret
        goto 997
      endif
      kret=1
      call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,
     >     ksec4,psec4,klenp,kgrib,kleng,kword,'D',kret)
      if(kret.gt.0)then
        write(6,*)'Error decoding data: kret=',kret
        goto 997
      endif
      call pbclose(ilsmunit,kret)
c
c.... set grid definitions
c
      nlon  = ksec2(2)        ! Number of longitudes
      nlat  = ksec2(3)        ! Number of latitudes
      xfac  = 1000.
      zlatn = ksec2(4)/xfac   ! Northernmost grid point
      zlonw = ksec2(5)/xfac   ! Westernmost grid point
      zdlon = ksec2(9)/xfac   ! Longitude increment
      zdlat = ksec2(10)/xfac  ! Latitude increment
      if (ksec2(11).eq.0) zdlat=-zdlat
c
      write(*,*)'rlonw: ',rlonw
      write(*,*)'rlone: ',rlone
      if(rlonw.lt.0.)then
        rlonw=rlonw+360.
      endif
      if(rlone.lt.0.)then
        rlone=rlone+360.
      endif
      write(*,*)'update of rlonw/rlone'
      write(*,*)'rlonw: ',rlonw
      write(*,*)'rlone: ',rlone
c
c.... search for indices corresponding to limits
c
      i2reg=0
c
c.... if area lies over the Greenwich 0 deg line divide into 2 areas
c
      if(rlonw.gt.rlone)then
        i2reg = 1
        rlonw1 = rlonw
        rlonw2 = zlonw+(nlon-1)*zdlon
        rlone1 = zlonw
        rlone2 = rlone
        write(*,*)'separated in 2 regions'
        write(*,*)'rlonw12: ',rlonw1,rlonw2
        write(*,*)'rlone12: ',rlone1,rlone2
        ilonw1=int((rlonw1-zlonw)/zdlon)+1
        ilonw2=int((rlonw2-zlonw)/zdlon)+1
        ilone1=int((rlone1-zlonw)/zdlon)+1
        ilone2=int((rlone2-zlonw)/zdlon)+1
        write(*,*)'ilonw12: ',ilonw1,ilonw2
        write(*,*)'ilone12: ',ilone1,ilone2
        nlon_new = ilonw2-ilonw1+1 + ilone2-ilone1+1
        write(*,*)'nlon:    ',nlon_new
      endif
c
c.... if area doesn't lie over the Greenwich 0 deg line, we have only 1 area
c
      if(i2reg.eq.0)then
        ilonw=max(int((rlonw-zlonw)/zdlon)+1,1)
        ilone=min(int((rlone-zlonw)/zdlon)+1,nlon)
        write(*,*)'ilonw: ',ilonw
        write(*,*)'ilone: ',ilone
        nlon_new = ilone-ilonw+1
        write(*,*)'nlon:  ',nlon_new
      endif
c
c.... search north/south limits
c
      write(*,*)'rlatn: ',rlatn
      write(*,*)'rlats: ',rlats
      ilatn=max(int((rlatn-zlatn)/zdlat)+1,1)
      ilats=min(int((rlats-zlatn)/zdlat)+1,nlat)
      write(*,*)'ilatn: ',ilatn
      write(*,*)'ilats: ',ilats
      nlat_new = ilats-ilatn+1
      write(*,*)'nlat:  ',nlat_new
c
c.... depending on icatch set regwgt and calculate weight of grid point
c
      if (icatch.eq.0) then
         if(i2reg.eq.0)then
           do i=1,klenp
              lat=(i-1)/nlon+1
              lon=i-(lat-1)*nlon
              if(lat.ge.ilatn .and. lat.le.ilats)then
              if(lon.ge.ilonw .and. lon.le.ilone)then
                regwgt(i) = 1.
                rlatdeg   = zlatn+zdlat*(lat-1)
                rlatbog   = (pi/180.)*rlatdeg
                latwgt(i) = cos(rlatbog)
                xlat(i)   = rlatdeg
                xlon(i)   = zlonw+zdlon*(lon-1)
c                write(*,*)rlatbog, rlatdeg, latwgt(i), regwgt(i)
              endif
              endif
           enddo
         endif
         if(i2reg.eq.1)then
           do i=1,klenp
              lat=(i-1)/nlon+1
              lon=i-(lat-1)*nlon
              if(lat.ge.ilatn .and. lat.le.ilats)then
              if((lon.ge.ilone1 .and. lon.le.ilone2) .or.
     >           (lon.ge.ilonw1 .and. lon.le.ilonw2))then
                regwgt(i) = 1.
                rlatdeg   = zlatn+zdlat*(lat-1)
                rlatbog   = (pi/180.)*rlatdeg
                latwgt(i) = cos(rlatbog)
                xlat(i)   = rlatdeg
                if((lon.ge.ilone1).and.(lon.le.ilone2))
     >            xlon(i)   = zlonw+zdlon*(lon-ilone1)
                if((lon.ge.ilonw1).and.(lon.le.ilonw2))
     >            xlon(i)   = zlonw+zdlon*(lon-ilonw1)
c               write(*,*)rlatbog, rlatdeg, latwgt(i), regwgt(i)
              endif
              endif
           enddo
         endif
      endif
c
c.... read catchment area file and set regwgt
c
      if (icatch.gt.0) then
         ylsmfile='CSE_MASK_0000.grb'
         write(ylsmfile(10:13),'(i4.4)')icatch
         ylsmfile='const/'//ylsmfile(1:17)
         write(*,*)ylsmfile
         kret=1
         call pbopen(ilsmunit,ylsmfile,'r',kret)
         if(kret.ne.0)then
           write(*,*)'Error in opening file: kret=',kret
           goto 997
         endif
         kret=1
         call pbgrib(ilsmunit,kgrib,kleng,koutlen,kret)
         if(kret.ne.0)then
           write(*,*)'Error in the fields: kret=',kret
           goto 997
         endif
         kret=1
         call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,
     >        ksec4,psec4c,klenp,kgrib,kleng,kword,'D',kret)
         if(kret.gt.0)then
           write(6,*)'Error decoding data: kret=',kret
           goto 997
         endif
         call pbclose(ilsmunit,kret)
         do i=1,klenp
            regwgt(i) = psec4c(i)
            if (regwgt(i).gt.0.) then
               rlatdeg   = zlatn+zdlat*(lat-1)
               rlatbog   = (pi/180.)*rlatdeg
               latwgt(i) = cos(rlatbog)
c              write(*,*)rlatbog, rlatdeg, latwgt(i), regwgt(i)
            endif
         enddo
      endif
c
      if(kret.eq.0)goto 998
 997  continue
      write(*,*)'Sorry, no success :('
      goto 999
 998  continue
      write(*,*)'get_region seems to be successfully finished :)'
      return
 999  continue
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE varcovar                 F. Doblas-Reyes, 03-Feb-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     computes variances and covariances of two arrays                 c
c                                                                      c
c     INPUT:                                                           c
c     x   : first data vector                                          c
c     y   : second data vector                                         c
c     n   : size of data                                               c
c     weight: weights to apply to the data                             c
c     mtag: whether remove the mean (1) or not (0)                     c
c                                                                      c
c     OUTPUT:                                                          c
c     sdx : standard deviation of array x                              c
c     sdy : standard deviation of array y                              c
c     cov : covariance                                                 c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE varcovar(x, y, n, weight, mtag, sdx, sdy, cov)
c
      implicit none
c
      integer n, mtag, i
      real*4 x(n), y(n), weight(n)
      real*4 sumx, sumy, sdx, sdy, cov, totweight
c
      sumx=0.
      sumy=0.
      if(mtag.eq.1)then
        totweight=0.
        do i=1,n
           sumx=sumx+weight(i)*x(i)
           sumy=sumy+weight(i)*y(i)
           totweight=totweight+weight(i)
        enddo
        sumx=sumx/totweight
        sumy=sumy/totweight
      endif
c
      sdx=0.
      sdy=0.
      cov=0.
      totweight=0.
      do i=1,n
         totweight=totweight+weight(i)
         sdx=sdx+weight(i)*(x(i)-sumx)**2
         sdy=sdy+weight(i)*(y(i)-sumy)**2
         cov=cov+weight(i)*(x(i)-sumx)*(y(i)-sumy)
      enddo
c     sdx=sqrt(sdx/(totweight-1))
c     sdy=sqrt(sdy/(totweight-1))
      sdx=sqrt(sdx/totweight)
      sdy=sqrt(sdy/totweight)
c     cov=cov/(totweight-1)
      cov=cov/totweight
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE divergence                   R. Hagedorn, 11-Mar-2002 c
c                                                                      c
c     PURPOSE:                                                         c
c     calculates divergence from u and v grid point input              c
c                                                                      c
c     INPUT:                                                           c
c     u      : u-component of wind vector                              c
c     v      : v-component of wind vector                              c
c     nx     : number of longitudes                                    c
c     ny     : number of latitudes                                     c
c     xtop   : northern most latitude                                  c
c                                                                      c
c     OUTPUT:                                                          c
c     div    : divergence                                              c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE divergence(u,v,nx,ny,xtop,div)
      implicit none
c
      real u(nx,ny), v(nx,ny), div(nx,ny)
      real xtop, pi, radius, conv, dlon, dlat, xfac
      real xlat, x_dis, y_dis, upart, vpart
      integer nx, ny, jlat, jlon, nlon, nlat
      integer jlonm1, jlonp1, jlatm1, jlatp1
c
      nlon = nx
      nlat = ny
c
c.... some constants
c
      write(*,*)'in divergence: xtop = ',xtop
      pi     = 3.14159
      radius = 6.37e+6
      conv   = atan(1.)/45.
c
      dlon = 360./float(nlon)
      dlat = -dlon
      xfac = 2.*dlat/360. * 2.*pi*radius
c
      do jlat=2,nlat-1
         jlatm1 = jlat-1
         jlatp1 = jlat+1
         xlat   = (xtop+(jlat-1)*dlat)*conv
         do jlon=1,nlon
            jlonm1 = jlon-1+nlon
            jlonp1 = jlon+1
            jlonm1 = 1+mod((jlonm1-1),nlon)
            jlonp1 = 1+mod((jlonp1-1),nlon)
            x_dis = xfac * cos(xlat)
            y_dis = xfac
            upart = (u(jlonp1,jlat)-u(jlonm1,jlat))/x_dis
            vpart = (v(jlon,jlatm1)-v(jlon,jlatp1))/y_dis
            div(jlon,jlat) = (upart+vpart)/2.
         enddo
      enddo
c
c.... FOR THE TOP SOUTH AND NORTH LATITUDES TAKE THE SAME AS THE CLOSEST ONES
c
      do jlon=1,nlon
         div(jlon,1)=div(jlon,2)
         div(jlon,nlat)=div(jlon,nlat-1)
      enddo
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE percen                   P. Doblas-Reyes, 15-Nov-2001 c
c                                                                      c
c     PURPOSE:                                                         c
c     computes the percentiles of a time series using:                 c
c     1) a kernel estimate of the pdf                                  c
c     2) a simple counting of the data and interpolation to the        c
c            required percentiles                                      c
c                                                                      c
c     INPUT:                                                           c
c     x      : time series containing the data                         c
c     ndim   : dimension for the number of data                        c
c     n      : number of data                                          c
c     ncla   : number of classes                                       c
c     permeth: method used to compute the percentiles as               c
c              described in the routine header with 1 for              c
c              kernel method and 0 for counting                        c
c                                                                      c
c     OUTPUT:                                                          c
c     t   : percentiles (to be used as classes thresholds)             c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE percen(x,ndim,n,t,ncla,permeth)
c
      implicit none
c
      integer permeth, ndim, n, ncla
      integer jcla, jx, ordir, i, ii
c
c.... The parameter constants are:
c.... nx:    number of bins used
c.... x0:    first (normalized) central value for the bins
c.... sigma: smoothness parameter
c.... xint:  bin width
c
      integer nx
      real x0, xint, sigma, xm, sd, xx
      real xcla, xi, epsilon
      parameter(nx=201,x0=-3.,xint=((-2.*x0)/nx),sigma=0.25)
c
      real x(ndim), y(ndim)
      real pdf(nx), cum(nx)
      real t(ncla-1), t1(ncla-1), t2(ncla-1)
      integer tag(ncla-1), nr(n)
c
      epsilon=tiny(epsilon)
c
c.... First method: kernel-based estimate
c
      do jcla=1,ncla-1
         tag(jcla)=0
      enddo
c
c.... estimating the PDF
c
c     write(*,*)'Call to Gauss'
      call kergauss(x,ndim,n,nx,x0,xint,sigma,xm,sd,pdf)
c     write(*,*)'Mean and sd',xm,sd,xint
c
c.... computing percentiles
c
      cum(1)=pdf(1)
      do jx=2,nx
         cum(jx)=cum(jx-1)+pdf(jx)
      enddo
      do jx=1,nx
         cum(jx)=cum(jx)/cum(nx)
      enddo
c     write(*,*)((jx,cum(jx)),jx=1,nx)
      xx=x0
      do jx=1,nx
         do jcla=1,ncla-1
            xcla=float(jcla)/ncla
            if((cum(jx).ge.xcla).and.(tag(jcla).eq.0))then
              tag(jcla)=1
              t1(jcla)=xm+sd*xx
            endif
         enddo
         xx=xx+xint
      enddo
c
c.... Second method: counting
c
c     write(*,*)'Call to counting'
c
c.... the data are ranked in increasing order
c
c.... ordir 1  : ascending
c.... ordir -1 : descending
      ordir=1
      call sort(ordir,ndim,n,x,y)
c     write(*,*)((i,x(i)),i=1,n)
c     write(*,*)((i,y(i)),i=1,n)
c
c.... computing percentiles
c
      do jcla=1,ncla-1
         ii=(jcla*(n+1))/ncla
         ii=(jcla*n)/ncla
         if(ii.eq.0)ii=1
         xi=amod(float(jcla*n),float(ncla))
         xi=float(jcla*(n+1))/ncla-jcla*(n+1)/ncla
         t2(jcla)=(1.-xi)*y(ii)+xi*y(ii+1)+epsilon
      enddo
c
c.... Selecting the final values
c
c     write(*,*)'percentiles'
c     write(*,*)(t1(jcla),jcla=1,ncla-1)
c     write(*,*)(t2(jcla),jcla=1,ncla-1)
      do jcla=1,ncla-1
         t(jcla)=permeth*t1(jcla)+(1-permeth)*t2(jcla)
      enddo
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE kergauss                 P. Doblas-Reyes, 15-Nov-2001 c
c                                                                      c
c     PURPOSE:                                                         c
c     computes an estimate of the PDF using a Gaussian kernel          c
c                                                                      c
c     INPUT:                                                           c
c     x   : data                                                       c
c     ndim: dimension for the number of data                           c
c     n   : number of data                                             c
c     nx  : number of bins                                             c
c     x0  : first abscissa for the PDF                                 c
c     xint: interval of bins                                           c
c     sigma: weight for the kernel                                     c
c                                                                      c
c     OUTPUT:                                                          c
c     xm  : sample mean                                                c
c     sd  : sample standard deviation                                  c
c     pdf : PDF estimate                                               c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE kergauss(x,ndim,n,nx,x0,xint,sigma,xm,sd,pdf)
c
      real x(ndim)
      real pdf(nx)
c
      xm=0.
      sd=0.
      do i=1,n
         xm=xm+x(i)/n
      enddo
      do i=1,n
         sd=sd+(x(i)-xm)**2
      enddo
      sd=sqrt(sd/(n-1))
c
c.... Gaussian factors
c
      a=1./(2*n*sqrt(asin(1.))*sigma)
c     a=1./(2*n*sqrt(asin(1.))*sd*sigma)
      b=-1./(2*(sigma)**2)
c     b=-1./(2*(sigma*sd)**2)
c
c.... xx is the central value of the bin
c.... xn is the normalized data value
c.... pdf is the normalized PDF
c
      xx=x0
      do jx=1,nx
         pdf(jx)=0.
         do i=1,n
            xn=(x(i)-xm)/sd
            pdf(jx)=pdf(jx)+exp(b*(xx-xn)**2)
         enddo
         pdf(jx)=pdf(jx)*a
         xx=xx+xint
      enddo
c
c.... checking out that the PDF sums to 1
c
      sum=0.
      do jx=1,nx
         sum=sum+pdf(jx)
      enddo
c     write(*,*)'check: PDF sums ',sum
      do jx=1,nx
         pdf(jx)=pdf(jx)/sum
      enddo
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE bsrelroc                  P. Doblas-Reyes 28-Mar-2007 c
c                                                                      c
c     PURPOSE:                                                         c
c     Calculates the attributes diagram, ROC score, BS and its         c
c     decomposition (taking into account the covariance between the    c
c     the probability forecasts within a bin) and value.               c
c     It is based on the old routines hrfr, relroc and value.          c
c                                                                      c
c     INPUT:                                                           c
c     nxny   : grid point dimension                                    c
c     iprb   : switch for number of prb'bins                           c
c     nyear  : year dimension                                          c
c     nens   : number of ensemble members                              c
c     nprob  : number of probability thresholds used in BS             c
c              decomposition and attributes diagram                    c
c     regwgt : weight of the grid point (0. to 1. depending on what    c
c             part of the grid box lays inside the region              c
c     latwgt : weight of the grid point (0. to 1. depending on         c
c             the latitude of the grid point (cos theta)               c
c     lsm    : land sea mask                                           c
c     narea  : switch for local(=nxny) or regional averaged (=1) calc. c
c     nbin   : number of intervals considered for the PEV              c
c     dat_mod: model data                                              c
c     dat_era: era data                                                c
c     thr_mod: threshold at grid point level for model data            c
c     thr_era: threshold at grid point level for era data              c
c     itag   : switch between anomalies above or below the threshold   c
c                                                                      c
c     OUTPUT:                                                          c
c     hr:      hit rate                                                c
c     fr:      false alarm rate                                        c
c     yhr:     sum (over all prob'bins) of all hr'events               c
c     yfr:     sum (over all prob'bins) of all fr'events               c
c     obs_occ: number of obs-occuring events in certain prob bin       c
c     obs_noc: number of obs-non-occuring events in certain prob bin   c
c     xprob  : average forecast probability for the bin                c
c     sample : number of events observed in a bin                      c
c     yprob  : relative number of hits                                 c
c     bss_ful: Brier score                                             c
c     bss_red: Brier skill score wrt climatology                       c
c     bss_inf: Brier skill score for infinite ensemble size            c
c     rel    : reliability                                             c
c     res    : resolution                                              c
c     bss_rel: reliability skill score                                 c
c     bss_res: resolution skill score                                  c
c     totocc_rel: climatological frequency                             c
c     ave_prob: average forecast probability                           c
c     sd_prob: standard deviation of the forecast probabilities        c
c     roc    : ROC area                                                c
c     rocsup : upper edge of the interval for the ROC area             c
c     roclow : lower edge of the interval for the ROC area             c
c     ipt    : probability threshold corresponding to the envelope     c
c     alpha  : cost-loss ratio                                         c
c     venv   : envelope of the potential economic value curves         c
c     jcount :                                                         c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE bsrelroc(nxny, iprb, nyear, nens, nprob, pev,
     >                    regwgt, latwgt, lsm, narea, nbin,
     >                    dat_mod, dat_era, thr_mod, thr_era, itag,
     >                    obs_occ, obs_noc, hr, fr, yhr, yfr, xprob,
     >                    sample, yprob, bss_ful, bss_red, bss_inf,
     >                    rel, res, bss_rel, bss_res,
     >                    unc, totocc_rel, ave_prob, sd_prob,
     >                    roc, rocsup, roclow,
     >                    ipt, alpha, venv, jcount)
c
      implicit none
c
      integer nxny, iprb, nyear, nens, nprob
      integer lsm(nxny), iarea, tagwei, itag
      integer ip, inew, iens, iprob, narea, iyear, i
      integer ipr
      integer jcount, j, nbin
      integer ipt(nbin)
      real obs_occ_t(nens+1), obs_noc_t(nens+1)
      double precision obs_occ_r(narea,nens+1), obs_noc_r(narea,nens+1)
      double precision obs_occ(narea,nprob), obs_noc(narea,nprob)
      double precision xprob(narea,nprob)
      real hr(narea,nens+1), fr(narea,nens+1)
      real hrsup(narea,nens+1), hrlow(narea,nens+1)
      real xhr(narea), yhr(narea), xfr(narea), yfr(narea)
      real dat_mod(nxny,nyear,nens), dat_era(nxny,nyear)
      real regwgt(nxny), latwgt(nxny), thr_mod(nxny), thr_era(nxny)
      real roc(narea), rocsup(narea), roclow(narea)
      double precision sample(narea,nprob)
      real ave_prob(narea), sd_prob(narea), yprob(narea,nprob)
      double precision totocc_rel(narea), rel(narea), res(narea)
      double precision bs(narea)
      double precision bss_sa(narea), bss_rel(narea), bss_res(narea)
      double precision unc(narea)
      double precision bss_ful(narea), bss_red(narea), bss_inf(narea)
      real xhlp(nens), yhlp, eps, t_mod, t_era
      real alpha(nbin), venv(nbin)
      real zalpha2, zalpha22, xsick
      real totocc, totobs, pev
      real xpr, x1, x1sup, x1low, y1, y1sup, y1low
      real x2, x2sup, x2low, y2, y2sup, y2low
      real*8 obsocc_rel, rect, tria
      real*8 wbv, wbc, okj, sam
      real s, num
      real alpha0, obar, xmin, v1, v2, v
c
      eps = tiny(eps)
      alpha0=0.001
      jcount=0
c
c.... reset contingency table
c
      do i=1,narea
         do iprob=1,nens+1
            obs_occ_r(i,iprob)=0.
            obs_noc_r(i,iprob)=0.
         enddo
         do iprob=1,nprob
            obs_occ(i,iprob)=0.
            obs_noc(i,iprob)=0.
            xprob(i,iprob)=0.
         enddo
      enddo
c
c.... alpha/2 quantile of the standard normal distribution
c.... the value selected is alpha=95%
c
      zalpha2 =1.96
      zalpha22=zalpha2**2
c
c.... loop over grid points and years to build contingency table
c.... tagwei includes the areal weighting (1) or not (0)
c
c     write(*,*)'pev=',pev
      do i=1,nxny
         if(regwgt(i).gt.0.)then
           if(narea.eq.1)then
             iarea=1
             tagwei=1
           else
             iarea=i
             tagwei=0
           endif
           t_mod = thr_mod(i)
           t_era = thr_era(i)
c          write(*,'(a,i5,2f9.4)')'thresholds ',i,t_mod,t_era
           do iprob=1,nens+1
              obs_occ_t(iprob)=0.
              obs_noc_t(iprob)=0.
           enddo
           do iyear=1,nyear
              yhlp=dat_era(i,iyear)
              do iens=1,nens
                 xhlp(iens)=dat_mod(i,iyear,iens)
              enddo !iens
c             write(*,'(10f9.4)')yhlp,(xhlp(iens),iens=1,nens)
c             write(*,'(2i3,f9.4,i3)')itag,tagwei,latwgt(i),lsm(i)
c
c... check whether threshold is negativ or positiv
c
              if(itag.eq.1)then
                if(yhlp.le.t_era)then !event is verified
                  num=0
                  do iens=1,nens
                     if(xhlp(iens).le.t_mod)num=num+1 ! ens'member forecasts event
                  enddo !iens
                  iprob=1+num
                  obs_occ_t(iprob)=obs_occ_t(iprob)+1. !add obs_occ in corresp. prob'bin
                else !event is not verified
                  num=0
                  do iens=1,nens
                     if(xhlp(iens).le.t_mod)num=num+1 ! ens'member forecasts event
                  enddo !iens
                  iprob=1+num
                  obs_noc_t(iprob)=obs_noc_t(iprob)+1. !add obs_noc in corresp. prob'bin
                endif
              else
                if(yhlp.gt.t_era)then !event is verified
                  num=0
                  do iens=1,nens
                     if(xhlp(iens).gt.t_mod)num=num+1 ! ens'member forecasts event
                  enddo !iens
                  iprob=1+num
                  obs_occ_t(iprob)=obs_occ_t(iprob)+1. !add obs_occ in corresp. prob'bin
                else !event is not verified
                  num=0
                  do iens=1,nens
                     if(xhlp(iens).gt.t_mod)num=num+1 ! ens'member forecasts event
                  enddo !iens
                  iprob=1+num
                  obs_noc_t(iprob)=obs_noc_t(iprob)+1. !add obs_noc in corresp. prob'bin
                endif
              endif
           enddo !iyear
           do iprob=1,nens+1
              obs_occ_r(iarea,iprob)=obs_occ_r(iarea,iprob)+
     >           lsm(i)*(1.+tagwei*(latwgt(i)-1.))*obs_occ_t(iprob)
              obs_noc_r(iarea,iprob)=obs_noc_r(iarea,iprob)+
     >           lsm(i)*(1.+tagwei*(latwgt(i)-1.))*obs_noc_t(iprob)
c             write(*,'(2i5,2f8.3)')i,iprob,
c    >           obs_occ_t(iprob),obs_noc_t(iprob)
           enddo !iprob
         endif !regwgt(i).gt.0.
      enddo !i
c
c... integrated bins
c
      do i=1,narea
         do iprob=1,nens+1
            xpr=(iprob-1.)/nens
            ip=iprob
            if(iprb.ne.1)then
              ip=1+((iprob-1)*nprob)/(nens+1)
              if(ip.gt.nprob)ip=nprob
            endif
            obs_occ(i,ip)=obs_occ(i,ip)+
     >         obs_occ_r(i,iprob)
            obs_noc(i,ip)=obs_noc(i,ip)+
     >         obs_noc_r(i,iprob)
            xprob(i,ip)=xprob(i,ip)+
     >         (obs_occ_r(i,iprob)+obs_noc_r(i,iprob))*xpr
c           write(*,'(2i5,3f12.4,i5,3f12.4)')i,iprob,xpr,
c    >         obs_occ_r(i,iprob),obs_noc_r(i,iprob),
c    >         ip,xprob(i,ip),obs_occ(i,ip),obs_noc(i,ip)
         enddo !iprob
      enddo !i
c
c.... contingency table finished, now calculate scores
c
      do i=1,narea
         do iprob=1,nprob
            sample(i,iprob)=obs_occ(i,iprob)+
     >         obs_noc(i,iprob)
            if(iprb.eq.1)then
              xprob(i,iprob)=(iprob-1.)/nens
            else
              if(obs_occ(i,iprob).ne.0.)then
                xprob(i,iprob)=xprob(i,iprob)/
     >             sample(i,iprob)
              else
                xprob(i,iprob)=(2*iprob-1.)/(2*nprob)
              endif
            endif
c           if(i.eq.1)then
c             write(*,'(i5,4f20.5)')iprob,xprob(i,iprob),
c    >           obs_occ(i,iprob),obs_noc(i,iprob),sample(i,iprob)
c           endif
         enddo !iprob
c
c.... HR and FR
c
         yhr(i)=0.
         yfr(i)=0.
         do iprob=1,nens+1
            yhr(i)=yhr(i)+obs_occ_r(i,iprob)
            yfr(i)=yfr(i)+obs_noc_r(i,iprob)
         enddo
         do iprob=1,nens+1
            xhr(i)=0.
            xfr(i)=0.
            do ip=iprob,nens+1
               xhr(i)=xhr(i)+obs_occ_r(i,ip)
               xfr(i)=xfr(i)+obs_noc_r(i,ip)
            enddo
            inew=nens+1-iprob+1
            hr(i,inew)=xhr(i)/(yhr(i)+eps)
            fr(i,inew)=xfr(i)/(yfr(i)+eps)
            if(yhr(i).eq.0.)then
              hrsup(i,inew)=0.
              hrlow(i,inew)=0.
            else
              xsick=zalpha2*sqrt( (hr(i,inew)*(1-hr(i,inew))
     >           + zalpha22/(4*yhr(i))) / yhr(i))
              hrsup(i,inew)=(hr(i,inew)+zalpha22/(2*yhr(i))+xsick)/
     >           (1+zalpha22/yhr(i))
              hrlow(i,inew)=(hr(i,inew)+zalpha22/(2*yhr(i))-xsick)/
     >           (1+zalpha22/yhr(i))
            endif
         enddo
c
c.... the test does not have statistical sense when the number of
c.... events is smaller than zalpha22
c
         if(yhr(i).lt.zalpha22)then
           do iprob=1,nens+1
              hrlow(i,iprob)=0.
              hrsup(i,iprob)=1.
              if(hr(i,iprob).eq.0.) hrlow(i,iprob)=0.
           enddo
c        else
c          do iprob=1,nens+1
c             if(hrlow(i,iprob).gt.hr(i,iprob))
c    >           write(*,*)'low',i,iprob,hrlow(i,iprob),hr(i,iprob),
c    >           hrsup(i,iprob),yhr(i)
c             if(hrsup(i,iprob).lt.hr(i,iprob))
c    >           write(*,*)'sup',i,iprob,hrlow(i,iprob),hr(i,iprob),
c    >           hrsup(i,iprob),yhr(i)
c          enddo
         endif
         hr(i,1)=0.
         hrlow(i,1)=0.
         hrsup(i,1)=0.
         hr(i,nens+1)=1.
         hrlow(i,nens+1)=1.
         hrsup(i,nens+1)=1.
         fr(i,1)=0.
         fr(i,nens+1)=1.
c
c.... Calculate ROC and the upper and lower bounds of the score
c.... confidence interval for HR.
c.... The test is not applied when the number of events is found
c.... to be lower than nxny*nyear/100 (ad-hoc value).
c.... The bounds are limited to the range [0,1].
c
c        if(narea.eq.1)write(*,*)'ROC score'
         roc(i)=0.
         rocsup(i)=0.
         roclow(i)=0.
         do iprob=1,nens+1
c           write(*,'(i8,4f10.3)')(iprob-1),
c    >      hr(i,iprob),fr(i,iprob),
c    >      min(1.,hrsup(i,iprob)),max(0.,hrlow(i,iprob))
            if(iprob.eq.1)then
              x1=0.
              x1sup=0.
              x1low=0.
              x2=fr(i,1)
              x2sup=fr(i,1)
              x2low=fr(i,1)
              y1=0.
              y1sup=0.
              y1low=0.
              y2=hr(i,1)
              y2sup=min(1.,hrsup(i,1))
              y2low=hrlow(i,1)
            else
              x1=fr(i,iprob-1)
              x1sup=fr(i,iprob-1)
              x1low=fr(i,iprob-1)
              x2=fr(i,iprob)
              x2sup=fr(i,iprob)
              x2low=fr(i,iprob)
              y1=hr(i,iprob-1)
              y1sup=min(1.,hrsup(i,iprob-1))
              y1low=hrlow(i,iprob-1)
              y2=hr(i,iprob)
              y2sup=min(1.,hrsup(i,iprob))
              y2low=hrlow(i,iprob)
            endif
            rect  = (x2-x1)*y1
            tria  = 0.5*(x2-x1)*(y2-y1)
            roc(i)=roc(i)+rect +tria
            rocsup(i)=rocsup(i)+(x2sup-x1sup)*y1sup +
     >         0.5*(x2sup-x1sup)*(y2sup-y1sup)
            roclow(i)=roclow(i)+(x2low-x1low)*y1low +
     >         0.5*(x2low-x1low)*(y2low-y1low)
         enddo
c
c.... Brier score and its decomposition
c
c        if(narea.eq.1)write(*,*)'Brier score'
         totocc = 0.
         totobs = 0.
         do iprob=1,nprob
            totocc=totocc+obs_occ(i,iprob)
            totobs=totobs+obs_occ(i,iprob)+obs_noc(i,iprob)
         enddo
         totocc_rel(i)=0.
         if(totobs.gt.0.)totocc_rel(i)=totocc/totobs
         if(nyear.eq.1)totocc_rel(i)=pev
c        if(narea.eq.1)write(*,*)'totocc,totobs ',totocc,totobs
c
c.... First, Brier score for all possible probabilities and for
c.... the restricted set
c
         bss_ful(i)=0.
         do iprob=1,nens+1
            xpr=(iprob-1.)/nens
            bss_ful(i)=bss_ful(i)+obs_occ_r(i,iprob)*(xpr-1)**2+
     >         obs_noc_r(i,iprob)*xpr**2
c           write(*,'(i2,3f14.3)')iprob,
c    >         xpr,obs_occ_r(i,iprob),obs_noc_r(i,iprob)
         enddo
         bss_ful(i)=bss_ful(i)/totobs
         bss_red(i)=0.
         do iprob=1,nprob
            xpr=xprob(i,iprob)
            bss_red(i)=bss_red(i)+obs_occ(i,iprob)*(xpr-1)**2+
     >         obs_noc(i,iprob)*xpr**2
c           write(*,'(i2,3f14.3)')iprob,
c    >         xpr,obs_occ(i,iprob),obs_noc(i,iprob)
         enddo
         bss_red(i)=bss_red(i)/totobs
c
c.... decomposition
c
         rel(i)=0.
         res(i)=0.
         ave_prob(i)=0.
         wbv=0.
         wbc=0.
         do iprob=1,nprob
            obsocc_rel = 0.
            if(sample(i,iprob).ne.0.)
     >         obsocc_rel=obs_occ(i,iprob)/sample(i,iprob)
            yprob(i,iprob)=obsocc_rel
            rel(i)=rel(i)+
     >         ((xprob(i,iprob)-obsocc_rel)**2)*sample(i,iprob)
            res(i)=res(i)+
     >         ((totocc_rel(i)-obsocc_rel)**2)*sample(i,iprob)
            ave_prob(i)=ave_prob(i)+xprob(i,iprob)*sample(i,iprob)
c
c.... terms for generalized resolution
c
            if(iprb.ne.1)then
              do ipr=1,nens+1
                 xpr=(ipr-1.)/nens
                 ip=1+((ipr-1)*nprob)/(nens+1)
c                write(*,*)iprob,ipr,ip
                 if(ip.eq.iprob)then
                   sam=obs_occ_r(i,ipr)+obs_noc_r(i,ipr)
                   wbv=wbv+((xpr-xprob(i,iprob))**2)*sam
                   okj=0.
                   if(sam.ne.0.)okj=obs_occ_r(i,ipr)/sam
                   wbc=wbc+2*(xpr-xprob(i,iprob))*
     >                (okj-obsocc_rel)*sam
c                  write(*,*)iprob,ipr,
c    >                sample(i,iprob),sam,
c    >                obs_occ_r(i,ipr),obs_noc_r(i,ipr),
c    >                xpr,xprob(i,iprob),wbv,
c    >                okj,obsocc_rel,wbc
                 endif
c                write(*,'(a,2i3,3f12.6)')'Additional terms ',
c    >              iprob,ipr,wbv/totobs,wbc/totobs,res(i)/totobs
              enddo
            endif
         enddo
c        write(*,'(a,3f10.4)')'RES components ',res(i)/totobs,
c    >      -wbv/totobs,wbc/totobs
cc         res(i)=res(i)-wbv+wbc
c
         ave_prob(i)=ave_prob(i)/totobs
c        write(*,*)'Averages ',totocc_rel(i),ave_prob(i)
         sd_prob(i)=0.
         do iprob=1,nprob
            sd_prob(i)=sd_prob(i)+
     >         sample(i,iprob)*(xprob(i,iprob)-ave_prob(i))**2
         enddo
         sd_prob(i)=sqrt(sd_prob(i))/totobs
cc         unc(i)=totocc_rel(i)*(1.0-totocc_rel(i))
         unc(i)=ave_prob(i)*(1.0-ave_prob(i))
         bs(i)=(unc(i)*totobs+rel(i)-res(i))/totobs
         rel(i)=rel(i)/totobs
         res(i)=res(i)/totobs
         if(unc(i).ne.0.)then
           bss_sa(i)=1.-bs(i)/unc(i)
           bss_ful(i)=1.-bss_ful(i)/unc(i)
           bss_red(i)=1.-bss_red(i)/unc(i)
           bss_rel(i)=1.-rel(i)/unc(i)
           bss_res(i)=res(i)/unc(i)
         else
           bss_sa(i)=999.
           bss_ful(i)=999.
           bss_red(i)=999.
           bss_rel(i)=999.
           bss_res(i)=999.
         endif
         bss_red(i)=bss_sa(i)
c        write(*,*)bs(i),bss_sa(i),rel(i),
c    >      bss_rel(i),res(i),bss_res(i),
c    >      unc(i),totocc_rel(i),ave_prob(i),
c    >      sd_prob(i)
c        write(*,'(a,3f10.4)')'BSS=',
c    >      bss_sa(i),bss_ful(i),bss_red(i)
c
c.... calculate Brier score for infinite ensemble size
c
c.... for a given ensemble size M different from the one
c.... used in the forecast, the expression is
c.... B_M,n=B_m,n-((1/(m-1))-m/(M*(m-1)))*(1/4-S)
c.... where B_m,n is the sample BS for ensemble size m
c.... and sample size n, M is the target size and S is
c.... the sharpness around 1/2 S=1/n*sum_n(Q_t-1/2)**2
c.... with Q_t the forecast probabilities
c.... for the infinite size
c.... B_M,n=B_m,n-(1/(m-1))*(1/4-S)
c
         s=0.
         do iprob=1,nens+1
            xpr=(iprob-1.)/nens
c           write(*,'(i2,3f14.3)')iprob,
c    >         xpr,obs_occ_r(i,iprob),obs_noc_r(i,iprob)
            s=s+(obs_occ_r(i,iprob)+obs_noc_r(i,iprob))*(xpr-0.5)**2
         enddo
         s=s/totobs
         if(unc(i).eq.0.)then
           bss_inf(i)=999.
         else
           bss_inf(i)=1.-(bs(i)-(1./(nens-1))*(1/4.-s))/unc(i)
         endif
c        write(*,'(5f14.3)')bs(i),s,totobs,unc(i),bss_inf(i)
c
c.... calculate probabilistic potential value
c.... alpha is the cost-loss ratio
c.... venv is the envelope of the probabilistic value curves
c.... ipt indicates the probability threshold corresponding to the value envelope
c
         obar=yhr(i)/(yhr(i)+yfr(i))
         do j=1,nbin
c
c.... logarithmic scale for alpha
c.... it corresponds to the formula:
c.... log(alpha0)+(log(alpha0/alphalast)/(nbin-1)*(j-1)
c.... where alphalast is 1.
c
            alpha(j)=10**(log10(alpha0)*
     >         ((nbin-j)/float(nbin-1)))
            if(alpha(j).gt.1.)goto40
            jcount  = jcount+1
            xmin    = min(alpha(j),obar)
            venv(j) = 0.
            ipt(j)  = nprob
            do iprob=1,nprob
               v1 = (xmin-fr(i,iprob)*alpha(j)*(1-obar)+
     >            hr(i,iprob)*obar*(1-alpha(j))-obar)
               v2 = (xmin-obar*alpha(j))
               v  = v1/(v2+eps)
               if(v.gt.venv(j))then
                 venv(j) = max(v,venv(j))
                 ipt(j)  = nprob-iprob
               endif
            enddo ! iprob loop
         enddo ! j loop
40       continue
      enddo !i
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE edge                     P. Doblas-Reyes, 31-Aug-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     computes the edges of a two-sided confidence interval given      c
c     a time series obtained by bootstrapping                          c
c                                                                      c
c     INPUT:                                                           c
c     x      : data                                                    c
c     nboot  : number of bootstraps samples                            c
c     ntop   : rank of the top edge                                    c
c     nbot   : rank of the bottom edge                                 c
c                                                                      c
c     OUTPUT:                                                          c
c     x_top  : value of the top edge                                   c
c     x_bot  : value of the bottom edge                                c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE edge(x,nboot,ntop,nbot,x_top,x_bot)
c
      implicit none
c
      real x(nboot)
      integer nboot, ntop, nbot
      real x_top, x_bot
      integer nr(nboot)
      integer ib, id, idt, idb
c
      do ib=1,nboot
         nr(ib)=1
         do id=1,nboot
            if(x(ib).gt.x(id))then
              nr(ib)=nr(ib)+1
            endif
         enddo ! id loop
      enddo ! ib loop
      do ib=1,nboot
         do id=ib+1,nboot
            if(nr(ib).eq.nr(id))nr(id)=nr(id)+1
         enddo ! id loop
      enddo ! ib loop
      do ib=1,nboot
         if(nr(ib).eq.ntop)idt=ib
         if(nr(ib).eq.nbot)idb=ib
      enddo
      x_top=x(idt)
      x_bot=x(idb)
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE sort                         L. Ferranti, 20-Sep-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     ranks an array in either increasing (1) or decreasing (-1) order c
c                                                                      c
c     INPUT:                                                           c
c     k      : switch to make increasing or decreasing sorting         c
c     ndim   : dimension of the arrays                                 c
c     n      : number of data                                          c
c     x      : time series containing the data                         c
c                                                                      c
c     OUTPUT:                                                          c
c     y      : time series containing the sorted data                  c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE sort(k,ndim,n,x,y)
c
      implicit none
c
      integer k, ndim, n
      real x(ndim), y(ndim)
      logical ldone
      real ztemp
      integer j
c
      do j=1,n
         y(j)=x(j)
      enddo
c
      do
         ldone = .true.
         if(k.eq.1)then
           do j=1,n-1
              if(y(j+1).lt.y(j)) then
                ztemp   = y(j+1)
                y(j+1)  = y(j)
                y(j)    = ztemp
                ldone   = .false.
              endif
           enddo
         elseif(k.eq.-1)then
           do j=1,n-1
              if(y(j+1).gt.y(j)) then
                ztemp   = y(j+1)
                y(j+1)  = y(j)
                y(j)    = ztemp
                ldone   = .false.
              endif
           enddo
         endif
         if(ldone) goto 1
      enddo
c
1     return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE rmse                       P.Doblas-Reyes  6-Sep-2001 c
c                                                                      c
c     PURPOSE:                                                         c
c     calculates deterministic scores for the indices time series      c
c                                                                      c
c     INPUT:                                                           c
c     r      : reference value                                         c
c     h      : hindcast value                                          c
c     ndim   : time dimension                                          c
c     mdim   : ensemble dimension                                      c
c     n      : number of time steps                                    c
c     m      : number of ensemble members                              c
c                                                                      c
c     OUTPUT:                                                          c
c     vr     : standard deviation of the reference                     c
c     vh     : standard deviation of the hindcasts                     c
c     vmh    : standard deviation of the ensemble mean hindcast        c
c     vnr    : spread                                                  c
c     vnri   : spread of the inflated values                           c
c     ratio  : ratio of standard deviations (model vs reference)       c
c     rms    : root mean square error                                  c
c     rmsi   : root mean square error of the inflated values           c
c     cor    : ensemble mean correlation                               c
c     sig    : correlation statistical significance                    c
c     cori   : ensemble mean correlation of the inflated values        c
c     sigi   : correlation statistical significance of inflated values c
c     snr    : signal-to-noise ratio                                   c
c     sigr   : SNR statistical significance                            c
c     snri   : signal-to-noise ratio of the inflated values            c
c     sigri  : SNR statistical significance for inflated values        c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE rmse(r,h,ndim,n,mdim,m,
     >                vr,vh,vmh,vnr,vnri,ratio,rms,rmsi,
     >                cor,sigc,cori,sigi,snr,sigr,snri,sigri)
      implicit none
c
c.... nscale deals with two types of hindcast ratio
c.... takes the value:
c....      1 when using the whole ensemble variance
c....      2 when using the ensemble mean variance
c
      integer nscale
      parameter(nscale=1)
c
c.... variances, covariances, correlation and inflation
c.... in crossvalidation mode
c....    r stands for reference
c....    h stands for ensemble hindcasts
c
      real r(ndim),h(ndim,mdim)
      real rc(ndim),hc(ndim,mdim),f(mdim)
      real hh(n,m),xm(n)
      external g01ebf
      external g01edf
c
      real cor, ratio, rms, rmsi, sigc, snri, snr, sigr, sigri
      real vr, vh, vn, vnr, vnri, vmh, vmht, vrt, vht, sigma_noise
      real cori, sigi
      real xmr, xmh, cov, ratiot
      real xmhi, vmhi, vhi, vni, eps
      real beta, gamma, cort, covt, fm
      integer ndim, n, mdim, m
      integer i, ii, il, j, ifail
      integer cv
      double precision g01ebf, p, df, t
      double precision g01edf, df1, df2, snrdd
c
      eps=tiny(eps)
c
c.... global mean and variance:
c.... vh is the total hindcast variance
c.... vn is the spread divided by the interannual sd
c.... vr is the verification variance
c.... vmh is the ensemble mean variance
c
      xmr=0.
      xmh=0.
      vr=0.
      vh=0.
      vn=0.
      vmh=0.
      cov=0.
      do i=1,n
         xm(i)=0.
         do j=1,m
            xmh=xmh+h(i,j)/(m*n)
            xm(i)=xm(i)+h(i,j)/m
         enddo
         xmr=xmr+r(i)/n
      enddo
      do i=1,n
         do j=1,m
            vh=vh+(h(i,j)-xmh)**2
            vn=vn+(h(i,j)-xm(i))**2
         enddo
         cov=cov+(xm(i)-xmh)*(r(i)-xmr)
         vmh=vmh+(xm(i)-xmh)**2
         vr=vr+(r(i)-xmr)**2
      enddo
      vh=sqrt(vh/(n*m-1))
      vn=sqrt(vn/(n*m-n))
      vr=sqrt(vr/(n-1))
      vmh=sqrt(vmh/(n-1))
      cov=cov/(n-1)
      cor=cov/(vmh*vr)
      if(nscale.eq.1)then
        ratio=vh/vr
      elseif(nscale.eq.2)then
        ratio=vmh/vr
      endif
c     vnr=vn/vh
      vnr=vn
c
      write(*,'(a,3f12.4)')'Variances vh,vn and vmh ',vh,vn,vmh
      write(*,'(a,f10.3)')'Spread= ',vnr
      write(*,'(a,f10.3)')'Observations= ',vr
      if(vn.ne.0.)then
        snr=(vmh/(vn+eps))**2
      else
        snr=999.
      endif
c
c.... the statistical significance test for the signal-to-noise
c.... ratio is applied here
c.... the statistic snr is distributed in the null case (of SNR 
c.... significantly lower than 1) like an F distribution
c.... with n-1 and and (n-1)*(m-1) degrees of freedom
c.... the F-value is computed and the one-sided significance level 
c.... is given by 1-p
c
      ifail=0
      df1=n-1
      df2=(n-1)*(m-1)
      write(*,'(a,2f7.0,f10.2)')'degrees of freedom and snr ',
     >   df1,df2,snr
      snrdd=snr
      p=g01edf('u',snrdd,df1,df2,ifail)
      write(*,'(a,f10.2)')' p-value ',p
c     sigr=1-p
      sigr=p
c
c.... the final value of snr is given as the ratio of standard deviations
c
      snr=sqrt(snr)
      write(*,'(a,f12.4)')'Signal-to-noise ratio= ',snr
c
c.... rmse and time correlation of the ensemble mean
c
      rms=0.
      do i=1,n
         do j=1,m
            rms=rms+(h(i,j)-r(i))**2
         enddo
      enddo
      rms=sqrt(rms/(m*n))
c
c.... the statistical significance test for time correlation
c.... is specific for small sample size
c.... the statistic t=cor*sqrt((n-2)/(1-cor**2)) is distributed
c.... in the null case (of no correlation) like Student's
c.... t distribution with n-2 degrees of freedom
c.... the p-value is given and the two-sided significance
c.... level is given by 1-2*p
c.... from Numerical Recipes, ch 13, p. 486
c
      t=cor*sqrt((n-2)/(1-cor**2))
      t=abs(t)
      ifail=0
      df=float(n-2)
      write(*,'(a,f7.0,f10.2)')'degrees of freedom and t ',df,t
      p=g01ebf('u',t,df,ifail)
      write(*,'(a,f10.4,a,f10.3)')'correlation ',cor,' p-value ',p
      if(ifail.ne.0)write(*,*)'error in NAG routine'
c     sigc=1-2*p
      sigc=2*p
c
c.... inflation (crossvalidation is applied if cv ne 0)
c
      cv=1
      call inflation(r, h, hh, n, m, cv)
c
c.... signal-to-noise ratio for the inflated values
c
      xmhi=0.
      do i=1,n
         xm(i)=0.
         do j=1,m
            xmhi=xmhi+hh(i,j)/(m*n)
            xm(i)=xm(i)+hh(i,j)/m
         enddo
      enddo
      vhi=0.
      vni=0.
      vmhi=0.
      cov=0.
      do i=1,n
         do j=1,m
            vhi=vhi+(hh(i,j)-xmhi)**2
            vni=vni+(hh(i,j)-xm(i))**2
         enddo
         cov=cov+(xm(i)-xmhi)*(r(i)-xmr)
         vmhi=vmhi+(xm(i)-xmhi)**2
      enddo
      vhi=sqrt(vhi/(n*m-1))
      vni=sqrt(vni/(n*m-n))
      vmhi=sqrt(vmhi/(n-1))
      cov=cov/(n-1)
      cori=cov/(vmhi*vr)
c     vnri=vni/vhi
      vnri=vni
c
      write(*,'(a,3f12.4)')'Variances vh, vn and vmh ',vhi,vni,vmhi
      if(vni.ne.0.)then
        snri=(vmhi/(vni+eps))**2
      else
        snri=999.
      endif
c
c.... statistical significance test for the signal-to-noise
c.... ratio of the inflated values
c
      ifail=0
      df1=n-1
      df2=(n-1)*(m-1)
      write(*,'(a,2f7.0,f10.2)')'degrees of freedom and snr (infl) ',
     >   df1,df2,snri
      snrdd=snri
      p=g01edf('u',snrdd,df1,df2,ifail)
      write(*,*)' p-value ',p
c     sigri=1-p
      sigri=p
c
c.... the final value of snr is given as the ratio of standard deviations
c
      snri=sqrt(snri)
      write(*,'(a,f10.4)')'Signal-to-noise ratio (inflated)= ',snri
c
c.... rmse for the inflated values
c
      rmsi=0.
      do i=1,n
         do j=1,m
            rmsi=rmsi+(hh(i,j)-r(i))**2
         enddo
      enddo
      rmsi=sqrt(rmsi/(m*n))
c
      t=cori*sqrt((n-2)/(1-cori**2))
      t=abs(t)
      ifail=0
      df=float(n-2)
      write(*,'(a,f7.0,f10.2)')'degrees of freedom and t ',df,t
      p=g01ebf('u',t,df,ifail)
      write(*,'(a,f10.4,a,f10.3)')'correlation ',cori,' p-value ',p
      if(ifail.ne.0)write(*,*)'error in NAG routine'
      sigi=2*p
c
      do i=1,n
         do j=1,m
            h(i,j)=hh(i,j)
         enddo
      enddo
c
      return
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE probsco                    P.Doblas-Reyes  6-Sep-2001 c
c                                                                      c
c     PURPOSE:                                                         c
c     calculates probabilistic scores for the time series              c
c                                                                      c
c     INPUT:                                                           c
c     r      : reference value                                         c
c     h      : hindcast value                                          c
c     ndim   : time dimension                                          c
c     mdim   : ensemble dimension                                      c
c     n      : number of time steps                                    c
c     m      : number of ensemble members                              c
c     ncla   : number of classes                                       c
c     permeth: method used to compute the percentiles as               c
c              described in the routine header with 1 for              c
c              kernel method and 0 for counting                        c
c                                                                      c
c     OUTPUT:                                                          c
c     ss     : RPSS                                                    c
c     ssp    : RPSS p-value                                            c
c     rpssd  : RPSSd                                                   c
c     ssd    : RPSSd p-value                                           c
c     tr     : thresholds for the classes for the reference            c
c     th     : thresholds for the classes for the hindcasts            c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE probsco(r, h, ndim, n, mdim, m, ncla, permeth, ss,
     >                   ssp, rpssd, ssd, tr, th)
c
      implicit none
c
      integer nt
      parameter(nt=100)
c
c.... ranked probability skill score
c....    r stands for reference
c....    h stands for ensemble hindcasts
c....    nt is the number of trials
c
      real, allocatable :: t(:)
      integer ndim, n, mdim, m, ncla, permeth
      integer ncl, istat
      real r(ndim),h(ndim,mdim)
      real hh(n*m),sh(ndim,mdim)
      real score(nt),rps1(nt),rps2(nt),y(nt)
      real tr(ncla-1),th(ncla-1)
      real xmr, xmh, vr, vh, cov
      real ss, sse, ssp, ssd, rpsh, rpsc, d, rpssd
      real dif, epsilon, xi1, xi2
      integer nr(nt)
      integer i, j, jt, it, l1, l2, icla, icl
c
      double precision x1,x2
c.... external functions
      external g05saf
c... external routines
      external g05kgf
      double precision g05saf
c
c.... estimating mean and variance
c
      xmr=0.
      xmh=0.
      vr=0.
      vh=0.
      cov=0.
      do i=1,n
         do j=1,m
            xmh=xmh+h(i,j)/(m*n)
         enddo
         xmr=xmr+r(i)/n
      enddo
      do i=1,n
         do j=1,m
            vh=vh+(h(i,j)-xmh)**2
         enddo
         vr=vr+(r(i)-xmr)**2
      enddo
      vh=sqrt(vh/(m*n-1))
      vr=sqrt(vr/(n-1))
c     write(*,*)'Check mean (should be close to 0) and variance'
c     write(*,*)'Mean: ensemble and reference ',xmh,xmr
c     write(*,*)'Variance: ensemble and reference ',vh,vr
c
c.... computing percentiles using a kernel-based PDF
c
c     write(*,*)'Call to percentiles for reference'
      ncl=100
      allocate (t(ncl-1),                                    stat=istat)
      call percen(r,ndim,n,t,ncl,permeth)
      do icla=1,ncla-1
         icl=(icla*ncl+1)/ncla
         write(*,*)'icl=',icl
         tr(icla)=t(icl)
      enddo
      write(*,*)'Reference thresholds ',(tr(icla),icla=1,ncla-1)
      deallocate (t,                                         stat=istat)
      do i=1,n
         do j=1,m
            hh((i-1)*m+j)=h(i,j)
         enddo
      enddo
c     write(*,*)'Call to percentiles for hindcasts'
      allocate (t(ncl-1),                                    stat=istat)
      call percen(hh,n*m,n*m,t,ncl,permeth)
      do icla=1,ncla-1
         icl=(icla*ncl+1)/ncla
         th(icla)=t(icl)
      enddo
      write(*,*)'Hindcast thresholds ',(th(icla),icla=1,ncla-1)
      deallocate (t,                                         stat=istat)
c
c.... computing rpss
c
c     write(*,*)'Call to RPSS'
      call rpss(r,h,ndim,n,mdim,m,tr,th,ncla,rpsh,rpsc,ss)
      d=(ncla**2-1)/(6.*ncla*m)
      d=d*n
      rpssd=1.-rpsh/(rpsc+d)
      write(*,'(a,3f10.3,2i4,2f10.3)')
     >   'RPS:',rpsh,rpsc,ss,ncla,m,d,rpssd
c
c.... computing the confidence level for rpss
c....   computes the rpss for nt bootstrap trials 
c....   in which the verification and the hindcasts
c....   are both scrambled
c
c     write(*,*)'Call to RPSS for significance'
c
c.... scrambling the data and computing RPSS
c
      do jt=1,nt
c        write(*,*)'Scrambling ',jt
         do i=1,n
            do j=1,m
100            call g05kgf(0)
               x1=g05saf(x1)
               l1=int(n*x1)+1
               if(l1.eq.i)goto 100
               x2=g05saf(x2)
               l2=int(m*x2)+1
               sh(i,j)=h(l1,l2)
            enddo
         enddo
c        write(*,*)'Scrambling ',jt,((sh(i,j),j=1,m),i=1,n)
c        write(*,*)'Scrambling ',jt,l1,l2
         call rpss(r,sh,ndim,n,mdim,m,tr,th,ncla,rpsh,rpsc,sse)
         rps1(jt)=rpsh
         rps2(jt)=rpsc
         score(jt)=sse
      enddo
c     write(*,*)((jt,score(jt)),jt=1,nt)
c
c.... ordering the results (increasing order) and
c.... statistics
c
      epsilon=tiny(epsilon)
c
c.... the data are ranked in increasing order
c
      do jt=1,nt
         nr(jt)=1
         do it=1,nt
            dif=score(jt)-score(it)
            dif=dif-epsilon
            dif=sign(1.,dif)
            dif=max(dif,0.)
            nr(jt)=nr(jt)+int(dif)
         enddo
      enddo
      do jt=1,nt
         do it=jt+1,nt
            if(nr(jt).eq.nr(it))nr(it)=nr(it)+1
         enddo
      enddo
      do jt=1,nt
         y(nr(jt))=score(jt)
c        write(*,*)jt,nr(jt),rps1(nr(jt)),rps2(nr(jt)),y(nr(jt))
      enddo
c     write(*,*)((jt,y(jt)),jt=1,nt)
c     write(*,*)'Real score ',ss
      it=1
      do jt=nt,1,-1
         if(ss.ge.y(jt))then
           it=jt
           goto 110
         endif
      enddo
c
c.... computing percentiles for RPSS
c
110   if(it.ge.nt)then
        ssp=0.
      elseif(it.le.1)then
        ssp=1.
      else
        xi1=ss-y(it)
        xi2=y(it+1)-ss
        xi1=xi1/(xi1+xi2)
        xi2=1.-xi1
        ssp=1.-(it*xi2+(it+1)*xi1)/100.
      endif
      if(ss.lt.0.)ssp=1.
      if(rpssd.lt.0.)then
        ssd=1.
      else
        ssd=ssp
      endif
      write(*,'(a,i3.3,a,f12.4,a,f6.1)')
     >   'it=',it,' rpss=',ss,' p-value=',ssp
      write(*,'(a,i3.3,a,f12.4,a,f6.1)')
     >   'it=',it,' rpssd=',rpssd,' p-value=',ssd
c
      return
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE rpss                     P. Doblas-Reyes, 15-Nov-2001 c
c                                                                      c
c     PURPOSE:                                                         c
c     computes the RPSS for a time series given a number of classes    c
c                                                                      c
c     INPUT:                                                           c
c     r   : reference data                                             c
c     h   : hindcast data                                              c
c     tr  : reference percentiles (classes thresholds)                 c
c     th  : hindcast percentiles                                       c
c     ny  : dimension for number of years                              c
c     nens: dimension for ensemble members                             c
c     n   : number of years                                            c
c     m   : number of ensemble members                                 c
c     ncla: number of classes                                          c
c                                                                      c
c     OUTPUT:                                                          c
c     rpsh: RPS for the hindcast                                       c
c     rpsc: RPS for the reference                                      c
c     skillscore                                                       c
c         : value of the RPSS for the series                           c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE rpss(r,h,ny,n,nens,m,tr,th,ncla,rpsh,rpsc,skillscore)
c
      implicit none
c
      integer ny, n, nens, m, ncla
      integer i, jcla, j
      real r(ny), h(ny,nens)
      real tr(ncla-1), th(ncla-1)
      real ph(ncla), pr(ncla)
      real rpsh, rpsc, skillscore, pclim
      real eps
c
      eps=tiny(eps)
c
c.... probabilities: pr for the reference and ph for the model
c
      rpsh=0.
      rpsc=0.
      do i=1,n
c        write(*,*)'Time step ',i
         do jcla=1,ncla
            ph(jcla)=0.
            pr(jcla)=0.
         enddo
c        write(*,*)'Hindcast probability for ',ncla,' categories'
         do j=1,m
c           write(*,*)'Member ',j,' out of ',m
            if(h(i,j).lt.th(1))ph(1)=ph(1)+1./m
            do jcla=2,ncla-1
               if((h(i,j).lt.th(jcla)).and.
     >            (h(i,j).ge.th(jcla-1)))ph(jcla)=ph(jcla)+1./m
            enddo
            if(h(i,j).ge.th(ncla-1))ph(ncla)=ph(ncla)+1./m
c           write(*,*)'ph= ',ph(1),ph(2),ph(3)
         enddo
c        write(*,*)'Reference probability for ',ncla,' categories'
         if(r(i).lt.tr(1))pr(1)=pr(1)+1.
         do jcla=2,ncla-1
            if((r(i).lt.tr(jcla)).and.
     >         (r(i).ge.tr(jcla-1)))pr(jcla)=pr(jcla)+1.
         enddo
         if(r(i).ge.tr(ncla-1))pr(ncla)=pr(ncla)+1.
c
c.... accummulation of the PDF
c
         do jcla=2,ncla
            ph(jcla)=ph(jcla-1)+ph(jcla)
            pr(jcla)=pr(jcla-1)+pr(jcla)
         enddo
c        write(*,*)'Probabilities'
c        write(*,*)(ph(jcla),jcla=1,ncla)
c        write(*,*)(pr(jcla),jcla=1,ncla)
c
c.... rpss: ratio of hindcast rps (rpsh) and clim. rps (rpsc)
c
         do jcla=1,ncla
            pclim=float(jcla)/ncla
            rpsh=rpsh+(ph(jcla)-pr(jcla))**2
            rpsc=rpsc+(pclim   -pr(jcla))**2
         enddo
      enddo
c     skillscore=(1-rpsh/(rpsc+eps))*100
      skillscore=1.-rpsh/(rpsc+eps)
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE fit1                         R. Hagedorn, 11-Sep-2003 c
c                                                                      c
c     PURPOSE:                                                         c
c     calculates linear fit to given data vector y=a+bx                c
c                                                                      c
c     INPUT:                                                           c
c     x     : input data                                               c
c     y     : input data                                               c
c     ndat  : number of data points                                    c
c                                                                      c
c     OUTPUT:                                                          c
c     a     : abszissa                                                 c
c     b     : slope                                                    c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE fit1(x,y,ndat,a,b)
c
      implicit none
c
      integer i, ndat
      real x(ndat), y(ndat), a, b, sx, sy, ss, sxoss, t, st2
c
      sx=0.
      sy=0.
      st2=0.
      b=0.
c
      do i=1,ndat
         sx = sx + x(i)
         sy = sy + y(i)
      enddo
      ss = float(ndat)
      sxoss = sx/ss
      do i=1,ndat
         t = x(i) - sxoss
         st2 = st2 + t*t
         b = b + t*y(i)
      enddo
      b = b/st2
      a = (sy-sx*b)/ss
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE inflation                P. Doblas-Reyes  23-Apr-2008 c
c                                                                      c
c     PURPOSE:                                                         c
c     inflates ensembles time series in cross-validation               c
c                                                                      c
c     INPUT:                                                           c
c     r      : reference time series                                   c
c     h      : hindcast ensemble time series                           c
c     n      : number of time steps                                    c
c     m      : number of ensemble members                              c
c                                                                      c
c     OUTPUT:                                                          c
c     h      : inflated hindcast ensemble time series                  c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE inflation(r, h, hi, n, m, cv)
      implicit none
c
      real, allocatable :: rr(:), hn(:), hh(:,:)
      real r(n), h(n,m)
      real hi(n,m)
      real hm(n), ha(n*m)
      real ave, adev, sdev, var, skew, curt
      real xr, sr, xm, sm, xe, se, cor
      real alpha, beta
      integer n, m, cv
      integer i, j, k, l, n1, istat
c
c.... ensemble mean and anomalies around the ensemble mean
c
      do i=1,n
         hm(i)=0.
         do j=1,m
            hm(i)=hm(i)+h(i,j)/m
         enddo
         do j=1,m
            h(i,j)=h(i,j)-hm(i)
         enddo
      enddo
c
c.... start crossvalidation (currently, only for one year out)
c
      n1=n-cv ! length of sample with cross-validation
      allocate (rr(n1),                                      stat=istat)
      allocate (hh(n1,m),                                    stat=istat)
      allocate (hn(n1),                                      stat=istat)
      do k=1,n
         l=0
         do i=1,n
            if(cv.ne.0)then
              if(k.ne.i)then
                l=l+1
                do j=1,m
                   hh(l,j)=h(i,j)
                enddo
                hn(l)=hm(i)
                rr(l)=r(i)
              endif
            else
              l=l+1
              do j=1,m
                 hh(l,j)=h(i,j)
              enddo
              hn(l)=hm(i)
              rr(l)=r(i)
            endif
         enddo
         if(l.ne.n1)exit
c
c.... computing variances and correlation
c
         call moment(rr, n1, ave, adev, sdev, var, skew, curt)
         xr=ave
         sr=sdev
         call moment(hn, n1, ave, adev, sdev, var, skew, curt)
         xm=ave
         sm=sdev
         do i=1,n1
            do j=1,m
               l=(i-1)*m+j
               ha(l)=hh(i,j)
            enddo
         enddo
         call moment(ha, n1*m, ave, adev, sdev, var, skew, curt)
         xe=ave
         se=sdev
         call correlation(rr, hn, n1, cor)
c
c.... inflation
c
         alpha=cor/sm
         beta=sqrt(1-cor**2)/se
         alpha=cor*sr/sm
         beta=sr*sqrt(1-cor**2)/se
c        if(k.eq.1)write(*,'(3e10.3)')xr,xm,xe
c        if(k.eq.1)write(*,'(6e12.3)')sr,sm,se,cor,alpha,beta
         do j=1,m
            hi(k,j)=beta*h(k,j)+alpha*(hm(k)-xm)+xm ! note that the inflation is done on anomalies
         enddo
      enddo
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE correlation              P. Doblas-Reyes  23-Apr-2008 c
c                                                                      c
c     PURPOSE:                                                         c
c     computes the correlation between two time series                 c
c                                                                      c
c     INPUT:                                                           c
c     x      : time series 1                                           c
c     y      : time series 2                                           c
c     n      : dimension                                               c
c                                                                      c
c     OUTPUT:                                                          c
c     cor    : correlation                                             c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE correlation(x, y, n, cor)
      implicit none
c
      real x(n), y(n), cor
      real*8 sumx, sumy, varx, vary, cov
      integer n
      integer i
c
      sumx=0.
      sumy=0.
      do i=1,n
         sumx=sumx+x(i)/n
         sumy=sumy+y(i)/n
      enddo
c
      varx=0.
      vary=0.
      cov=0.
      do i=1,n
         varx=varx+(x(i)-sumx)**2
         vary=vary+(y(i)-sumy)**2
         cov=cov+(x(i)-sumx)*(y(i)-sumy)
      enddo
      cor=cov/sqrt(varx*vary)
c
      return
c
      end
c
