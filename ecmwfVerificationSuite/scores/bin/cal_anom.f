c
c----------------------------------------------------------------------c
c     PROGRAM cal_anom                    F. Doblas-Reyes, 12-Jan-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     Calculates anomaly of the given year with respect to the         c
c     climatology of a given time range (in cross-validation           c
c     excluding CV years).                                             c
c                                                                      c
c     INPUT:                                                           c
c     MM_EXPT_YYYYMMDDTT and MM_refe_YYYYMMDDTT GRIB files             c
c                                                                      c
c     OUTPUT:                                                          c
c     ANOM_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_YYYY GRIB files         c
c                                                                      c
c     USAGE:                                                           c
c     cal_anom.x < nlist                                               c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 cal_anom.f tools.f -o cal_anom.x $EMOSLIB          c
c                                                                      c
c     MODS:                                                            c
c                                         F. Doblas-Reyes, 12-Jan-2006 c
c     Based on the original cal_anom.f from DEMETER                    c
c                                         F. Doblas-Reyes, 26-Jul-2006 c
c     Adapted to the linux machines                                    c
c                                                                      c
c----------------------------------------------------------------------c
c
      program cal_anom
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: psec4(:),lsmask(:)
      real, allocatable :: field(:,:,:,:)
      real, allocatable :: ano(:,:,:), cli(:,:)
      integer, allocatable :: ivfdate(:,:), istep(:,:)
      integer, allocatable:: mask(:)
c
c.... other definitions
c
      character yifile*45, yofile*65
      character*21 expt
      character*5 form1
      integer nx, ny, nmon, nmod, nenm
      integer iyy1, iyy2, iyya, imm, idd, itt
      integer cros, anin, ipar, ilev, idatetype
      integer nens, nyear, nxny, ncv, nyear_cros, exptl
      integer iunit, ounit, lsunit
      integer iyy, iyc, idat
      integer istat, lena, kret
      integer i, imon, iens, iensp1, iy, ik, ic, ia
      real ravg
      real rmiss
      integer nmod_max
c
c.... GRIB headers
c
      integer ksec0(2)
      integer ksec1(1024)
      integer ksec1_sav(1024)
      integer ksec2(1024)
      integer ksec3(2)
      integer ksec4(512)
      real psec2(512)
      real psec3(2)
c
c
c.... dummy GRIB headers for land surface file
c
      integer dksec0(2)
      integer dksec1(1024)
      integer dksec1_sav(1024)
      integer dksec2(1024)
      integer dksec3(2)
      integer dksec4(512)
c
      real dpsec2(512)
      real dpsec3(2)
c
c.... namelist definition
c
      namelist /control/   nx,   ny, nmon,
     >                   iyy1, iyy2,  imm,  idd,  itt,
     >                   cros, ipar, ilev, iyya,
     >                   nens, anin, expt
c
c.... set default values and read control namelist
c
      nx    = 144
      ny    = 71
      nmon  = 6
      nens  = 9
      iyy1  = 1991
      iyy2  = 2001
      imm   = 11
      idd   = 1
      itt   = 0
      cros  = 1
      iyya  = 1987
      ipar  = 139
      ilev  = 000
      anin  = 0
      expt = 'mm01'
c
      read(5,control)
      write(*,*)'  nx: ',nx
      write(*,*)'  ny: ',ny
      write(*,*)'nmon: ',nmon
      write(*,*)'nens: ',nens
      write(*,*)'iyy1: ',iyy1
      write(*,*)'iyy2: ',iyy2
      write(*,*)' imm: ',imm
      write(*,*)' idd: ',idd
      write(*,*)' itt: ',itt
      write(*,*)'cros: ',cros
      write(*,*)'iyya: ',iyya
      write(*,*)'ipar: ',ipar
      write(*,*)'ilev: ',ilev
      write(*,*)'anin: ',anin
      write(*,*)'expt: ',expt
c
      rmiss=-9999
      nxny=nx*ny
      nyear=iyy2-iyy1+1
      exptl=lena(expt)
      if(expt.eq.'refe')then
        exptl=4
      endif
      write(form1,'(a,i2.2,a)')'(a',exptl,')'
      ncv=(cros-1)/2
c
c.... allocate fields
c
      allocate (psec4(nxny),                                 stat=istat)
      allocate (lsmask(nxny),                                stat=istat)
      allocate (mask(nxny),                                  stat=istat)
      allocate (ivfdate(nmon,nyear),                         stat=istat)
      allocate (istep(nmon,nyear),                           stat=istat)
c
c.... open output file
c
      yofile='ANOM_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_YYYY'
      write(yofile(       6: 5+exptl),form1)expt(1:exptl)
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
      write(yofile(30+exptl:30+exptl),'(i1.1)')anin
      write(yofile(31+exptl:31+exptl),'(a1)')'_'
      write(yofile(32+exptl:34+exptl),'(i3.3)')ipar
      write(yofile(35+exptl:35+exptl),'(a1)')'_'
      write(yofile(36+exptl:38+exptl),'(i3.3)')ilev
      write(yofile(39+exptl:39+exptl),'(a1)')'_'
      write(yofile(40+exptl:43+exptl),'(i4.4)')iyya
      write(*,*)'open output file: ',yofile
      kret=0 
      call pbopen(ounit,yofile,'w',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret 
        goto 997
      endif
c
c.... loop over number of experiments (in case of nmod > 1 they form the multi-model)
c
      idat=1
      idatetype=0
      if(expt.eq.'refe')then
        idat=2
        nens=1
        idatetype=1
      endif
      allocate (field(nxny,nmon,nens,nyear),                 stat=istat)
      allocate (ano(nxny,nmon,nens),                         stat=istat)
      allocate (cli(nxny,nmon),                              stat=istat)
      ano(:,:,:)=0.
      cli(:,:)=0.
c
c.... read all years
c
      do iy=1,nyear
         yifile='MM_EXPT_YYYYMMDDTT'
         write(yifile(       4: 3+exptl),form1)expt(1:exptl)
         write(yifile( 4+exptl: 4+exptl),'(a1)')'_'
         write(yifile( 5+exptl: 8+exptl),'(i4.4)')iy+iyy1-1
         write(yifile( 9+exptl:10+exptl),'(i2.2)')imm
         write(yifile(11+exptl:12+exptl),'(i2.2)')idd
         write(yifile(13+exptl:14+exptl),'(i2.2)')itt
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
         call readgrb(iunit, nxny, nmon, nens, idat,
     >      imm, iyy1, iy, ipar, ilev, idatetype,
     >      field(1,1,1,iy), ivfdate(1,iy), istep(1,iy),
     >      ksec0, ksec1_sav, ksec2, ksec3, ksec4, psec2, psec3)
         call pbclose(iunit,kret)
         if(kret.ne.0)then
           write(*,*)'Error in closing file: kret=',kret
           goto 997
         endif
      enddo !iy loop
      
c      
c....read land sea mask -DM, fixing soil moisture issue      
c
c      kret=0
c      call pbopen(lsunit,'LSM_data','r',kret)
c      if(kret.ne.0)then
c           write(*,*)'Error in opening file: kret=',kret
c           goto 997
c      endif
c      
c      call read1grb(lsunit, nxny, lsmask,
c     >      dksec0, dksec1_sav, dksec2, dksec3, dksec4, dpsec2, dpsec3)
c      call pbclose(lsunit,kret)
c      if(kret.ne.0)then
c           write(*,*)'Error in closing file: kret=',kret
c           goto 997
c      endif
      
c
c.... now distribute data to clim or ano fields
c
      do imon=1,nmon
         write(*,*)'Month ',imon
c
c.... create mask for missing data
c.... points for which one reference or model value
c.... are missing are not taken into account in the
c.... computations
c
         do i=1,nxny
            mask(i)=0
            do iy=1,nyear
               do iens=1,nens
c	       write(*,*) field(i,imon,iens,iy)
                  if(field(i,imon,iens,iy).eq.rmiss)mask(i)=1
               enddo
            enddo
         enddo
c
         nyear_cros=0
         do iy=1,nyear
            iyy=iyy1+iy-1
c
c.... ic and ia determine whether a year will be used
c.... to compute the climate or the anomaly
c
            ic=0
            ia=0
            do iyc=-ncv,ncv
               if(iyy.ne.(iyya+iyc))then
c                write(*,*)'Year ',iyy,' used for climate'
                 ic=1
                 ia=0
               endif
            enddo !iyc loop
            if(iyy.eq.iyya)then
c             write(*,*)'Year ',iyy,' used for anomaly'
              ic=0
              ia=1
            endif
            if((ic.eq.1).and.(ia.eq.0))then
              nyear_cros=nyear_cros+1
              do iens=1,nens
                 do i=1,nxny
                    cli(i,imon)=cli(i,imon)+field(i,imon,iens,iy)
                 enddo !i loop
              enddo !iens loop
            endif
            if((ic.eq.0).and.(ia.eq.1))then
              do iens=1,nens
                 do i=1,nxny
                    ano(i,imon,iens)=field(i,imon,iens,iy)
                 enddo !i loop
              enddo !iens loop
            endif
         enddo !iy loop
c
c.... for every lead time convert accumulation to average climate,
c
         ravg=float(nyear_cros*nens)
         do i=1,nxny
            cli(i,imon)=cli(i,imon)/ravg
         enddo !i loop
c
c.... calculate the anomaly for every ensemble member
c
         do iens=1,nens
            do i=1,nxny
c
c.... deal with missing data
c
               if(mask(i).eq.1)then
                 psec4(i)=psec3(2)
               else
                 psec4(i)=ano(i,imon,iens)-cli(i,imon)
               endif
	       
c
c.... deal with ocean masking in the soil moisture case
c
               if (ipar.eq.039) then 
               if(lsmask(i).eq.1)then
                 psec4(i)=rmiss
		 write(*,*)'psec3(2) is',psec3(2)
               endif
	       endif
	       
	       
	       
	       
            enddo !i loop
c
c.... finally write anomaly data to the output file
c
            do ik=1,1024
               ksec1(ik)=ksec1_sav(ik)
            enddo !ik loop
            if(iyya.gt.2000)then
              ksec1(10)=iyya-2000
              ksec1(21)=21
            else
              ksec1(10)=iyya-1900
              ksec1(21)=20
            endif
            ksec1(16)=istep(imon,iyya-iyy1+1)
            ksec1(42)=iens-1
            ksec1(46)=ivfdate(imon,iyya-iyy1+1)
            ksec1(48)=imon
            if(expt.eq.'refe')then
              ksec1(12)=1
              ksec1(13)=0
              ksec1(16)=imon*30*24
              ksec1(17)=0
              ksec1(18)=10
              ksec1(37)=16
              ksec1(38)=11
              ksec1(39)=80
              ksec1(40)=1221
              ksec1(42)=0
              ksec1(43)=0
              ksec1(44)=0
              ksec1(45)=0
              iyy=(imon+imm-1)/12
              ksec1(46)=(iyya+iyy)*100+(imon+imm-1)-iyy*12
            endif
            call writegrb(ounit, nxny, psec4,
     >         ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
         enddo !iens loop
      enddo !imon loop
      deallocate (field,                                  stat=istat)
      deallocate (lsmask,                                 stat=istat)
      deallocate (ano,                                    stat=istat)
c
      call pbclose(ounit,kret)
      if(kret.ne.0)then
        write(*,*)'Error in closing file: kret=',kret
        goto 997
      endif
c
      if(kret.eq.0)goto 998
 997  continue
      write(*,*)'Sorry, no success :('
      call abort
 998  write(*,*)'program seems to be successfully finished :)'
c
      end
c
