c
c----------------------------------------------------------------------c
c     PROGRAM cal_bias                    F. Doblas-Reyes, 03-Jan-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     Reads all available model and reference years to calculate bias  c
c     and ratio of standard deviations on a monthly basis              c
c     Additionally writes climatology and stdev to separate files      c
c     to separate files                                                c
c                                                                      c
c     INPUT:                                                           c
c     MM_EXPT_YYYYMMDDTT and MM_refe_YYYYMMDDTT GRIB files             c
c                                                                      c
c     OUTPUT:                                                          c
c     BIAS_EXPT_YYYY-YYYY_MMDDTT_PAR_LEV                 GRIB file     c
c     CLIM_EXPT_YYYY-YYYY_MMDDTT_PAR_LEV                 GRIB file     c
c     STDV_EXPT_YYYY-YYYY_MMDDTT_PAR_LEV                 GRIB file     c
c     SDRT_EXPT_YYYY-YYYY_MMDDTT_PAR_LEV                 GRIB file     c
c                                                                      c
c     USAGE:                                                           c
c     cal_bias.x < nlist                                               c
c                                                                      c
c     COMPILING (example):                                             c
c     pgf90 -pg -O3 cal_bias.f tools.f -o cal_bias.x $EMOSLIB          c
c                                                                      c
c     MODS:                                                            c
c                                         F. Doblas-Reyes, 12-Jan-2006 c
c     Based on the original cal_bias.f from DEMETER                    c
c                                         F. Doblas-Reyes, 26-Jul-2006 c
c     Adapted to the linux machines                                    c
c                                                                      c
c----------------------------------------------------------------------c
c
      program cal_bias
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: psec4(:)
      real, allocatable :: field_mod(:,:,:,:)
      real, allocatable :: field_ref(:,:,:,:)
      real, allocatable :: mod(:)
      real, allocatable :: ref(:)
      real, allocatable :: xhlp_mod(:)
      real, allocatable :: xhlp_ref(:)
      integer, allocatable :: ivfdate(:,:)
      integer, allocatable :: istep(:,:)
      integer, allocatable:: mask(:), mask_ref(:), mask_mod(:)
c
c.... other definitions
c
      character yifile*45, yofile*60, ysfile*60
      character*21 expt, expti
      character*5 form1
      integer nx, ny, nmon, nens, iyy1, iyy2, nyear
      integer imm, idd, itt, ipar, ilev, idatetype
      integer iunit, ounit0, ounit1, ounit2, ounit3, ounit4, ounit5
      integer iy, iens, iensm1, ik, imon, istat, nxny
      integer i, kret, idat, lena, ind, explen, exptl
      real rmiss, xfac, yfac
      real ave, adev, sdev, var, skew, curt, eps
c
c.... GRIB headers
c
      integer ksec0(2)
      integer ksec1(1024)
      integer ksec1_mod(1024)
      integer ksec1_ref(1024)
      integer ksec2(1024)
      integer ksec3(2)
      integer ksec4(512)
c
      real psec2(512)
      real psec3(2)
c
c.... namelist definition
c
      namelist /control/   nx,   ny, nmon, nens,
     >                   iyy1, iyy2,  imm,  idd,  itt,
     >                   ipar, ilev, expt
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
      ipar  = 139
      ilev  = 000
      expt  = 'scwc'
c
      read(5,control)
      write(*,*)'  nx:  ',nx
      write(*,*)'  ny:  ',ny
      write(*,*)'nmon:  ',nmon
      write(*,*)'nens:  ',nens
      write(*,*)'iyy1:  ',iyy1
      write(*,*)'iyy2:  ',iyy2
      write(*,*)' imm:  ',imm
      write(*,*)' idd:  ',idd
      write(*,*)' itt:  ',itt
      write(*,*)'ipar:  ',ipar
      write(*,*)'ilev:  ',ilev
      write(*,*)'expt:   ',expt
c
c.... set missing data and other indicators
c
c      rmiss=-1.e30
      rmiss=-9999
      
      nxny=nx*ny
      nyear=iyy2-iyy1+1
      explen=lena(expti)
      eps=1.e-20
c
c.... allocate fields
c
      allocate (psec4(nxny),                                 stat=istat)
      allocate (field_mod(nxny,nmon,nens,nyear),             stat=istat)
      allocate (field_ref(nxny,nmon,1,nyear),                stat=istat)
      allocate (mod(nxny),                                   stat=istat)
      allocate (ref(nxny),                                   stat=istat)
      allocate (xhlp_mod(nyear*nens),                        stat=istat)
      allocate (xhlp_ref(nyear),                             stat=istat)
      allocate (ivfdate(nmon,nyear),                         stat=istat)
      allocate (istep(nmon,nyear),                           stat=istat)
      allocate (mask(nxny),                                  stat=istat)
      allocate (mask_ref(nxny),                              stat=istat)
      allocate (mask_mod(nxny),                              stat=istat)
c
c.... loop over reading model and reference data
c

      do idat=1,2
         expti=expt
         exptl=explen
         if(idat.eq.2)then
           expti='refe'
           exptl=4
         endif
         write(form1,'(a,i2.2,a)')'(a',exptl,')'
c
c.... read all model years
c
         do iy=1,nyear
            yifile='MM_EXPT_YYYYMMDDTT'
            write(yifile(       4: 3+exptl),form1)expti(1:exptl)
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
            if(idat.eq.1)then
              idatetype=0
              call readgrb(iunit, nxny, nmon, nens, idat,
     >           imm, iyy1, iy, ipar, ilev, idatetype,
     >           field_mod(1,1,1,iy), ivfdate(1,iy), istep(1,iy),
     >           ksec0, ksec1_mod, ksec2, ksec3, ksec4, psec2, psec3)
            endif
            if(idat.eq.2)then
              idatetype=1
              call readgrb(iunit, nxny, nmon, 1, idat,
     >           imm, iyy1, iy, ipar, ilev, idatetype,
     >           field_ref(1,1,1,iy), ivfdate(1,iy), istep(1,iy),
     >           ksec0, ksec1_ref, ksec2, ksec3, ksec4,psec2, psec3)
            endif
            call pbclose(iunit,kret)
            if(kret.ne.0)then
              write(*,*)'Error in closing file: kret=',kret
              goto 997
            endif
         enddo !iy loop
      enddo !idat loop
c     do iy=1,nyear
c        write(*,*)(ivfdate(imon,iy),imon=1,nmon)
c        write(*,*)(istep(imon,iy),imon=1,nmon)
c     enddo
c
c.... open output files
c
      exptl  = explen
      write(form1,'(a,i2.2,a)')'(a',exptl,')'
      ysfile='_EXPT_YYYY-YYYY_MMDDTT_PAR_LEV'
      write(ysfile(       2: 1+exptl),form1)expt(1:exptl)
      write(ysfile( 2+exptl: 2+exptl),'(a1)')'_'
      write(ysfile( 3+exptl: 6+exptl),'(i4.4)')iyy1
      write(ysfile( 7+exptl: 7+exptl),'(a1)')'-'
      write(ysfile( 8+exptl:11+exptl),'(i4.4)')iyy2
      write(ysfile(12+exptl:12+exptl),'(a1)')'_'
      write(ysfile(13+exptl:14+exptl),'(i2.2)')imm
      write(ysfile(15+exptl:16+exptl),'(i2.2)')idd
      write(ysfile(17+exptl:18+exptl),'(i2.2)')itt
      write(ysfile(19+exptl:19+exptl),'(a1)')'_'
      write(ysfile(20+exptl:22+exptl),'(i3.3)')ipar
      write(ysfile(23+exptl:23+exptl),'(a1)')'_'
      write(ysfile(24+exptl:26+exptl),'(i3.3)')ilev
      yofile='BIAS'//ysfile
      write(*,*)'open output file: ',yofile
      kret=0
      call pbopen(ounit0,yofile,'w',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
c
      yofile='CLIM'//ysfile
      write(*,*)'open output file: ',yofile
      kret=0
      call pbopen(ounit1,yofile,'w',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
c
      yofile='STDV'//ysfile
      write(*,*)'open output file: ',yofile
      kret=0
      call pbopen(ounit3,yofile,'w',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
c
      yofile='SDRT'//ysfile
      write(*,*)'open output file: ',yofile
      kret=0
      call pbopen(ounit5,yofile,'w',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
c
      exptl=4
      ysfile='_EXPT_YYYY-YYYY_MMDDTT_PAR_LEV'
      write(ysfile(       2: 1+exptl),'(a4)')'refe'
      write(ysfile( 2+exptl: 2+exptl),'(a1)')'_'
      write(ysfile( 3+exptl:6+exptl),'(i4.4)')iyy1
      write(ysfile( 7+exptl: 7+exptl),'(a1)')'-'
      write(ysfile( 8+exptl:11+exptl),'(i4.4)')iyy2
      write(ysfile(12+exptl:12+exptl),'(a1)')'_'
      write(ysfile(13+exptl:14+exptl),'(i2.2)')imm
      write(ysfile(15+exptl:16+exptl),'(i2.2)')idd
      write(ysfile(17+exptl:18+exptl),'(i2.2)')itt
      write(ysfile(19+exptl:19+exptl),'(a1)')'_'
      write(ysfile(20+exptl:22+exptl),'(i3.3)')ipar
      write(ysfile(23+exptl:23+exptl),'(a1)')'_'
      write(ysfile(24+exptl:26+exptl),'(i3.3)')ilev
      yofile='CLIM'//ysfile
      write(*,*)'open output file: ',yofile
      kret=0
      call pbopen(ounit2,yofile,'w',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
c
      yofile='STDV'//ysfile
      write(*,*)'open output file: ',yofile
      kret=0
      call pbopen(ounit4,yofile,'w',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
c
c.... for every lead time: 
c.... convert accumulation to average over the years and calculate bias
c
      xfac = 1./float(nyear*nens)
      yfac = 1./float(nyear)
      do imon=1,nmon
         write(*,*)'Month ',imon
         do i=1,nxny
            mod(i)=0.
            ref(i)=0.
            mask(i)=0
            mask_ref(i)=0
            mask_mod(i)=0
            do iy=1,nyear
c	       write(*,*) field_ref(i,imon,1,iy)
               if((field_ref(i,imon,1,iy).eq.rmiss).or.
     >            (ref(i).eq.rmiss))then
                 ref(i)=rmiss
               else
                 ref(i)=ref(i)+field_ref(i,imon,1,iy)
               endif
               do iens=1,nens
                  if((field_mod(i,imon,iens,iy).eq.rmiss).or.
     >               (mod(i).eq.rmiss))then
                    mod(i)=rmiss
                  else
                    mod(i)=mod(i)+field_mod(i,imon,iens,iy)
                  endif
               enddo
            enddo
            if(ref(i).ne.rmiss)then
              ref(i)=ref(i)*yfac
            else
              mask_ref(i)=1
            endif
            if(mod(i).ne.rmiss)then
              mod(i)=mod(i)*xfac
            else
              mask_mod(i)=1
            endif
            if((ref(i).ne.rmiss).and.(mod(i).ne.rmiss))then
              psec4(i)=mod(i)-ref(i)
            else
              psec4(i)=rmiss
              mask(i)=1
c              write(*,*)imon,i,psec4(i)
            endif
         enddo
c
c.... write bias to output file
c
         do ik=1,1024
            ksec1(ik)=ksec1_mod(ik)
         enddo
         ksec1(6)=ipar
         ksec1(8)=ilev
         ksec1(16)=istep(imon,1)
         ksec1(46)=ivfdate(imon,1)
         ksec1(48)=imon
         psec3(2)=rmiss
         write(*,*)'Write model bias for verification date ',ksec1(46)
         call writegrb(ounit0, nxny, psec4,
     >      ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
c
c.... write model climatology
c
         do i=1,nxny 
            psec4(i)=mod(i)
         enddo
         write(*,*)'Write model climatology'
         call writegrb(ounit1, nxny, psec4,
     >      ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
c
c.... write reference climatology 
c
         ksec1(41)=ksec1_ref(41)
         do i=1,nxny 
            psec4(i)=ref(i)
         enddo
         write(*,*)'Write reference climatology'
         call writegrb(ounit2, nxny, psec4,
     >      ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
c
c.... calculate and write model stdev
c
         do i=1,nxny
            ind=0
            do iy=1,nyear
               do iens=1,nens
                  ind=ind+1
                  xhlp_mod(ind)=field_mod(i,imon,iens,iy)
               enddo
            enddo
            call moment(xhlp_mod,ind,ave,adev,sdev,var,skew,curt)
            mod(i)=sdev
            if(mask_mod(i).eq.0)then
              psec4(i)=sdev
            else
              psec4(i)=rmiss
            endif
         enddo
         ksec1(41)=ksec1_mod(41)
         write(*,*)'Write model standard deviation'
         call writegrb(ounit3, nxny, psec4,
     >      ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
c
c.... calculate and write reference stdev
c
         do i=1,nxny 
            do iy=1,nyear
               xhlp_ref(iy)=field_ref(i,imon,1,iy)
            enddo
            call moment(xhlp_ref,nyear,ave,adev,sdev,var,skew,curt)
            ref(i)=sdev
            if(mask_mod(i).eq.0)then
              psec4(i)=sdev
            else
              psec4(i)=rmiss
            endif
         enddo
         ksec1(41)=ksec1_ref(41)
         write(*,*)'Write reference standard deviation'
         call writegrb(ounit4, nxny, psec4,
     >      ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
c
c.... calculate and write ratio of model to reference stdev
c
         do i=1,nxny 
	   if(mask_ref(i).eq.0) then
            if(ref(i).gt.eps)then
              psec4(i)=mod(i)/ref(i)
c	      write(*,*) 'MASK is',mask(i)
c	      write(*,*) 'Mod is',mod(i)
c              write(*,*) 'Ref is',ref(i)
c              write(*,*) 'mod/ref (psec4) is',psec4(i)
            else
              psec4(i)=0.
	      write(*,*) 'ref(i)is not gt than eps so psec is ',psec4(i)
            endif
	   else
	      psec4(i)=rmiss
	   endif
c            if(mask(i).eq.1)psec4(i)=rmiss
         enddo
 	 write(*,*) 'SDRT is ',psec4 	    
         ksec1(41)=ksec1_mod(41)
         write(*,*)'Write model/reference standard deviation'
         call writegrb(ounit5, nxny, psec4,
     >      ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
      enddo
      call pbclose(ounit0,kret)
      call pbclose(ounit1,kret)
      call pbclose(ounit2,kret)
      call pbclose(ounit3,kret)
      call pbclose(ounit4,kret)
      call pbclose(ounit5,kret)
      if(kret.ne.0)then
        write(*,*)'Error in closing file: kret=',kret
        goto 997
      endif
c
      if(kret.eq.0)goto 998
 997  continue
      write(*,*)'Sorry, no success :('
      call abort
      goto 999
 998  continue
      write(*,*)'program seems to be successfully finished :)'
 999  continue
c
      end
c
