c
c----------------------------------------------------------------------c
c     PROGRAM cal_read                    F. Doblas-Reyes, 21-Sep-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     Reads and print seasonal forecast data for Thomas to play with   c
c                                                                      c
c     INPUT:                                                           c
c     ANOM_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_YYYY GRIB files         c
c                                                                      c
c     OUTPUT:                                                          c
c                                                                      c
c     USAGE:                                                           c
c     cal_read.x < nlist                                               c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 cal_read.f tools.f -o cal_read.x $EMOSLIB $NAGLIB  c
c                                                                      c
c     MODS:                                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      program cal_read
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable:: ano_mod(:,:,:,:), ano_ref(:,:,:,:)
      real, allocatable:: sim(:)
      real, allocatable:: regwgt(:), latwgt(:)
      real, allocatable:: rlat(:), rlon(:)
      integer, allocatable :: ivfdate(:,:), istep(:,:)
      integer, allocatable:: lsm(:)
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
      character yifile*90, yofile*90, yrfile*90, yfile*100
      character*5 form1
      character*30 form2
      character*21 expt, expti
      real xlats, xlatn, xlonw, xlone
      real yhlp, xhlp
      real totwgt
      real rint
      integer nx, ny, nmon, nens
      integer iyy1, iyy2, imm, idd, itt
      integer ipar, ilev, anin, nf1, nf2, nreg, cros, idatetype
      integer lena, istat, klenp, exptl, nlat, nlon
      integer nxny, nensp1, nensm1, nyear, idat
      integer i, nummon, num1, num2
      integer imon, iens, jens, ie, ir, iy, ik
      integer iunit
      integer iilsm, icatch
c
c.... hard coded field definitions
c
      integer nreg_max
      parameter(nreg_max=48)
      character*4 namr(nreg_max)
      real limn(nreg_max), lims(nreg_max) 
      real limw(nreg_max), lime(nreg_max)
      integer ilsm(nreg_max), rcal(nreg_max)
c
      namelist /control/   nx,   ny, nmon, nens,
     >                   iyy1, iyy2,  imm,  idd,  itt,
     >                   cros, ipar, ilev, anin,  nf1,  nf2,
     >                   expt, nreg
      namelist /region/  namr, limn, lims, limw, lime, ilsm, rcal
c
c.... set default values and read input namelists
c
      nx = 144
      ny = 71
      nmon = 6
      nens = 9 
      iyy1 = 0
      iyy2 = 0
      imm  = 11
      idd  = 1
      itt  = 0
      cros = 1
      ipar = 139
      ilev = 000
      anin = 0
      expt = 'scwc'
      nreg = 12
c
      read(5,control)
      write(*,*)'  nx: ', nx
      write(*,*)'  ny: ', ny
      write(*,*)'nmon: ',nmon
      write(*,*)'nens: ',nens
      write(*,*)'iyy1: ',iyy1
      write(*,*)'iyy2: ',iyy2
      write(*,*)' imm: ',imm
      write(*,*)' idd: ',idd
      write(*,*)' itt: ',itt
      write(*,*)'cros: ',cros
      write(*,*)'ipar: ',ipar
      write(*,*)'ilev: ',ilev
      write(*,*)'expt: ',expt
      write(*,*)'anin: ',anin
      write(*,*)' nf1: ',nf1
      write(*,*)' nf2: ',nf2
      write(*,*)'nreg: ',nreg
c
      if(nreg.gt.nreg_max)then
        write(*,*)'Problems with the number of regions'
        goto 998
      endif
      read(5,region)
      write(*,*)'namr: ',(namr(i),i=1,nreg)
      write(*,*)'limn: ',(limn(i),i=1,nreg)
      write(*,*)'lims: ',(lims(i),i=1,nreg)
      write(*,*)'limw: ',(limw(i),i=1,nreg)
      write(*,*)'lime: ',(lime(i),i=1,nreg)
      write(*,*)'ilsm: ',(ilsm(i),i=1,nreg)
      write(*,*)'rcal: ',(rcal(i),i=1,nreg)
c
      nensm1 = nens-1
      nensp1 = nens+1
      nxny=nx*ny
      klenp=nxny
      nyear=iyy2-iyy1+1
c
c.... allocate fields
c
      allocate (ano_mod(nxny,nmon,nens,nyear),               stat=istat)
      allocate (ano_ref(nxny,nmon,1,nyear),                  stat=istat)
      allocate (ivfdate(nmon,nyear),                         stat=istat)
      allocate (istep(nmon,nyear),                           stat=istat)
      allocate (regwgt(nxny),                                stat=istat)
      allocate (latwgt(nxny),                                stat=istat)
      allocate (rlon(nxny),                                  stat=istat)
      allocate (rlat(nxny),                                  stat=istat)
      allocate (lsm(nxny),                                   stat=istat)
c
c.... loop over reading model and reference data
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
      nummon=nf2-nf1+1
c
c.... loop over regions
c
      write(form2,'(a,i2.2,a)')'(2i8,',(nens+1),'f10.4)'
      write(*,*)'Output format ',form2
c
      do idat=1,2
         expti=expt
         if(idat.eq.2)then
            expti='refe'
         endif
         exptl=lena(expti)
         write(form1,'(a,i2.2,a)')'(a',exptl,')'
         yfile='ANOM_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV'
         write(yfile(       6: 5+exptl),form1)expti(1:exptl)
         write(yfile( 6+exptl: 6+exptl),'(a1)')'_'
         write(yfile( 7+exptl:10+exptl),'(i4.4)')iyy1
         write(yfile(11+exptl:11+exptl),'(a1)')'-'
         write(yfile(12+exptl:15+exptl),'(i4.4)')iyy2
         write(yfile(16+exptl:16+exptl),'(a1)')'_'
         write(yfile(17+exptl:18+exptl),'(i2.2)')imm
         write(yfile(19+exptl:20+exptl),'(i2.2)')idd
         write(yfile(21+exptl:22+exptl),'(i2.2)')itt
         write(yfile(23+exptl:25+exptl),'(a3)')'_CV'
         write(yfile(26+exptl:27+exptl),'(i2.2)')cros
         write(yfile(28+exptl:29+exptl),'(a2)')'_I'
         if(idat.eq.2)then
           write(yfile(30+exptl:30+exptl),'(i1.1)')0
         else
           write(yfile(30+exptl:30+exptl),'(i1.1)')anin
         endif
         write(yfile(31+exptl:31+exptl),'(a1)')'_'
         write(yfile(32+exptl:34+exptl),'(i3.3)')ipar
         write(yfile(35+exptl:35+exptl),'(a1)')'_'
         write(yfile(36+exptl:38+exptl),'(i3.3)')ilev
         if(idat.eq.1)then
           yofile=yfile
         endif
         if(idat.eq.2)then
           yrfile=yfile
         endif
      enddo
c
      do ir=1,nreg
         if(rcal(ir).eq.1)then
c
c.... select parameters for the region
c
           write(*,*)'Computing for region: ',namr(ir)
           iilsm=ilsm(ir)
           xlats=lims(ir)
           xlatn=limn(ir)
           xlonw=limw(ir)
           xlone=lime(ir)
           icatch = 0
           call get_mask(iilsm,lsm,nxny,expt)
c          write(*,*)'lsm'
c          write(*,'(40i4)')(lsm(i),i=1,nxny)
c          write(*,*) xlats, xlatn, xlonw, xlone
           call get_region(icatch, xlats, xlatn, xlonw, xlone,
     >        nxny, expt, '/vol/demeter/verify', regwgt, latwgt
     >        rlat, rlon, nlat, nlon, rint)
c          write(*,*)'regwgt'
c          write(*,'(40f4.1)')(regwgt(i),i=1,nxny)
c          write(*,*)'latwgt'
c          write(*,'(20f8.3)')(latwgt(i),i=1,nxny)
           num1=0
           do i=1,klenp
              if(regwgt(i).gt.0.)num1=num1+1
           enddo
c
c.... open output file
c
           yfile=yofile(1:lena(yofile))//'_'//namr(ir)
           write(*,*)'open output file: ',yfile
           open(10,file=yfile,form='formatted',status='unknown')
           yfile=yrfile(1:lena(yrfile))//'_'//namr(ir)
           write(*,*)'open output file: ',yfile
           open(11,file=yfile,form='formatted',status='unknown')
c
c.... computation for single members
c
           allocate (sim(nens),                              stat=istat)
           write(10,'(2i8)')nf1,nf2
           do iy=1,nyear
              do i=1,klenp
                 if(regwgt(i).gt.0.)then
                   yhlp=0.
                   do imon=nf1,nf2
                      yhlp=yhlp+ano_ref(i,imon,1,iy)
                   enddo
                   write(11,'(2i8,f10.4)')iyy1+iy-1,i,yhlp
                   do iens=1,nens
                      xhlp=0.
                      do imon=nf1,nf2
                         xhlp=xhlp+ano_mod(i,imon,iens,iy)
                      enddo !imon
                      sim(iens)=xhlp/float(nummon)
                   enddo
                   write(10,form2)iyy1+iy-1,i,(sim(iens),iens=1,nensp1) 
                 endif
              enddo !i
           enddo !iy loop
           deallocate (sim,                                  stat=istat)
           close(10)
           close(11)
         endif
      enddo
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
