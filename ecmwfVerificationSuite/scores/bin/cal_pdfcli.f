c
c----------------------------------------------------------------------c
c     PROGRAM cal_pdfcli                  F. Doblas-Reyes 01-Jul-2008  c
c                                                                      c
c     PURPOSE:                                                         c
c     Computes the percentiles and the climatological pdf at each grid c
c     point with the possibility of using anomalies or full fields     c
c                                                                      c
c     INPUT:                                                           c
c     ANOM_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_YYYY GRIB files         c
c                                                                      c
c     OUTPUT:                                                          c
c     PERC_EXPT_YYYY-YYYY_MMDDTT_CV_IN_PAR_LEV_TYP_PP_III-EEE.grb      c
c                                                          GRIB files  c
c                                                                      c
c     USAGE:                                                           c
c     cal_pdfcli.x < nlist                                             c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 cal_pdfcli.f tools.f -o cal_pdfcli.x $EMOSLIB $NAGLIB      c
c                                                                      c
c     MODS:                                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      program cal_pdfcli
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: ano_mod(:,:,:,:), ano_ref(:,:,:,:)
      real, allocatable :: cli_mod(:,:,:,:), cli_ref(:,:,:)
      real, allocatable :: arrh(:), arrr(:)
      real, allocatable :: tr(:)
      real, allocatable :: thresh(:,:), thresr(:,:)
      real, allocatable :: dat_ref(:,:), dat_mod(:,:,:)
      real, allocatable :: xr(:,:), xh(:,:), yr(:,:), yh(:,:)
      real, allocatable :: pdfr(:), pdfh(:)
      integer, allocatable :: lsm(:)
      integer, allocatable :: ivfdate(:,:), istep(:,:)
      integer, allocatable :: ivfdate_c(:), istep_c(:)
      integer, allocatable:: maskr(:), maskh(:)
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
      character*3 type
      integer nx, ny, nmon, nens
      integer iyy1, iyy2, imm, idd, itt, permeth
      integer cros, ipar, ilev, anin,  nf1,  nf2, ndat
      integer idatetype, nmod, nensin, full_field
      integer lena, istat, exptl
      integer nxny, nensp1, nensm1, nyear, idat, nummon
      integer iy, iens, i, icla, imon, iensp1, ik, imod, iiens
      integer iunit, ounit1
      integer inmod, inref, iilsm
      integer ibin
      real sigma
      real ave, adev, sdev, var, skew, curt
      real sum
      real eps
      real rmiss
c
c.... hard coded field definitions
c
      integer ncla, nbin
      parameter(ncla=100,nbin=201)
      real x0, xint
      parameter(x0=-3.,xint=-2.*x0/nbin)
c
c.... namelist
c
      namelist /control/   nx,   ny, nmon, nens,
     >                   iyy1, iyy2,  imm,  idd,  itt,
     >                   cros, ipar, ilev, anin,  nf1,  nf2,
     >                   expt, ndat, type, permeth, sigma
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
      write(*,*)'type: ',type
      write(*,*)'permeth: ',permeth
      write(*,*)'sigma: ',sigma
c
      rmiss=-1.e30
      nensm1 = nens-1
      nensp1 = nens+1
      nxny=nx*ny
      nyear=iyy2-iyy1+1
      nummon=nf2-nf1+1
c
      eps      = 1.e-30
c
c.... allocate fields
c
      allocate (ano_mod(nxny,nmon,nens,nyear),               stat=istat)
      allocate (ano_ref(nxny,nmon,1,nyear),                  stat=istat)
      allocate (cli_ref(nxny,nmon,1),                        stat=istat)
      allocate (ivfdate(nmon,nyear),                         stat=istat)
      allocate (ivfdate_c(nmon),                             stat=istat)
      allocate (istep(nmon,nyear),                           stat=istat)
      allocate (istep_c(nmon),                               stat=istat)
      allocate (lsm(nxny),                                   stat=istat)
      allocate (maskr(nxny),                                 stat=istat)
      allocate (maskh(nxny),                                 stat=istat)
      allocate (thresr(nxny,ncla),                           stat=istat)
      allocate (thresh(nxny,ncla),                           stat=istat)
      allocate (dat_ref(nxny,nyear),                         stat=istat)
      allocate (dat_mod(nxny,nyear,nens),                    stat=istat)
      allocate (psec4(nxny),                                 stat=istat)
c
      iilsm=0
      call get_mask(iilsm,lsm,nxny,expt)
c
c.... loop over reading model and ref data
c....    idat=1 for model
c....    idat=2 for reference
c
      nmod=1
      if(type.eq.'ful')then
        full_field=1
c
c.... special definitions for DEMETER/ENSEMBLES
c
        nensin=nens
        if(expt.eq.'Emm01')nmod=7
        if(expt.eq.'Emm11')nmod=5
        if(expt.eq.'Epp01')then
          nmod=9
          nensin=1
        endif
      else
        full_field=0
      endif
      allocate (cli_mod(nxny,nmon,1,nmod),                   stat=istat)
c
      do idat=1,ndat
c
c.... read climate if necessary
c
         if(full_field.eq.1)then
           if(idat.eq.1)then
             do imod=1,nmod
                expti=expt
                if(expt.eq.'Emm01')then
                  if(imod.eq.1)expti='CdmOecmfEscwfS000M001'
                  if(imod.eq.2)expti='CdmOegrrEukmoS000M001'
                  if(imod.eq.3)expti='CdmOlfpwEcnrmS000M001'
                  if(imod.eq.4)expti='CdmOlodyElodyS000M001'
                  if(imod.eq.5)expti='CdmOingvEscnrS000M001'
                  if(imod.eq.6)expti='CdmOsmpiEsmpiS000M001'
                  if(imod.eq.7)expti='CdmOcrfcEcrfcS000M001'
                endif
                if(expt.eq.'Emm11')then
                  if(imod.eq.1)expti='CenOecmfE1004S001M001'
                  if(imod.eq.2)expti='CenOegrrE1001S001M001'
                  if(imod.eq.3)expti='CenOlfpwE1001S000M001'
                  if(imod.eq.4)expti='CenOifmkE1000S001M010'
                  if(imod.eq.5)expti='CenOegrrE1503S051M010'
                endif
                if(expt.eq.'Epp01')then
                  if(imod.eq.1)expti='CenOegrrE1503S051M010'
                  if(imod.eq.2)expti='CenOegrrE1504S051M011'
                  if(imod.eq.3)expti='CenOegrrE1504S051M012'
                  if(imod.eq.4)expti='CenOegrrE1504S051M013'
                  if(imod.eq.5)expti='CenOegrrE1504S051M014'
                  if(imod.eq.6)expti='CenOegrrE1504S051M015'
                  if(imod.eq.7)expti='CenOegrrE1504S051M016'
                  if(imod.eq.8)expti='CenOegrrE1504S051M017'
                  if(imod.eq.9)expti='CenOegrrE1504S051M018'
                endif
                exptl=lena(expti)
                write(form1,'(a,i2.2,a)')'(a',exptl,')'
                yifile='CLIM_EXPT_YYYY-YYYY_MMDDTT_PAR_LEV'
                write(yifile(       6: 5+exptl),form1)expti(1:exptl)
                write(yifile( 6+exptl: 6+exptl),'(a1)')'_'
                write(yifile( 7+exptl:10+exptl),'(i4.4)')iyy1
                write(yifile(11+exptl:11+exptl),'(a1)')'-'
                write(yifile(12+exptl:15+exptl),'(i4.4)')iyy2
                write(yifile(16+exptl:16+exptl),'(a1)')'_'
                write(yifile(17+exptl:18+exptl),'(i2.2)')imm
                write(yifile(19+exptl:20+exptl),'(i2.2)')idd
                write(yifile(21+exptl:22+exptl),'(i2.2)')itt
                write(yifile(23+exptl:23+exptl),'(a1)')'_'
                write(yifile(24+exptl:26+exptl),'(i3.3)')ipar
                write(yifile(27+exptl:27+exptl),'(a1)')'_'
                write(yifile(28+exptl:30+exptl),'(i3.3)')ilev
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
                idatetype=0
                call readgrb(iunit, nxny, nmon, 1, idat,
     >             imm, iyy1, 1, ipar, ilev, idatetype,
     >             cli_mod(1,1,1,imod), ivfdate_c, istep_c,
     >             ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
                do imon=1,nmon
                   write(*,*)ivfdate_c(imon),istep_c(imon)
                enddo
                call pbclose(iunit,kret)
                if(kret.ne.0)then
                  write(*,*)'Error in closing file: kret=',kret
                  goto 997
                endif
             enddo
           endif
           if(idat.eq.2)then
             expti = 'refe'
             exptl=lena(expti)
             write(form1,'(a,i2.2,a)')'(a',exptl,')'
             yifile='CLIM_EXPT_YYYY-YYYY_MMDDTT_PAR_LEV'
             write(yifile(       6: 5+exptl),form1)expti(1:exptl)
             write(yifile( 6+exptl: 6+exptl),'(a1)')'_'
             write(yifile( 7+exptl:10+exptl),'(i4.4)')iyy1
             write(yifile(11+exptl:11+exptl),'(a1)')'-'
             write(yifile(12+exptl:15+exptl),'(i4.4)')iyy2
             write(yifile(16+exptl:16+exptl),'(a1)')'_'
             write(yifile(17+exptl:18+exptl),'(i2.2)')imm
             write(yifile(19+exptl:20+exptl),'(i2.2)')idd
             write(yifile(21+exptl:22+exptl),'(i2.2)')itt
             write(yifile(23+exptl:23+exptl),'(a1)')'_'
             write(yifile(24+exptl:26+exptl),'(i3.3)')ipar
             write(yifile(27+exptl:27+exptl),'(a1)')'_'
             write(yifile(28+exptl:30+exptl),'(i3.3)')ilev
             write(*,*)'open input file: ',yifile
             kret=0
             call pbopen(iunit,yifile,'r',kret)
             if(kret.ne.0)then
               write(*,*)'Error in opening file: kret=',kret
               goto 997
             endif
             idatetype=2
             call readgrb(iunit, nxny, nmon, 1, idat,
     >          imm, iyy1, 1, ipar, ilev, idatetype,
     >          cli_ref, ivfdate_c, istep_c,
     >          ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
             do imon=1,nmon
                write(*,*)ivfdate_c(imon),istep_c(imon)
             enddo
             call pbclose(iunit,kret)
             if(kret.ne.0)then
               write(*,*)'Error in closing file: kret=',kret
               goto 997
             endif
           endif
         endif
c
c.... read anomalies
c
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
c
c.... add climate if necessary
c
              if(full_field.eq.1)then
                do imon=1,nmon
                   do imod=1,nmod
                      do iens=1,nensin
                         iiens=iens+(imod-1)*nensin
c                        write(*,*)imon,imod,iens
                         do i=1,nxny
                            ano_mod(i,imon,iiens,iy)=
     >                         ano_mod(i,imon,iiens,iy)+
     >                         cli_mod(i,imon,1,imod)
                         enddo
                      enddo
                   enddo
                enddo
              endif
            endif
            if(idat.eq.2)then
              idatetype=2
              call readgrb(iunit, nxny, nmon, 1, idat,
     >            imm, iyy1, iy, ipar, ilev, idatetype,
     >            ano_ref(1,1,1,iy), ivfdate(1,iy), istep(1,iy),
     >            ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
              if(full_field.eq.1)then
                do i=1,nxny
                   do imon=1,nmon
                      ano_ref(i,imon,1,iy)=
     >                   ano_ref(i,imon,1,iy)+cli_ref(i,imon,1)
                      do iens=1,nens
                      enddo
                   enddo
                enddo
              endif
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
c.... length of 1d-data vector at every gridpoint: inmod = nyears*nensa(for model data)
c.... length of 1d-data vector at every gridpoint: inref = nyears      (for ref   data)
c
      allocate (arrr(nyear),                                 stat=istat)
      allocate (arrh(nyear*nens),                            stat=istat)
      do i=1,nxny
c        write(*,*)'Working with point ',i
         inmod=0
         inref=0
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
               inmod = inmod+1
               if(sum.ne.rmiss)then
                 dat_mod(i,iy,iens)=sum/float(nummon)
               else
                 dat_mod(i,iy,iens)=rmiss
               endif
               arrh(inmod)=dat_mod(i,iy,iens)
            enddo !iens
c
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
              inref = inref+1
              if(sum.ne.rmiss)then
                dat_ref(i,iy)=sum/float(nummon)
              else
                dat_ref(i,iy)=rmiss
              endif
              arrr(inref)=dat_ref(i,iy)
            endif
         enddo !iy
c        write(*,*)'Point ',i,'; Size of arrays ',inref,inmod
c        write(*,*)(arrr(iy),iy=1,inref)
c        write(*,*)(arrh(iy),iy=1,inmod)
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
         if(ndat.eq.2)then
           maskr(i)=0
           do iy=1,nyear
              if(dat_ref(i,iy).eq.rmiss)maskr(i)=1
           enddo
         endif
c
c.... compute thresholds for binary categories
c
         allocate (tr(ncla-1),                               stat=istat)
c        write(*,*)'Compute percentiles'
         if(ndat.eq.2)then
           call percen(arrr,inref,inref,tr,ncla,permeth)
           do icla=1,ncla-1
              if(maskr(i).eq.0)then
                thresr(i,icla)=tr(icla)
              else
                thresr(i,icla)=rmiss
              endif
           enddo
         endif
         call percen(arrh,inmod,inmod,tr,ncla,permeth)
         do icla=1,ncla-1
            if(maskh(i).eq.0)then
              thresh(i,icla)=tr(icla)
            else
              thresh(i,icla)=rmiss
            endif
         enddo
         deallocate (tr,                                     stat=istat)
      enddo !i
      deallocate (arrh,                                      stat=istat)
      deallocate (arrr,                                      stat=istat)
c
c.... open output file
c
      do idat=1,ndat
         expti=expt
         if(idat.eq.2)then
            expti = 'refe'
         endif
         exptl=lena(expti)
         write(form1,'(a,i2.2,a)')'(a',exptl,')'
         yofile=
     >'PERC_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_TYP_PP_AAA-EEE'
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
         write(yofile(40+exptl:42+exptl),'(a3)')type
         write(yofile(43+exptl:44+exptl),'(a2)')'_M'
         write(yofile(45+exptl:45+exptl),'(i1.1)')permeth
         write(yofile(46+exptl:46+exptl),'(a1)')'_'
         write(yofile(47+exptl:49+exptl),'(i3.3)')nf1
         write(yofile(50+exptl:50+exptl),'(a1)')'-'
         write(yofile(51+exptl:53+exptl),'(i3.3)')nf2
         yofile=yofile(1:lena(yofile))//'.grb'
         write(*,*)'open output file: ',yofile(1:lena(yofile))
         kret=0
         call pbopen(ounit1,yofile,'w',kret)
         if(kret.ne.0)then
           write(*,*)'Error in opening file: kret=',kret
           goto 997
         endif
c
c.... write percentiles
c
         do ik=1,1024
            ksec1(ik)=ksec1_sav(ik)
         enddo !ik loop
         write(*,*)'Write percentiles'
         do icla=1,ncla-1
            ksec1(42)=icla
            do i=1,nxny
               if(idat.eq.1)psec4(i)=thresh(i,icla)
               if(idat.eq.2)psec4(i)=thresr(i,icla)
            enddo
            call writegrb(ounit1, nxny, psec4,
     >         ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
         enddo
         kret=1
         call pbclose(ounit1,kret)
         if(kret.lt.0)then
           write(*,*)'Error in closing file: kret=',kret
           goto 997
         endif
      enddo !idat loop
c
c.... write pdf for single points
c
      allocate (arrr(nyear),                                 stat=istat)
      allocate (arrh(nyear*nens),                            stat=istat)
      allocate (xr(nxny,nbin),                               stat=istat)
      allocate (xh(nxny,nbin),                               stat=istat)
      allocate (yr(nxny,nbin),                               stat=istat)
      allocate (yh(nxny,nbin),                               stat=istat)
      allocate (pdfr(nbin),                                  stat=istat)
      allocate (pdfh(nbin),                                  stat=istat)
      do i=1,nxny
c        write(*,*)'Working with point ',i
         inmod=0
         inref=0
         do iy=1,nyear
            do iens=1,nens
               inmod=inmod+1
               arrh(inmod)=dat_mod(i,iy,iens)
            enddo
            if(ndat.eq.2)then
              inref=inref+1
              arrr(inref)=dat_ref(i,iy)
            endif
         enddo
c
         call moment(arrh, inmod, ave, adev, sdev, var, skew, curt)
c        write(*,*)ave,sdev
         call kergauss(arrh, inmod, inmod, nbin, x0, xint, sigma,
     >      ave, sdev, pdfh)
c        write(*,*)ave,sdev
         sum=0.
         do ibin=1,nbin
            xh(i,ibin)=ave+sdev*(x0+(ibin-1)*xint)
            sum=sum+sdev*xint*pdfh(ibin)
         enddo
c        write(*,*)'sum=',sum
         do ibin=1,nbin
            if(maskh(i).eq.0)then
              yh(i,ibin)=pdfh(ibin)/sum
            else
              xh(i,ibin)=rmiss
              yh(i,ibin)=rmiss
            endif
c           write(*,'(4f12.4)')xr(i,ibin),xh(i,ibin),
c    >         yr(i,ibin),yh(i,ibin)
         enddo
         if(ndat.eq.2)then
           call moment(arrr, inref, ave, adev, sdev, var, skew, curt)
c          write(*,*)ave,sdev
           call kergauss(arrr, inref, inref, nbin, x0, xint, sigma,
     >        ave, sdev, pdfr)
c          write(*,*)ave,sdev
           sum=0.
           do ibin=1,nbin
              xr(i,ibin)=ave+sdev*(x0+(ibin-1)*xint)
              sum=sum+sdev*xint*pdfr(ibin)
           enddo
c          write(*,*)'sum=',sum
           do ibin=1,nbin
              if(maskr(i).eq.0)then
                yr(i,ibin)=pdfr(ibin)/sum
              else
                xr(i,ibin)=rmiss
                yr(i,ibin)=rmiss
              endif
           enddo
         endif
      enddo
      deallocate (arrh,                                      stat=istat)
      deallocate (arrr,                                      stat=istat)
      deallocate (pdfr,                                      stat=istat)
      deallocate (pdfh,                                      stat=istat)
c
c.... open output file
c
      do idat=1,ndat
         expti=expt
         if(idat.eq.2)then
            expti = 'refe'
         endif
         exptl=lena(expti)
         write(form1,'(a,i2.2,a)')'(a',exptl,')'
         yofile=
     >'PDFC_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_TYP_SIGMA_AAA-EEE'
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
         write(yofile(40+exptl:42+exptl),'(a3)')type
         write(yofile(43+exptl:44+exptl),'(a2)')'_S'
         write(yofile(45+exptl:48+exptl),'(f4.2)')sigma
         write(yofile(49+exptl:49+exptl),'(a1)')'_'
         write(yofile(50+exptl:52+exptl),'(i3.3)')nf1
         write(yofile(53+exptl:53+exptl),'(a1)')'-'
         write(yofile(54+exptl:56+exptl),'(i3.3)')nf2
         yofile=yofile(1:lena(yofile))//'.grb'
         write(*,*)'open output file: ',yofile(1:lena(yofile))
         kret=0
         call pbopen(ounit1,yofile,'w',kret)
         if(kret.ne.0)then
           write(*,*)'Error in opening file: kret=',kret
           goto 997
         endif
c
c.... write pdfs
c
         do ik=1,1024
            ksec1(ik)=ksec1_sav(ik)
         enddo !ik loop
         write(*,*)'Write PDFs'
         write(*,*)'It first writes the x value with ensemble member ',
     >      'number i and then the pdf value for that x value with ',
     >      'ensemble member number i+1'
         do ibin=1,nbin
            write(*,*)'Writing PDF for bin ',ibin
            if(idat.eq.2)then
              ksec1(42)=ibin*2-1
              do i=1,nxny
                 psec4(i)=xr(i,ibin)
              enddo
              write(*,*)'Writing x axis for reference'
              call writegrb(ounit1, nxny, psec4,
     >           ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
              ksec1(42)=ibin*2
              do i=1,nxny
                 psec4(i)=yr(i,ibin)
              enddo
              write(*,*)'Writing y axis for reference'
              call writegrb(ounit1, nxny, psec4,
     >           ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
            endif
            if(idat.eq.1)then
              ksec1(42)=ibin*2-1
              do i=1,nxny
                 psec4(i)=xh(i,ibin)
              enddo
              write(*,*)'Writing x axis for experiment'
              call writegrb(ounit1, nxny, psec4,
     >           ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
              ksec1(42)=ibin*2
              do i=1,nxny
                 psec4(i)=yh(i,ibin)
              enddo
              write(*,*)'Writing y axis for experiment'
              call writegrb(ounit1, nxny, psec4,
     >           ksec0, ksec1, ksec2, ksec3, ksec4, psec2, psec3)
            endif
         enddo
         kret=1
         call pbclose(ounit1,kret)
         if(kret.lt.0)then
           write(*,*)'Error in closing file: kret=',kret
           goto 997
         endif
      enddo !idat loop
c
      deallocate (xr,                                        stat=istat)
      deallocate (xh,                                        stat=istat)
      deallocate (yr,                                        stat=istat)
      deallocate (yh,                                        stat=istat)
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
