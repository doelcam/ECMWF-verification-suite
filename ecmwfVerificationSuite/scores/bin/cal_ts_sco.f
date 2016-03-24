c
c----------------------------------------------------------------------c
c     PROGRAM cal_ts_sco                  P. Doblas-Reyes 04-Apr-2008  c
c                                                                      c
c     PURPOSE:                                                         c
c     Computes scores for and inflates time series                     c
c                                                                      c
c     INPUT:                                                           c
c     TS_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE_NAME            c
c                                                       ASCII files    c
c                                                                      c
c     OUTPUT:                                                          c
c     TS_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE_NAME            c
c                                                       ASCII files    c
c                                                                      c
c     USAGE:                                                           c
c     cal_ts_sco.x < nlist                                             c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 cal_ts_sco.f tools.f -o cal_ts_sco.x               c
c                          $EMOSLIB $NAGLIB                            c
c                                                                      c
c     MODS:                                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      PROGRAM cal_ts_sco
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: r(:), h(:,:), hm(:)
c
c.... hard coded field definitions
c
      integer nts_max
      parameter(nts_max=80)
      character*6 namet(nts_max)
      integer par(nts_max), lev(nts_max)
      integer iyys(nts_max), iyye(nts_max)
      integer ncla
      parameter(ncla=3)
      real tr(ncla-1), th(ncla-1), tru(ncla-1), thu(ncla-1)
      real tri(ncla-1), thi(ncla-1)
      real eps
      parameter(eps=0.1)
c
c.... other definitions
c
      real rms, rmsi, varmh, varn, ratio, rpss, rpssp, rpssd, rpssdp
      real cor, sig, cori, sigi, sigr, sigri, varni
      real varr, varh, rpssu, rpsspu, rpssdu, rpssdpu
      real snr, snri
      integer nens, nf1, nf2, imm, idd, itt, cros, anin, nts
      integer ipar, ilev, iyy1, iyy2
      integer iunit1, iunit2, exptl, lena, istat
      integer i, it, nyear, iy, idat, iens
      integer permeth, icla
      character*21 expt, expti
      character*100 yfile, yifile, yofile
      character*20 form1
c
      namelist /control/ nens,  imm,  idd,  itt,
     >                   cros, anin,  nf1,  nf2,
     >                   expt,  nts
      namelist /timeseries/
     >                  namet,  par,  lev,  iyys, iyye
c
c.... set default values and read input namelist
c
      nens = 9
      imm  = 11
      idd  = 1
      itt  = 0
      cros = 1
      anin = 0
      expt = 'scwc'
c
c.... Check the order of the parameters for the indices
c
c   nens:   number of ensemble members
c   iyy1:   start year
c   iyy2:   end year
c   imm:    forecast starting month
c   expt:   model id
c
      read(5,control)
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
      read(5,timeseries)
      write(*,*)'namet: ',(namet(i),i=1,nts)
      write(*,*)' par: ',(par(i),i=1,nts)
      write(*,*)' lev: ',(lev(i),i=1,nts)
      write(*,*)'iyys: ',(iyys(i),i=1,nts)
      write(*,*)'iyye: ',(iyye(i),i=1,nts)
c
      exptl=lena(expt)
c
c.... loop over time series to plot
c
      do it=1,nts
         iyy1=iyys(it)
         iyy2=iyye(it)
         ipar=par(it)
         ilev=lev(it)
         nyear=iyy2-iyy1+1
c
c.... allocate fields
c
         allocate (r(nyear),                                 stat=istat)
         allocate (h(nyear,nens),                            stat=istat)
         allocate (hm(nyear),                                stat=istat)
c
c.... open input file
c
         do idat=1,2
            expti=expt
            if(idat.eq.1)then
              expti = 'refe'
            endif
            exptl=lena(expti)
            write(form1,'(a,i2.2,a)')'(a',exptl,')'
            yifile=
     >        'TS_EXPT_YYYY-YYYY_MMDDTT_CVYY_II_PAR_LEV_AAA-EEE'
            write(yifile(       4: 3+exptl),form1)expti(1:exptl)
            write(yifile( 4+exptl: 4+exptl),'(a1)')'_'
            write(yifile( 5+exptl: 8+exptl),'(i4.4)')iyy1
            write(yifile( 9+exptl: 9+exptl),'(a1)')'-'
            write(yifile(10+exptl:13+exptl),'(i4.4)')iyy2
            write(yifile(14+exptl:14+exptl),'(a1)')'_'
            write(yifile(15+exptl:16+exptl),'(i2.2)')imm
            write(yifile(17+exptl:18+exptl),'(i2.2)')idd
            write(yifile(19+exptl:20+exptl),'(i2.2)')itt
            write(yifile(21+exptl:23+exptl),'(a3)')'_CV'
            write(yifile(24+exptl:25+exptl),'(i2.2)')cros
            write(yifile(26+exptl:27+exptl),'(a2)')'_I'
            if(idat.eq.2)then
              write(yifile(28+exptl:28+exptl),'(i1.1)')0
            else
              write(yifile(28+exptl:28+exptl),'(i1.1)')anin
            endif
            write(yifile(29+exptl:29+exptl),'(a1)')'_'
            write(yifile(30+exptl:32+exptl),'(i3.3)')ipar
            write(yifile(33+exptl:33+exptl),'(a1)')'_'
            write(yifile(34+exptl:36+exptl),'(i3.3)')ilev
            write(yifile(37+exptl:37+exptl),'(a1)')'_'
            write(yifile(38+exptl:40+exptl),'(i3.3)')nf1
            write(yifile(41+exptl:41+exptl),'(a1)')'-'
            write(yifile(42+exptl:44+exptl),'(i3.3)')nf2
            yifile=yifile(1:lena(yifile))//'_'//namet(it)
            write(*,*)'open input file: ',yifile(1:lena(yifile))
            open(10,file=yifile,form='formatted',status='old')
c
c.... reading data
c
            if(idat.eq.1)then
              write(form1,'(a,i1.1,a)')'(4x,e12.4)'
              write(*,*)form1
              do iy=1,nyear
                 read(10,form1)r(iy)
              enddo
            else
              write(form1,'(a,i2.2,a)')'(4x,',nens+1,'e12.3)'
              if(nens.ge.99)write(form1,'(a,i3.3,a)')
     >           '(4x,',nens+1,'e12.3)'
              write(*,*)form1
              do iy=1,nyear
                 read(10,form1)(h(iy,iens),iens=1,nens),hm(iy)
              enddo
            endif
            close(10)
         enddo !idat
c
c.... compute variances, correlations, and RMSE
c.... hindcast inflation is performed in rmse routine unless 
c.... stated otherwise
c.... a first probabilistic score computation is performed to
c.... obtain the values for the uninflated ensemble
c
         permeth=0
         call probsco(r,h,nyear,nyear,nens,nens,ncla,
     >      permeth,rpssu,rpsspu,rpssdu,rpssdpu,tr,th)
         write(*,*)'Original thresholds'
         do icla=1,ncla-1
            tru(icla)=tr(icla)
            thu(icla)=th(icla)
            write(*,*)tru(icla),thu(icla)
         enddo
         call rmse(r,h,nyear,nyear,nens,nens,
     >      varr,varh,varmh,varn,varni,ratio,
     >      rms,rmsi,cor,sig,cori,sigi,
     >      snr,sigr,snri,sigri)
         varn=varn/rms
         varni=varni/rms
         call probsco(r,h,nyear,nyear,nens,nens,ncla,
     >      permeth,rpss,rpssp,rpssd,rpssdp,tr,th)
            write(*,*)'Inflated thresholds'
         do icla=1,ncla-1
            tri(icla)=tr(icla)
            thi(icla)=th(icla)
            write(*,*)tri(icla),thi(icla)
         enddo
c
c.... output
c
         write(*,*)'open output file: ',yifile(1:lena(yifile))
         open(10,file=yifile,form='formatted',status='old',
     >      position='append')
         write(*,*)' Output '
         write(*,*)
     >      ' Vars, ratio, RMSEs, cor (sig), SNR, RPSS, RPSSd '
         write(*,'(22f12.4)')varr,varh,varmh,varn,varni,ratio,
     >      rms,rmsi,cor,sig,cori,sigi,snr,snri,
     >      rpssu,rpsspu,rpss,rpssp,
     >      rpssdu,rpssdpu,rpssd,rpssdp
c
c.... ratio:   ratio of standard deviations (model vs reference)
c.... varn:    spread/RMSE ratio
c.... varni:   spread/RMSE ratio of inflated values
c.... rms:     root mean square error
c.... rmsi:    root mean square error of the inflated values
c.... cor:     ensemble mean correlation
c.... sig:     statistical significance of the correlation
c.... snr:     signal-to-noise ratio
c.... sigr:    signal-to-noise ratio statistical confidence
c.... snri:    signal-to-noise ratio of the inflated values
c.... sigri:   signal-to-noise ratio statistical confidence for
c....               the inflated values
c.... rpssu:   RPSS of the original values
c.... rpsspu:  RPSS statistical significance of the original values
c.... rpss:    RPSS of the inflated values
c.... rpssp:   RPSS statistical significance of the inflated values
c.... rpssdu:  RPSSd of the original values
c.... rpssdpu: RPSSd statistical significance of the original values
c.... rpssd:   RPSSd of the inflated values
c.... rpssdp:  RPSSd statistical significance of the inflated values
c
         write(10,'(f12.2,21e12.4)')
     >      ratio,varn,varni,rms,rmsi,cor,sig,cori,sigi,
     >      snr,sigr,snri,sigri,
     >      rpssu,rpsspu,rpss,rpssp,rpssdu,rpssdpu,rpssd,rpssdp
         write(form1,'(a,i1.1,a)')'(',3*(ncla-1),'e12.4)'
c         write(10,form1)((tru(icla),thu(icla)),icla=1,ncla-1),
c     >      (thi(icla),icla=1,ncla-1)
         write(10,form1)(tru(icla),thu(icla),icla=1,ncla-1),
     >      (thi(icla),icla=1,ncla-1)
c DM fixed error here - too many pararentheses


         write(*,*)' Then yearly (eventually inflated) values '
         do iy=1,nyear
            hm(iy)=0.
            do iens=1,nens
               hm(iy)=hm(iy)+h(iy,iens)/nens
            enddo
            write(form1,'(a,i2.2,a)')'(i4,',nens+1,'e12.3)'
            if(nens.ge.99)write(form1,'(a,i3.3,a)')
     >         '(i4,',nens+1,'e12.3)'
            write(10,form1)iy+iyy1-1,
     >         (h(iy,iens),iens=1,nens),
c    >         (hm(iy)*varr/varmh)
     >         hm(iy)
         enddo
c
         deallocate (r,                                      stat=istat)
         deallocate (h,                                      stat=istat)
         deallocate (hm,                                     stat=istat)
c
      enddo
c
      write(*,*)'Program seems to be successfully finished :)'
      continue
c
      end
