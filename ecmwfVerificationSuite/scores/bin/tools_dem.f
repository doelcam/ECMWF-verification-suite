
c----------------------------------------------------------------------c
c                                                                      c
c     COLLECTION OF SUBROUTINES FOR AUTOMATED VERIFICATION SYSTEM      c
c                                                                      c
c----------------------------------------------------------------------c
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
      SUBROUTINE get_mask(iilsm,lsm,nxny,expt)
c
      integer iilsm
      integer lsm(nxny)
      character*4 expt
      character*8 ylsmfile
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
        ylsmfile='LSM_EXPT'
        write(ylsmfile(5:8),'(a4)')expt
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
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE get_region(icatch,rlats,rlatn,rlonw,rlone,
     >                      nxny,expt,ybasedisk,regwgt,latwgt)
c
      implicit none
c
      real    rlats, rlatn, rlonw, rlone
      integer ilats, ilatn, ilonw, ilone
      integer ilonw1, ilonw2, ilone1, ilone2
      integer nxny, nlon, nlat, nlon_new, nlat_new, lat, lon
      integer icatch
      integer i, i2reg, ilsmunit, icunit, lena
      real    pi, regwgt(nxny), latwgt(nxny)
      real xfac, zlatn, zlonw, zdlat, zdlon
      real rlone1, rlone2, rlonw1, rlonw2, rlatdeg, rlatbog
      character*4 expt
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
      ylsmfile='LSM_EXPT'
      write(ylsmfile(5:8),'(a4)')expt
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
c.... in case that area lies over the Greenwich 0 deg line divide into 2 areas
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
c.... in case that area doesn't lie over the Greenwich 0 deg line, we have only 1 area
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
c                write(*,*)rlatbog, rlatdeg, latwgt(i), regwgt(i)
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
         ylsmfile=ybasedisk(1:lena(ybasedisk))//
     >'data/const/'//ylsmfile(1:17)
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
c     SUBROUTINE ann_cycle                P. Doblas-Reyes, 15-Nov-2001 c
c                                                                      c
c     PURPOSE:                                                         c
c     removes the mean annual cycle from a set of data x               c
c                                                                      c
c     INPUT:                                                           c
c     x   : data (monthly basis)                                       c
c     date: corresponding dates                                        c
c     nxny: dimension for number of points                             c
c     ny  : dimension for number of years                              c
c     nmon: dimension for number of months                             c
c     nens: dimension for ensemble members                             c
c     ip  : number of points                                           c
c     iy  : number of years                                            c
c     imon: number of months                                           c
c     iens: number of ensemble members                                 c
c                                                                      c
c     OUTPUT:                                                          c
c     x   : data without the annual cycle                              c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE ann_cycle(x,date,nxny,ny,nmon,nens,ip,iy,imon,iens)
c
      real x(nxny,ny,nmon,nens)
      real zmed(ip,12)
      integer date(ny,nmon),nmed(12)
c
      do jm=1,12
         nmed(jm)=0
         do jp=1,ip
            zmed(jp,jm)=0.
         enddo
      enddo
c
      do jy=1,iy
         do jm=1,imon
            i1=date(jy,jm)
c           i1=date(jy,jm)-int(date(jy,jm)/100.)*100
            write(*,*)'month ',i1,' for date ',date(jy,jm)
            do je=1,iens
               nmed(i1)=nmed(i1)+1
               do jp=1,ip
                  zmed(jp,i1)=zmed(jp,i1)+x(jp,jy,jm,je)
               enddo
            enddo
         enddo
      enddo
c
      do jm=1,12
         write(*,*)'Number of elements for month ',jm,'=',nmed(jm)
      enddo
c
      do jy=1,iy
         do jm=1,imon
            i1=date(jy,jm)
            do je=1,iens
               do jp=1,ip
                  x(jp,jy,jm,je)=x(jp,jy,jm,je)-zmed(jp,i1)/nmed(i1)
               enddo
            enddo
         enddo
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
c     x   : time series containing the data                            c
c     ndim: dimension for the number of data                           c
c     n   : number of data                                             c
c     ncla: number of classes                                          c
c                                                                      c
c     OUTPUT:                                                          c
c     t   : percentiles (to be used as classes thresholds)             c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE percen(x,ndim,n,t,ncla)
c
c.... The parameter constants are:
c.... nx:    number of bins used
c.... x0:    first (normalized) central value for the bins
c.... sigma: smoothness parameter
c.... xint:  bin width
c
c.... permeth: method used to compute the percentiles as
c....          described in the routine header with 1 for
c....          kernel method and 0 for counting
c
      integer permeth
      parameter(permeth=0)
      parameter(nx=101,x0=-5.,xint=((-2.*x0)/nx),sigma=0.25)
c
      real x(ndim)
      real pdf(nx),cum(nx),y(n)
      real t(ncla-1),t1(ncla-1),t2(ncla-1)
      integer tag(ncla-1),nr(n)
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
c     write(*,*)(x(i),i=1,n),ndim,n
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
      epsilon=tiny(epsilon)
c
c.... the data are ranked in increasing order
c
      do i=1,n
         nr(i)=1
         do ii=1,n
            dif=x(i)-x(ii)
            dif=dif-epsilon
            dif=sign(1.,dif)
            dif=max(dif,0.)
            nr(i)=nr(i)+int(dif)
         enddo
      enddo
      do i=1,n
         do ii=i+1,n
            if(nr(i).eq.nr(ii))nr(ii)=nr(ii)+1
         enddo
      enddo
      do i=1,n
         y(nr(i))=x(i)
      enddo
c     write(*,*)((i,x(i)),i=1,n)
c     write(*,*)((i,y(i)),i=1,n)
c
c.... computing percentiles
c
      do jcla=1,ncla-1
         ii=(jcla*(n+1))/ncla
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
c     SUBROUTINE gauss                    P. Doblas-Reyes, 26-Apr-2002 c
c                                                                      c
c     PURPOSE:                                                         c
c     similar to kergauss, but uses a variable sigma to avoid the      c
c        spread of probability near the tail distribution              c
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
      SUBROUTINE gauss(x,ndim,n,nx,x0,xint,sigma,xm,sd,pdf)
c     parameter (nth=10)
c
      real x(ndim)
      real pdf(nx)
c
      eps=tiny(eps)
      nth=int(nx/10.)
      write(*,*)'nth=',nth
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
      if(sd.eq.0.)write(*,*)'Beware, constant values'
c
c.... Gaussian factors
c
      a=1./(2*n*sqrt(asin(1.))*sigma)
c     a=1./(2*n*sqrt(asin(1.))*sd*sigma)
c
c.... xx is the central value of the bin
c.... xn is the normalized data value
c.... pdf is the normalized PDF
c
      xx=x0
      do jx=1,nx
         pdf(jx)=0.
         if(jx.le.nth)then
           sig=sigma**(nth-jx+1)
         elseif(jx.gt.(nx-nth))then
           sig=sigma**(jx-nx+nth)
         else
           sig=sigma
         endif
         b=-1./(2*(sig)**2)
c        b=-1./(2*(sig*sd)**2)
         do i=1,n
            xn=(x(i)-xm)/(sd+eps)
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
      real r(ny),h(ny,nens)
      real tr(ncla-1),th(ncla-1)
      real ph(ncla),pr(ncla)
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
c     SUBROUTINE hrfr                          R.Hagedorn, 09-Jan-2002 c
c                                                                      c
c     PURPOSE:                                                         c
c     calculates contingency table as well as                          c
c     hit rate and false alarm rate                                    c
c                                                                      c
c     INPUT:                                                           c
c     nxny   : grid point dimension                                    c
c     nmod   : number of models involved                               c
c     iprb   : switch for number of prb'bins in case of multi-model    c
c     nyear  : year dimension                                          c
c     nens   : number of ensemble members                              c
c     nprob  : number of probability thresholds                        c
c     regwgt: weight of the grid point (0. to 1. depending on what     c
c             part of the grid box lays inside the region              c
c     latwgt: weight of the grid point (0. to 1. depending on          c
c             the latitude of the grid point (cos theta)               c
c     lsm    : land sea mask                                           c
c     narea  : switch for local(=nxny) or regional averaged (=1) calc. c
c     dat_mod: model data                                              c
c     dat_era: era data                                                c
c     thr_mod: threshold at grid point level for model data            c
c     thr_era: threshold at grid point level for era data              c
c                                                                      c
c     OUTPUT:                                                          c
c     hr:      hit rate                                                c
c     hrsup:   upper bound for the hit rate score confidence interval  c
c     hrlow:   lower bound for the hit rate score confidence interval  c
c     fr:      false alarm rate                                        c
c     yhr:     sum (over all prob'bins) of all hr'events               c
c     yfr:     sum (over all prob'bins) of all fr'events               c
c     obs_occ: number of obs-occuring events in certain prob bin       c
c     obs_noc: number of obs-non-occuring events in certain prob bin   c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE hrfr(nxny, nmod, iprb, nyear, nens, nprob,
     >                regwgt, latwgt, lsm, narea, 
     >                dat_mod, dat_era, thr_mod, thr_era,
     >                obs_occ, obs_noc, hr, fr, hrsup, hrlow, yhr, yfr)
c
      implicit none
c
      integer nxny, nmod, iprb, nyear, nens, nprob
      integer lsm(nxny), iarea, tagwei
      integer num, ip, inew, iens, iprob, narea, iyear, i
      real obs_occ(nxny,nprob),  obs_noc(nxny,nprob) 
      real hr(nxny,nprob), fr(nxny,nprob)
      real hrsup(nxny,nprob), hrlow(nxny,nprob)
      real xhr(nxny), yhr(nxny), xfr(nxny), yfr(nxny)
      real dat_mod(nxny,nyear,nens), dat_era(nxny,nyear)
      real regwgt(nxny), latwgt(nxny), thr_mod(nxny), thr_era(nxny)
      real xhlp(nens), yhlp, eps, t_mod, t_era
      real zalpha2, zalpha22, xsick
c
c.... reset contingency table
c
      do iprob=1,nprob
      do iarea=1,narea
         obs_occ(iarea,iprob)=0.
         obs_noc(iarea,iprob)=0.
      enddo
      enddo
      eps = tiny(eps)
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
           do iyear=1,nyear
              yhlp=dat_era(i,iyear)
              do iens=1,nens
                 xhlp(iens)=dat_mod(i,iyear,iens)
              enddo !iens
c
c... check whether threshold is negativ or positiv
c
              if(t_era.lt.0)then
                if(yhlp.lt.t_era)then !event is observed
                  num=0
                  do iens=1,nens
                     if(xhlp(iens).lt.t_mod)num=num+1 ! ens'member forecasts event
                  enddo !iens
                  if(iprb.eq.1)then
                    iprob=1+num
                  else
                    iprob=1+num/nmod
                  endif
                  obs_occ(iarea,iprob)=obs_occ(iarea,iprob)
     >               +lsm(i)*(1.+tagwei*(latwgt(i)-1.)) !add obs_occ in corresp. prob'bin
                else !event is not observed
                  num=0
                  do iens=1,nens
                     if(xhlp(iens).lt.t_mod)num=num+1 ! ens'member forecasts event
                  enddo !iens
                  if(iprb.eq.1)then
                    iprob=1+num
                  else
                    iprob=1+num/nmod
                  endif
                  obs_noc(iarea,iprob)=obs_noc(iarea,iprob)
     >               +lsm(i)*(1.+tagwei*(latwgt(i)-1.)) !add obs_noc in corresp. prob'bin
                endif
              else
                if(yhlp.gt.t_era)then !event is observed
                  num=0
                  do iens=1,nens
                     if(xhlp(iens).gt.t_mod)num=num+1 ! ens'member forecasts event
                  enddo !iens
                  if(iprb.eq.1)then
                    iprob=1+num
                  else
                    iprob=1+num/nmod
                  endif
                  obs_occ(iarea,iprob)=obs_occ(iarea,iprob)
     >               +lsm(i)*(1.+tagwei*(latwgt(i)-1.)) !add obs_occ in corresp. prob'bin
                else !event is not observed
                  num=0
                  do iens=1,nens
                     if(xhlp(iens).gt.t_mod)num=num+1 ! ens'member forecasts event
                  enddo !iens
                  if(iprb.eq.1)then
                    iprob=1+num
                  else
                    iprob=1+num/nmod
                  endif
                  obs_noc(iarea,iprob)=obs_noc(iarea,iprob)
     >               +lsm(i)*(1.+tagwei*(latwgt(i)-1.)) !add obs_noc in corresp. prob'bin
                endif
              endif
           enddo !iyear
         endif !regwgt(i).gt.0.
      enddo !i
c
c.... contingency table finished, now calculate HR and FR
c
      do i=1,narea
         yhr(i)=0.
         yfr(i)=0.
         do ip=1,nprob
            yhr(i)=yhr(i)+obs_occ(i,ip)
            yfr(i)=yfr(i)+obs_noc(i,ip)
         enddo
         do iprob=1,nprob
            xhr(i)=0.
            xfr(i)=0.
            do ip=iprob,nprob
               xhr(i)=xhr(i)+obs_occ(i,ip)
               xfr(i)=xfr(i)+obs_noc(i,ip)
            enddo
            inew=nprob-iprob+1
            hr(i,inew)=xhr(i)/(yhr(i)+eps)
            fr(i,inew)=xfr(i)/(yfr(i)+eps)
            if(yhr(i).eq.0.)then
              hrsup(i,inew)=0.
              hrlow(i,inew)=0.
            else
              xsick=zalpha2*sqrt( (hr(i,inew)*(1-hr(i,inew)) 
     >             + zalpha22/(4*yhr(i))) / yhr(i))
              hrsup(i,inew)=(hr(i,inew)+zalpha22/(2*yhr(i))+xsick)/
     >                      (1+zalpha22/yhr(i))
              hrlow(i,inew)=(hr(i,inew)+zalpha22/(2*yhr(i))-xsick)/
     >                      (1+zalpha22/yhr(i))
            endif
         enddo
c
c.... the test does not have statistical sense when the number of
c.... events is smaller than zalpha22
c
         if(yhr(i).lt.zalpha22)then
            do iprob=1,nprob
               hrlow(i,iprob)=0.  
               hrsup(i,iprob)=1. 
               if(hr(i,iprob).eq.0.) hrlow(i,iprob)=0.
            enddo
c        else
c           do iprob=1,nprob
c              if(hrlow(i,iprob).gt.hr(i,iprob))
c    >            write(*,*)'low',i,iprob,hrlow(i,iprob),hr(i,iprob),
c    >            hrsup(i,iprob),yhr(i)
c              if(hrsup(i,iprob).lt.hr(i,iprob))
c    >            write(*,*)'sup',i,iprob,hrlow(i,iprob),hr(i,iprob),
c    >            hrsup(i,iprob),yhr(i)
c           enddo
         endif 
            hr(i,1)=0.
         hrlow(i,1)=0.
         hrsup(i,1)=0.
            hr(i,nprob)=1.
         hrlow(i,nprob)=1.
         hrsup(i,nprob)=1.
            fr(i,1)=0.
            fr(i,nprob)=1.
      enddo !i
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE hrfr_de                  P. Doblas-Reyes, 15-May-2003 c
c                                                                      c
c     PURPOSE:                                                         c
c     calculates contingency table as well as                          c
c     hit rate and false alarm rate for the decomposition of the RPSS  c
c                                                                      c
c     INPUT:                                                           c
c     nxny   : grid point dimension                                    c
c     nmod   : number of models involved                               c
c     iprb   : switch for number of prb'bins in case of multi-model    c
c     nyear  : year dimension                                          c
c     nens   : number of ensemble members                              c
c     nprob  : number of probability thresholds                        c
c     regwgt: weight of the grid point (0. to 1. depending on what     c
c             part of the grid box lays inside the region              c
c     latwgt: weight of the grid point (0. to 1. depending on          c
c             the latitude of the grid point (cos theta)               c
c     lsm    : land sea mask                                           c
c     narea  : switch for local(=nxny) or regional averaged (=1) calc. c
c     dat_mod: model data                                              c
c     dat_era: era data                                                c
c     thr_mod: threshold at grid point level for model data            c
c     thr_era: threshold at grid point level for era data              c
c                                                                      c
c     OUTPUT:                                                          c
c     hr:      hit rate                                                c
c     fr:      false alarm rate                                        c
c     yhr:     sum (over all prob'bins) of all hr'events               c
c     yfr:     sum (over all prob'bins) of all fr'events               c
c     obs_occ: number of obs-occuring events in certain prob bin       c
c     obs_noc: number of obs-non-occuring events in certain prob bin   c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE hrfr_de(nxny, nmod, iprb, nyear, nens, nprob,
     >                regwgt, latwgt, lsm, narea, 
     >                dat_mod, dat_era, thr_mod, thr_era,
     >                obs_occ, obs_noc, hr, fr, yhr, yfr)
c
      implicit none
c
      integer nxny, nmod, iprb, nyear, nens, nprob
      integer lsm(nxny), iarea, tagwei
      integer num, ip, inew, iens, iprob, narea, iyear, i
      real obs_occ(nxny,nprob),  obs_noc(nxny,nprob) 
      real hr(nxny,nprob), fr(nxny,nprob)
      real xhr(nxny), yhr(nxny), xfr(nxny), yfr(nxny)
      real dat_mod(nxny,nyear,nens), dat_era(nxny,nyear)
      real regwgt(nxny), latwgt(nxny), thr_mod(nxny), thr_era(nxny)
      real xhlp(nens), yhlp, eps, t_mod, t_era
      real zalpha2, zalpha22, xsick
c
c.... reset contingency table
c
      do iprob=1,nprob
      do iarea=1,narea
         obs_occ(iarea,iprob)=0.
         obs_noc(iarea,iprob)=0.
      enddo
      enddo
      eps = tiny(eps)
c
c.... loop over grid points and years to build contingency table
c.... tagwei includes the areal weighting (1) or not (0)
c
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
           do iyear=1,nyear
              yhlp=dat_era(i,iyear)
              do iens=1,nens
                 xhlp(iens)=dat_mod(i,iyear,iens)
              enddo !iens
c
c... check whether threshold is negativ or positiv
c
              if(yhlp.lt.t_era)then !event is observed
                num=0
                do iens=1,nens
                   if(xhlp(iens).lt.t_mod)num=num+1 ! ens'member forecasts event
                enddo !iens
                if(iprb.eq.1)then
                  iprob=1+num
                else
                  iprob=1+num/nmod
                endif
                obs_occ(iarea,iprob)=obs_occ(iarea,iprob)
     >             +lsm(i)*(1.+tagwei*(latwgt(i)-1.)) !add obs_occ in corresp. prob'bin
              else !event is not observed
                num=0
                do iens=1,nens
                   if(xhlp(iens).lt.t_mod)num=num+1 ! ens'member forecasts event
                enddo !iens
                if(iprb.eq.1)then
                  iprob=1+num
                else
                  iprob=1+num/nmod
                endif
                obs_noc(iarea,iprob)=obs_noc(iarea,iprob)
     >             +lsm(i)*(1.+tagwei*(latwgt(i)-1.)) !add obs_noc in corresp. prob'bin
              endif
           enddo !iyear
         endif !regwgt(i).gt.0.
      enddo !i
c
c.... contingency table finished, now calculate HR and FR
c
      do i=1,narea
         yhr(i)=0.
         yfr(i)=0.
         do ip=1,nprob
            yhr(i)=yhr(i)+obs_occ(i,ip)
            yfr(i)=yfr(i)+obs_noc(i,ip)
         enddo
         do iprob=1,nprob
            xhr(i)=0.
            xfr(i)=0.
            do ip=iprob,nprob
               xhr(i)=xhr(i)+obs_occ(i,ip)
               xfr(i)=xfr(i)+obs_noc(i,ip)
            enddo
            inew=nprob-iprob+1
            hr(i,inew)=xhr(i)/(yhr(i)+eps)
            fr(i,inew)=xfr(i)/(yfr(i)+eps)
         enddo
         hr(i,1)=0.
         hr(i,nprob)=1.
         fr(i,1)=0.
         fr(i,nprob)=1.
      enddo !i
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE reliability                   R.Hagedorn, 28-Mar-2002 c
c                                                                      c
c     PURPOSE:                                                         c
c     calculates how often the model predicts the event with a certain c
c     probability and how often in such case the event is observed     c
c     (data for reliability diagrams)                                  c
c                                                                      c
c     INPUT:                                                           c
c     nxny   : grid point dimension                                    c
c     nmod   : number of models involved                               c
c     iprb   : switch for number of prb'bins in case of multi-model    c
c     nyear  : year dimension                                          c
c     nens   : number of ensemble members                              c
c     nprob  : number of probability thresholds                        c
c     iyy1   : first year of data                                      c
c     iyy2   : last  year of data                                      c
c     irflag : area flag                                               c
c     rlsm   : land sea mask                                           c
c     dat_mod: model data                                              c
c     dat_era: era data                                                c
c     thr_mod: threshold at grid point level for model data            c
c     thr_era: threshold at grid point level for era data              c
c                                                                      c
c     OUTPUT:                                                          c
c     occ_mod: number of cases model predicts event with certain prob  c
c     occ_era: number of cases that event occurs when model predicts   c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE reliability(nxny, nmod, iprb, nyear, nens, nprob,
     >                       iyy1, iyy2, irflag, rlsm,
     >                       dat_mod, dat_era, thr_mod, thr_era,
     >                       occ_mod, occ_era)
c
      implicit none
c
      integer nxny, nmod, iprb, nyear, nens, nprob, iyy1, iyy2
      integer irflag(nxny), iprob, i, iyy, iyear, iens, num 
      real dat_mod(nxny,nyear,nens), dat_era(nxny,nyear)
      real thr_mod(nxny), thr_era(nxny)
      real rlsm(nxny), xhlp(nens), yhlp, t_mod, t_era
      real occ_mod(nprob), occ_era(nprob) 
c
c.... reset number of occurences
c
      do iprob=1,nprob
         occ_mod(iprob)=0.
         occ_era(iprob)=0.
      enddo
c
c.... loop over grid points and years to build contingency table
c.... tagwei includes the areal weighting (1) or not (0)
c
      do i=1,nxny
         if(irflag(i).ge.1)then
           t_mod = thr_mod(i)
           t_era = thr_era(i)
           iyear=0
           do iyy=iyy1,iyy2
              iyear=iyear+1
              yhlp=dat_era(i,iyear)
              do iens=1,nens
                 xhlp(iens)=dat_mod(i,iyear,iens)
              enddo !iens
c
c.... check whether threshold is negativ or positiv
c
              if(t_mod.lt.0)then !negative threshold
c
c.... compute the probability the event is forecast
c
                num=0
                do iens=1,nens
                   if(xhlp(iens).lt.t_mod)num=num+1 ! count ens'members forecasting event
                enddo !iens
                if(iprb.eq.1)then
                  iprob=1+num
                else
                  iprob=1+num/nmod
                endif
                occ_mod(iprob)=occ_mod(iprob)+1.*rlsm(i) !add occ_mod in corresp. prob'bin
c
c.... check whether event is observed and add in corresponding bin
c
                if(yhlp.lt.t_era)then !event is observed
                  occ_era(iprob)=occ_era(iprob)+1.*rlsm(i) !add occ_era in corresp. prob'bin
                endif
c
              else !positive threshold
c
c.... compute the probability the event is forecast
c
                num=0
                do iens=1,nens
                   if(xhlp(iens).gt.t_mod)num=num+1 ! count ens'members forecasting event
                enddo !iens
                if(iprb.eq.1)then
                  iprob=1+num
                else
                  iprob=1+num/nmod
                endif
                occ_mod(iprob)=occ_mod(iprob)+1.*rlsm(i) !add occ_mod in corresp. prob'bin
c
c.... check whether event is observed and add in corresponding bin
c
                if(yhlp.gt.t_era)then !event is observed
                  occ_era(iprob)=occ_era(iprob)+1.*rlsm(i) !add occ_era in corresp. prob'bin
                endif
c
              endif !threshold test
c
           enddo !iyy
         endif !irflag(i).ge.1
      enddo !i
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
c     ratio  : ratio of standard deviations (model vs reference)       c
c     rms    : root mean square error                                  c
c     rmsi   : root mean square error of the inflated values           c
c     cor    : ensemble mean correlation                               c
c     sig    : correlation statistical significance                    c
c     snr    : signal-to-noise ratio                                   c
c     sigr   : SNR statistical significance                            c
c     snri   : signal-to-noise ratio of the inflated values            c
c     sigri  : SNR statistical significance for inflated values        c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE rmse(r,h,ndim,n,mdim,m,
     >                vr,vh,vmh,ratio,rms,rmsi,
     >                cor,sigc,snr,sigr,snri,sigri)
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
      real cor, ratio, rms, rmsi, sigc, snr, snri, sigr, sigri
      real vr, vh, vn, vmh, vrt, vht, sigma_noise
      real xmr, xmh, cov, t, ratiot
      real xmhi, vmhi, vhi, vni, thf, eps
      real beta, gamma, cort, covt, fm
      integer ndim, n, mdim, m
      integer i, ii, il, j, ifail
      double precision g01ebf,p,df
      double precision g01edf,df1,df2
c
      eps=tiny(eps)
c
c.... global mean and variance:
c.... vh is the total hindcast variance
c.... vn is the average spread
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
c
c.... sigma_noise is the variance of the noise (mean spread)
c.... computed as the difference between the total variance
c.... and the ensemble mean variance with the factors corresponding
c.... to unbiased estimates
c
c     sigma_noise=(1./(m*n-n))*((m*n-1)*vh**2-(n-1)*vmh**2)
c     if(sigma_noise.lt.0.)sigma_noise=0.
c     sigma_noise=sqrt(sigma_noise)
c     write(*,*)'Average spread (sigma_noise)= ',sigma_noise
c     if(sigma_noise.ne.0.)then
c       snr=(sqrt(vh**2-sigma_noise**2)/(sigma_noise+eps))**2
c     else
c       snr=999.
c     endif
      write(*,'(a,2f10.2)')'variances vh and vmh ',vh,vmh
      write(*,'(a,f10.2)')'Average spread (vn)= ',vn
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
      thf=4.
      if((df1.ge.10.).and.(df2.gt.100.))then
        thf=2.9
        if(snr.lt.0.8)thf=0.0
      endif
      if((df1.ge.100.).and.(df2.gt.100.))then
        thf=2.5
        if(snr.lt.0.8)thf=0.0
      endif
      write(*,'(a,a,2f7.0,f10.2,f7.0)')'degrees of freedom, snr, and ',
     >   ' threshold to apply the test ',df1,df2,snr,thf
      if(thf.eq.0.)then
        p=1.
      elseif(snr.gt.thf)then
        p=0.
      else
        p=g01edf('u',snr,df1,df2,ifail)
      endif
      write(*,'(a,f10.2)')' p-value ',p
      sigr=1-p
c
c.... the final value of snr is given as the ratio of standard deviations
c
      snr=sqrt(snr)
      write(*,'(a,f10.2)')'Signal-to-noise ratio= ',snr
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
c.... is specific for small size samples
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
      write(*,'(a,f10.2,a,f10.2)')'correlation ',cor,' p-value ',p
      if(ifail.ne.0)write(*,*)'error in NAG routine'
      sigc=1-2*p
c
c.... inflation (crossvalidation is applied)
c.... the model is xi=beta*xm+gamma*xr
c.... xi is the inflated value
c.... xm is the ensemble mean
c.... xr is the ensemble member anomaly
c.... biased estimates of the variances are used
c
      do ii=1,n
         il=0
         do i=1,n
            if(i.eq.ii)then
              fm=0.
              do j=1,m
                 fm=fm+h(ii,j)/m
              enddo
              do j=1,m
                 f(j)=h(ii,j)-fm
              enddo
            else
              il=il+1
              do j=1,m
                 hc(il,j)=h(i,j)
              enddo
              rc(il)=r(i)
            endif
         enddo
         xmr=0.
         xmh=0.
         do i=1,n-1
            xm(i)=0.
            do j=1,m
               xmh=xmh+hc(i,j)/(m*(n-1))
               xm(i)=xm(i)+hc(i,j)/m
            enddo
            xmr=xmr+rc(i)/(n-1)
         enddo
         vrt=0.
         vmh=0.
         vht=0.
         covt=0.
         do i=1,n-1
            do j=1,m
               vht=vht+(hc(i,j)-xm(i))**2
            enddo
            vrt=vrt+(rc(i)-xmr)**2
            vmh=vmh+(xm(i)-xmh)**2
            covt=covt+(rc(i)-xmr)*(xm(i)-xmh)
         enddo
         vht=sqrt(vht/(m*(n-1)))
         vrt=sqrt(vrt/(n-1))
         vmh=sqrt(vmh/(n-1))
         covt=covt/(n-1)
         cort=covt/(vmh*vrt)
         beta=cort*vrt/vmh
         gamma=sqrt(1.-cort**2)*vrt/vht
         write(*,'(a,i2.2,a,4f10.2,a,f10.2,a,f10.2)')
     >      'year ',ii,' vht,vrt,vmh,cor=',vht,vrt,vmh,cort,
     >      ' beta=',beta,' gamma=',gamma
         do j=1,m
            hh(ii,j)=beta*fm+gamma*f(j)
c           write(*,'(4(a,f10.2))')
c    >         'fm=',fm,' f(j)=',f(j),' ori=',h(ii,j),' inf=',hh(ii,j)
         enddo
      enddo
      do i=1,n
         do j=1,m
            h(i,j)=hh(i,j)
         enddo
      enddo
c
c.... signal-to-noise ratio for the inflated values
c
      xmhi=0.
      do i=1,n
         xm(i)=0.
         do j=1,m
            xmhi=xmhi+h(i,j)/(m*n)
            xm(i)=xm(i)+h(i,j)/m
         enddo
      enddo
      vhi=0.
      vni=0.
      vmhi=0.
      do i=1,n
         do j=1,m
            vhi=vhi+(h(i,j)-xmhi)**2
            vni=vni+(h(i,j)-xm(i))**2
         enddo
         vmhi=vmhi+(xm(i)-xmhi)**2
      enddo
      vhi=sqrt(vhi/(n*m-1))
      vni=sqrt(vni/(n*m-n))
      vmhi=sqrt(vmhi/(n-1))
c
c     sigma_noise=(1./(m*n-n))*((m*n-1)*vhi**2-(n-1)*vmhi**2)
c     if(sigma_noise.lt.0.)then
c       sigma_noise=0.
c     endif
c     sigma_noise=sqrt(sigma_noise)
c     sigma_noise=sqrt((vhi**2-vmhi**2)*(m/float(m-1)))
c     write(*,*)'Average spread (sigma_noise)= ',sigma_noise
c     snri=(vmhi/(vhi-vmhi))**2
c     if(sigma_noise.ne.0)then
c       snri=(sqrt(vhi**2-sigma_noise**2)/(sigma_noise+eps))**2
c     else
c       snr=999.
c     endif
      write(*,'(a,2f10.2)')'Variances vh and vmh ',vhi,vmhi
      write(*,'(a,f10.2)')'Average spread (vn)= ',vni
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
      thf=4.
      if((df1.ge.10.).and.(df2.gt.100.))then
        thf=2.9
        if(snri.lt.0.8)thf=0.0
      endif
      if((df1.ge.100.).and.(df2.gt.100.))then
        thf=2.5
        if(snri.lt.0.8)thf=0.0
      endif
      write(*,'(a,a,a,2f7.0,f10.2,f7.0)')'(inflated) degrees of ',
     >   'freedom, snr, and ',
     >   'threshold to apply the test ',df1,df2,snri,thf
      if(thf.eq.0.)then
        p=1.
      elseif(snri.gt.thf)then
        p=0.
      else
        p=g01edf('u',snri,df1,df2,ifail)
      endif
      write(*,*)' p-value ',p
      sigri=1-p
c
c.... the final value of snr is given as the ratio of standard deviations
c
      snri=sqrt(snri)
      write(*,'(a,f10.2)')'Signal-to-noise ratio (inflated)= ',snri
c
c.... rmse for the inflated values
c
      rmsi=0.
      do i=1,n
         do j=1,m
            rmsi=rmsi+(h(i,j)-r(i))**2
         enddo
      enddo
      rmsi=sqrt(rmsi/(m*n))
c
      return
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE probsco                    P.Doblas-Reyes  6-Sep-2001 c
c                                                                      c
c     PURPOSE:                                                         c
c     calculates probabilistic scores for the indices time series      c
c                                                                      c
c     INPUT:                                                           c
c     r      : reference value                                         c
c     h      : hindcast value                                          c
c     ndim   : time dimension                                          c
c     mdim   : ensemble dimension                                      c
c     n      : number of time steps                                    c
c     m      : number of ensemble members                              c
c     ncla   : number of classes                                       c
c                                                                      c
c     OUTPUT:                                                          c
c     ss     : RPSS                                                    c
c     sscl   : RPSS statistical significance                           c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE probsco(r,h,ndim,n,mdim,m,ncla,ss,sscl,tr,th)
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
      integer ndim, n, mdim, m, ncla
      real r(ndim),h(ndim,mdim)
      real hh(n*m),sh(ndim,mdim)
      real score(nt),rps1(nt),rps2(nt),y(nt)
      real tr(ncla-1),th(ncla-1)
      real xmr, xmh, vr, vh, cov
      real ss, sse, sscl, rpsh, rpsc
      real dif, epsilon, xi1, xi2
      integer nr(nt)
      integer i, j, jt, it, l1, l2, icla
c
      double precision x1,x2
c.... external functions
      double precision g05saf
      external g05saf
c... external routines
      external g05kgf
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
      call percen(r,ndim,n,tr,ncla)
      write(*,*)'Reference thresholds ',(tr(icla),icla=1,ncla-1)
      do i=1,n
         do j=1,m
            hh((i-1)*m+j)=h(i,j)
         enddo
      enddo
c     write(*,*)'Call to percentiles for hindcasts'
      call percen(hh,n*m,n*m,th,ncla)
      write(*,*)'Hindcast thresholds ',(th(icla),icla=1,ncla-1)
c
c.... computing rpss
c
c     write(*,*)'Call to RPSS'
      call rpss(r,h,ndim,n,mdim,m,tr,th,ncla,rpsh,rpsc,ss)
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
        sscl=1.
      elseif(it.le.1)then
        sscl=0.
      else
        xi1=ss-y(it)
        xi2=y(it+1)-ss
        xi1=xi1/(xi1+xi2)
        xi2=1.-xi1
        sscl=(it*xi2+(it+1)*xi1)/100.
      endif
      if(ss.lt.0.)sscl=0.
      write(*,'(a,i3.3,a,f10.2,a,f6.1)')
     >   'it=',it,' rpss=',ss,' Confidence level=',sscl
c
      return
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE prob                         R. Hagedorn, 09-Aug-2002 c
c                                                                      c
c     PURPOSE:                                                         c
c     computes probabilities of a given event                          c
c                                                                      c
c     INPUT:                                                           c
c     data  : input data                                               c
c     event : event                                                    c
c     nxny  : number of grid points                                    c
c     nens  : number of ensemble members                               c
c     rflag : flag for pos or negative threshold                       c
c                                                                      c
c     OUTPUT:                                                          c
c     psec4 : probabilities                                            c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE prob(data,event,nxny,nens,rflag,psec4)
c
      implicit none
c
      integer nxny, nens, i, iens
      real data(nxny,nens), event(nxny), psec4(nxny)
      real rflag, rens
c
c.... calculate probabilities
c
      rens=float(nens)
      if (rflag.lt.0.) then
         do i=1,nxny
            psec4(i) = 0.
            do iens=1,nens
               if (data(i,iens).lt.event(i)) psec4(i)=psec4(i)+1./rens
            enddo
         enddo
      else
         do i=1,nxny
            psec4(i) = 0.
            do iens=1,nens
               if (data(i,iens).gt.event(i)) psec4(i)=psec4(i)+1./rens
            enddo
         enddo
      endif
c
      return
c
      end
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE get_nummod                   R. Hagedorn, 25-Apr-2003 c
c                                                                      c
c     PURPOSE:                                                         c
c     gives model combinations (2,3,4 or 5 out of 6)                   c
c                                                                      c
c     INPUT:                                                           c
c     inummod: number of models to form multi-model                    c
c                                                                      c
c     OUTPUT:                                                          c
c     ncombi: number of possible combinations                          c
c     imodcombi: field which gives for every combination the chosen    c
c                ensemble members                                      c
c                                                                      c
c----------------------------------------------------------------------c
c
      SUBROUTINE get_nummod(n1,n2,inummod,ncombi,imodcombi)
c
      implicit none
c
      integer a,b,c,d,e,f, i, nz, i1, i2, iens, iensa, ihlp, model(6)
      integer a1,b1,c1,d1,e1,f1, n1,n2
      integer inummod, ncombi, nens, imodcombi(n1,n2)
c
      do i1=1,n1
      do i2=1,n2
         imodcombi(i1,i2)=0
      enddo
      enddo
      do i=1,6
         model(i)=i
      enddo
c
c.... determine imodcombi
c
      if (inummod.eq.1) then
         nz=0    
         do a=1,6
            nz=nz+1
            i1 = (a-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=a
            enddo
         enddo
         ncombi=nz
      endif
c
      if (inummod.eq.2) then
         nz=0
         do a=1,6 
         b1=a+1
         do b=b1,6 
            if (a.eq.b) goto 20
            nz=nz+1
            i1 = (a-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=a
            enddo
            i1 = (b-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=b
            enddo
 20         continue
         enddo
         enddo
         ncombi=nz
      endif
c
      if (inummod.eq.3) then
         nz=0
         do a=1,6 
         b1=a+1
         do b=b1,6
         c1=b+1
         do c=c1,6
            if (a.eq.b .or. a.eq.c .or. b.eq.c) goto 30
            nz=nz+1
            i1 = (a-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=a
            enddo
            i1 = (b-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=b
            enddo
            i1 = (c-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=c
            enddo
 30      continue
         enddo
         enddo
         enddo
         ncombi=nz
      endif
c
      if (inummod.eq.4) then
         nz=0
         do a=1,6 
         b1=a+1
         do b=b1,6 
         c1=b+1
         do c=c1,6 
         d1=c+1
         do d=d1,6
            if (a.eq.b .or. a.eq.c .or. a.eq.d .or. 
     >          b.eq.c .or. b.eq.d .or.
     >          c.eq.d) goto 40
            nz=nz+1
            i1 = (a-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=a
            enddo
            i1 = (b-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=b
            enddo
            i1 = (c-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=c
            enddo
            i1 = (d-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=d
            enddo
 40      continue
         enddo
         enddo
         enddo
         enddo
         ncombi=nz
      endif
c
      if (inummod.eq.5) then
         nz=0
         do a=1,6
         b1=a+1
         do b=b1,6
         c1=b+1
         do c=c1,6
         d1=c+1
         do d=d1,6 
         e1=d+1
         do e=e1,6
            if (a.eq.b .or. a.eq.c .or. a.eq.d .or. a.eq.e. or.
     >          b.eq.c .or. b.eq.d .or. b.eq.e .or.
     >          c.eq.d .or. c.eq.e) goto 50
            nz=nz+1
            i1 = (a-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=a
            enddo
            i1 = (b-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=b
            enddo
            i1 = (c-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=c
            enddo
            i1 = (d-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=d
            enddo
            i1 = (e-1)*9 + 1
            i2 = i1+8
            do iens=i1,i2
               imodcombi(nz,iens)=e
            enddo
 50      continue
         enddo
         enddo
         enddo
         enddo
         enddo
         ncombi=nz
      endif
c
      if (inummod.eq.6) then
         do ihlp=1,6
            do iens=1,9
               iensa = (ihlp-1)*9 + iens
               imodcombi(1,iensa)=ihlp
            enddo
         enddo
         ncombi=1
      endif
c
c      do i1=1,n1
c         write(*,*)(imodcombi(i1,i2),i2=1,n2)
c      enddo
c
      return
c
      end
c
c
c
c----------------------------------------------------------------------c
c     SUBROUTINE fit                          R. Hagedorn, 11-Sep-2003 c
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
      SUBROUTINE fit(x,y,ndat,a,b)
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
