c
c----------------------------------------------------------------------c
c     PROGRAM uv2div                      F. Doblas-Reyes, 26-Apr-2006 c
c                                                                      c
c     PURPOSE:                                                         c
c     reads u,v grid point data and calculates divergence              c
c                                                                      c
c     INPUT:                                                           c
c     131_LEV_CCYYMM files                                             c
c     132_LEV_CCYYMM files                                             c
c                                                                      c
c     OUTPUT:                                                          c
c     155_LEV_CCYYMM files                                             c
c                                                                      c
c     USAGE:                                                           c
c     uv2div.x < nlist                                                 c
c                                                                      c
c     COMPILING:                                                       c
c     pgf90 -pg -O3 uv2div.f tools.f -o uv2div.x $EMOSLIB              c
c                                                                      c
c     MODS:                                                            c
c                                         F. Doblas-Reyes, 26-Apr-2006 c
c     Based on the original programme from DEMETER                     c
c                                         F. Doblas-Reyes, 26-Jul-2006 c
c     Adapted to the linux machines                                    c
c                                                                      c
c----------------------------------------------------------------------c
c
      program uv2div
c
      implicit none
c
c.... define allocatable fields
c
      real, allocatable :: u(:,:), v(:,:), div(:,:)
 
c
c.... definitions for GRIBEX call
c
      integer kleng, koutlen
      parameter (kleng=200000)
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
      integer klenp, kword, kret
c
      real psec2(512)
      real psec2_sav(512)
      real psec3(2)
      real psec3_sav(2)
      real, allocatable :: psec4(:)
      real, allocatable :: psec4u(:)
      real, allocatable :: psec4v(:)
c
c.... other stuff
c
      character yifile*14, yofile*17
      character yidisk*80, yodisk*80
      character ybasedisk*80, yfile*80
      character expt*4
      real rmiss, xtop
      integer iyy, imm, ipar, ilev, istat
      integer iread, iunitu, iunitv, ounit, ix, iy
      integer lena, nx, ny, nxny, i
c
      namelist /control/  nx,  ny, 
     >                   iyy,  imm, ilev, expt,
     >                   ybasedisk
c
c.... set default values and read input namelist
c
      nx   = 144
      ny   = 71
      iyy  = 1986
      imm  = 11
      ilev = 200
      expt = 'xxxx'
      ybasedisk = 'yyyy'
c
      read(5,control)
      write(*,*)'processing date:'
      write(*,*)'nx = ', nx
      write(*,*)'ny = ', ny
      write(*,*)'YY = ', iyy
      write(*,*)'MM = ', imm
c
      nxny   = nx*ny
      klenp  = nxny
c
c.... allocate fields
c
      allocate (psec4(nxny), stat=istat)
      allocate (psec4u(nxny), stat=istat)
      allocate (psec4v(nxny), stat=istat)
      allocate (u(nx,ny), stat=istat)
      allocate (v(nx,ny), stat=istat)
      allocate (div(nx,ny), stat=istat)
c
c.... set missing data indicators and reset psec fields
c
      rmiss    = -1.e30
      psec3(2) = rmiss
      do i=1,klenp
         psec4u(i) = 0.
         psec4v(i) = 0.
      enddo
c
c.... set disknames
c
      yidisk = ybasedisk(1:lena(ybasedisk))//'/'
      yodisk = ybasedisk(1:lena(ybasedisk))//'/'
c
c.... open input and output files
c
      yifile='131_000_CCYYMM'
      write(yifile( 5: 7),'(i3.3)')ilev
      write(yifile( 9:12),'(i4.4)')iyy
      write(yifile(13:14),'(i2.2)')imm
      yfile=yidisk(1:lena(yidisk))//yifile
      write(*,*)'open input file: ',yfile
      kret=0
      call pbopen(iunitu,yfile,'r',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
      yifile='132_000_CCYYMM'
      write(yifile( 5: 7),'(i3.3)')ilev
      write(yifile( 9:12),'(i4.4)')iyy
      write(yifile(13:14),'(i2.2)')imm
      yfile=yidisk(1:lena(yidisk))//yifile
      write(*,*)'open input file: ',yfile
      kret=0
      call pbopen(iunitv,yfile,'r',kret)
      if(kret.ne.0)then
        write(*,*)'Error in opening file: kret=',kret
        goto 997
      endif
      yofile='155_000_CCYYMM'
      write(yofile( 5: 7),'(i3.3)')ilev
      write(yofile( 9:12),'(i4.4)')iyy
      write(yofile(13:14),'(i2.2)')imm
      yfile=yodisk(1:lena(yodisk))//yofile
      write(*,*)'open output file: ',yfile
      kret=0
      call pbopen(ounit,yfile,'w',kret)
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
      call pbgrib(iunitu,kgrib,kleng,koutlen,kret)
      if(kret.lt.0)then
        if(kret.eq.-1)then
          write(*,*)'End of file for u-component'
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
     >            psec4u,klenp,kgrib,kleng,kword,'D',kret)
      if(kret.gt.0)then
        write(6,*)'Error decoding data: kret=',kret
        goto 997
      endif
c
      kret=1
      call pbgrib(iunitv,kgrib,kleng,koutlen,kret)
      if(kret.lt.0)then
        if(kret.eq.-1)then
          write(*,*)'End of file for v-component'
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
     >            psec4v,klenp,kgrib,kleng,kword,'D',kret)
      if(kret.gt.0)then
        write(6,*)'Error decoding data: kret=',kret
        goto 997
      endif
c
c.... write u and v 1-dim data to 2-dim fields
c
      ix=1
      iy=1
      do iy=1,ny
      do ix=1,nx
         i=ix+(iy-1)*nx
         u(ix,iy)=psec4u(i)
         v(ix,iy)=psec4u(i)
      enddo
      enddo
c
      xtop = ksec2(4)*0.001
      call divergence(u,v,nx,ny,xtop,div)
c
      ix=1
      iy=1
      do iy=1,ny
      do ix=1,nx
         i=ix+(iy-1)*nx
         psec4(i)=div(ix,iy)
      enddo
      enddo
c
c.... write output
c
      ksec1(6) = 155
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
c
c.... do further fields
c
      goto 10
c
20    continue
      call pbclose(iunitu,kret)
      call pbclose(iunitv,kret)
      call pbclose(ounit,kret)
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
