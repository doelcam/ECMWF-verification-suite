      implicit none
      integer n, ifail
      external g01ebf
      double precision g01ebf, df, p, t
      real z, r, ciu, cil, sig
      read(*,*)r,n
      t=r*sqrt((n-2)/(1-r**2))
      ifail=0
      df=float(n-2)
      write(*,'(a,f7.0,f10.2)')'degrees of freedom and t ',df,t
      p=g01ebf('u',t,df,ifail)
      if(ifail.ne.0)write(*,*)'error in NAG routine'
      sig=1-2*p
      write(*,'(3(a,f10.3))')'correlation ',r,' p-value ',p,' sig ',sig
      write(*,*)'First method to compute 95% confidence intervals'
      z=0.5*log((1+r)/(1-r))
      ciu=tanh(z+1.96/(n-3)**0.5)
      cil=tanh(z-1.96/(n-3)**0.5)
      write(*,'(2(a,f10.3))')'lower edge ',cil,' upper edge ',ciu
      write(*,*)'Second method to compute 95% confidence intervals'
      ciu=r+1.96*(1-r**2)/(n-1)**0.5
      cil=r-1.96*(1-r**2)/(n-1)**0.5
      write(*,'(2(a,f10.3))')'lower edge ',cil,' upper edge ',ciu
      stop
      end
