      Program example
      implicit real*8(a-h,o-z)
c
c     example program to call FLUX 
c
      ! t9=1.55e-1 ! sun temperature as an example
      ! t9=1.0002498554836535E-003
      t9=1.0079532158217444E-003

      flx =0.d0
      dflx=0.d0

      write(*,*)'give iprint, ic, inter [see comments in flux.f]'
      write(*,*)'ic=0 to stop'

      read(*,*)iprint,ic,inter

      flx =0.d0
      dflx=0.d0

      if(ic.ne.0)then
         call ppflux(t9,ic,inter,flx,dflx)
         write(*,1000)t9,flx,dflx

      endif

 1000 format('t9 = ', e15.8, ', flx = ',e15.8,' +/- ',e15.8,/)

      End
c
c--------------------------------------------------------------------------
c                                   FLUX.F
c                              Laura E. Marcucci
c                     University of Pisa and INFN - sezione di Pisa
c
c   This two subroutines are needed to calculate the flux for the proton-proton
c   weak capture reaction, given the star temperature in 10^9 K, presented in
c   Tognelli, Degl'Innocenti, Marcucci, Prada Moroni, Physics Letters B 742 (2015), pp. 189-194.
c   It is based on the following articles: 
c
c   - L.E. Marcucci et al., Phys. Rev. Lett. 110, 192503 (2013) and
c
c   - NACRE II, Nucl. Phys. A 918, 61 (2013) [ Eq.(3) ].
c
c   The code is structured in such a way that there are several options:
c
c   (1) the initial p-p wave function can include only the S-wave contribution
c       or also all the P-wave contributions. The S+P waves is what has been
c       suggested in L.E. Marcucci et al., the S-wave contribution is what
c       has been used sofar in the literature.
c
c   (2) The user can calculate the flux using the calculated values for
c       the p-p astrophysical S-factor on an energy grid of 101 points
c       of step = 1 keV, or alternatively use the values of S(0), S^n(0),
c       [S^n(0) is the n-th derivative of S(E) calculated at zero
c       energy] up to n=3, and tabulate S(E) with the preferred steps.
c       In this second case, in parameter, the user needs to adjust
c       he and ne_int (see below).
c
c   The code should be compiled as:
c     gfortran -O4 flux.f
c   and the main program above is just an example to show how to run 
c   the program
c
c   The user needs to know only the following inputs indeces:
c
c     iprint = 1/0 for printing/not printing intermediate results
c     t9 = star temperature in 10^9 K
c     ic = 1 for S+P waves
c        = 2 for only S-waves
c     inter = 1 for using the calculated S-factor
c           = 2 for using the fitted values 
c               for S(0), S'(0), S''(0) and S'''(0) and then 
c               tabulating the S(E) as preferred.
c               In this case, in parameter the user should fix:
c                         he=1.e-3 ne_int=101 : step (given in MeV) of 1 keV
c                         he=1.e-4 ne_int=1001: step (given in MeV0 of 0.1 keV 
c                           etc....
c     
c     In output, the user finds:
c
c     flx = flux in cm^3 mol^{-1} s^{-1}
c     dflx= theoretical uncertainty on the flux
c
c--------------------------------------------------------------------------
c     For any assistance, send an email to laura.marcucci@df.unipi.it
c--------------------------------------------------------------------------
      subroutine ppflux(t9,ic,inter,flx,dflx)
      implicit real*8(a-h,o-z)
c
      parameter(ne=101,ne_int=101,he=1.e-3)
c
      dimension sfup(ne,2),sfdw(ne,2),
     x          a_up(ne),a_dw(ne)
      dimension s0up(2),s1up(2),s2up(2),s3up(2),
     x          s0dw(2),s1dw(2),s2dw(2),s3dw(2),
     x          aint_up(ne_int),aint_dw(ne_int)
c
c
c     astrophysical S-factor in MeV b - upper values
      data sfup / 4.036e-25 , 4.081e-25 , 4.130e-25 , 4.178e-25 , 
     x            4.228e-25 , 4.279e-25 , 4.332e-25 , 4.386e-25 ,
     x            4.441e-25 , 4.496e-25 , 4.555e-25 , 4.615e-25 , 
     x            4.672e-25 , 4.735e-25 , 4.799e-25 , 4.861e-25 , 
     x            4.927e-25 , 4.991e-25 , 5.058e-25 , 5.124e-25 , 
     x            5.194e-25 , 5.262e-25 , 5.330e-25 , 5.402e-25 , 
     x            5.474e-25 , 5.546e-25 , 5.620e-25 , 5.695e-25 , 
     x            5.769e-25 , 5.845e-25 , 5.923e-25 , 5.998e-25 , 
     x            6.077e-25 , 6.157e-25 , 6.236e-25 , 6.316e-25 , 
     x            6.397e-25 , 6.479e-25 , 6.562e-25 , 6.645e-25 , 
     x            6.729e-25 , 6.814e-25 , 6.899e-25 , 6.986e-25 , 
     x            7.073e-25 , 7.160e-25 , 7.249e-25 , 7.338e-25 , 
     x            7.428e-25 , 7.518e-25 , 7.610e-25 , 7.702e-25 ,
     x            7.795e-25 , 7.888e-25 , 7.982e-25 , 8.077e-25 , 
     x            8.172e-25 , 8.269e-25 , 8.366e-25 , 8.464e-25 ,
     x            8.562e-25 , 8.661e-25 , 8.761e-25 , 8.862e-25 , 
     x            8.964e-25 , 9.066e-25 , 9.169e-25 , 9.272e-25 ,
     x            9.377e-25 , 9.482e-25 , 9.588e-25 , 9.694e-25 ,
     x            9.802e-25 , 9.910e-25 ,10.019e-25 ,10.128e-25 ,
     x           10.238e-25 ,10.349e-25 ,10.461e-25 ,10.574e-25 ,
     x           10.687e-25 ,10.801e-25 ,10.916e-25 ,11.032e-25 ,
     x           11.148e-25 ,11.265e-25 ,11.383e-25 ,11.501e-25 ,
     x           11.621e-25 ,11.741e-25 ,11.862e-25 ,11.984e-25 ,
     x           12.106e-25 ,12.229e-25 ,12.353e-25 ,12.478e-25 ,
     x           12.604e-25 ,12.730e-25 ,12.857e-25 ,12.985e-25 ,
     x           13.114e-25 ,! S+P-waves 
     x            4.013e-25 , 4.052e-25 , 4.098e-25 , 4.145e-25 , 
     x            4.192e-25 , 4.240e-25 , 4.290e-25 , 4.342e-25 , 
     x            4.394e-25 , 4.446e-25 , 4.503e-25 , 4.560e-25 , 
     x            4.614e-25 , 4.674e-25 , 4.735e-25 , 4.794e-25 , 
     x            4.857e-25 , 4.918e-25 , 4.982e-25 , 5.045e-25 , 
     x            5.111e-25 , 5.176e-25 , 5.240e-25 , 5.309e-25 , 
     x            5.377e-25 , 5.446e-25 , 5.516e-25 , 5.587e-25 , 
     x            5.657e-25 , 5.729e-25 , 5.803e-25 , 5.874e-25 , 
     x            5.949e-25 , 6.024e-25 , 6.099e-25 , 6.175e-25 , 
     x            6.251e-25 , 6.328e-25 , 6.406e-25 , 6.485e-25 , 
     x            6.564e-25 , 6.644e-25 , 6.724e-25 , 6.805e-25 , 
     x            6.887e-25 , 6.969e-25 , 7.053e-25 , 7.136e-25 , 
     x            7.220e-25 , 7.305e-25 , 7.391e-25 , 7.477e-25 , 
     x            7.564e-25 , 7.651e-25 , 7.739e-25 , 7.828e-25 , 
     x            7.917e-25 , 8.007e-25 , 8.097e-25 , 8.189e-25 , 
     x            8.280e-25 , 8.373e-25 , 8.466e-25 , 8.559e-25 , 
     x            8.654e-25 , 8.749e-25 , 8.844e-25 , 8.940e-25 , 
     x            9.037e-25 , 9.134e-25 , 9.232e-25 , 9.331e-25 , 
     x            9.430e-25 , 9.530e-25 , 9.630e-25 , 9.731e-25 , 
     x            9.833e-25 , 9.935e-25 ,10.038e-25 ,10.142e-25 ,
     x           10.246e-25 ,10.352e-25 ,10.456e-25 ,10.562e-25 ,
     x           10.669e-25 ,10.776e-25 ,10.884e-25 ,10.993e-25 ,
     x           11.102e-25 ,11.212e-25 ,11.322e-25 ,11.433e-25 ,
     x           11.545e-25 ,11.657e-25 ,11.770e-25 ,11.883e-25 ,
     x           11.998e-25 ,12.112e-25 ,12.228e-25 ,12.344e-25 ,
     x           12.461e-25 / ! S-wave 
c     astrophysical S-factor in MeV b - lower values
      data sfdw / 4.024e-25 , 4.071e-25 , 4.119e-25 , 4.167e-25 , 
     x            4.217e-25 , 4.268e-25 , 4.320e-25 , 4.374e-25 , 
     x            4.429e-25 , 4.484e-25 , 4.543e-25 , 4.603e-25 , 
     x            4.660e-25 , 4.723e-25 , 4.786e-25 , 4.848e-25 , 
     x            4.914e-25 , 4.978e-25 , 5.045e-25 , 5.111e-25 , 
     x            5.181e-25 , 5.248e-25 , 5.317e-25 , 5.389e-25 , 
     x            5.461e-25 , 5.533e-25 , 5.606e-25 , 5.681e-25 , 
     x            5.755e-25 , 5.831e-25 , 5.908e-25 , 5.983e-25 , 
     x            6.062e-25 , 6.142e-25 , 6.221e-25 , 6.300e-25 , 
     x            6.381e-25 , 6.463e-25 , 6.545e-25 , 6.628e-25 , 
     x            6.712e-25 , 6.796e-25 , 6.882e-25 , 6.968e-25 , 
     x            7.054e-25 , 7.142e-25 , 7.230e-25 , 7.319e-25 , 
     x            7.408e-25 , 7.498e-25 , 7.589e-25 , 7.681e-25 , 
     x            7.774e-23 , 7.867e-25 , 7.960e-25 , 8.055e-25 , 
     x            8.150e-25 , 8.246e-25 , 8.343e-25 , 8.441e-25 , 
     x            8.539e-25 , 8.638e-25 , 8.738e-25 , 8.838e-25 , 
     x            8.939e-25 , 9.041e-25 , 9.144e-25 , 9.247e-25 ,
     x            9.351e-25 , 9.456e-25 , 9.561e-25 , 9.668e-25 , 
     x            9.775e-25 , 9.882e-25 , 9.991e-25 ,10.100e-25 ,
     x           10.210e-25 ,10.321e-25 ,10.432e-25 ,10.544e-25 , 
     x           10.657e-25 ,10.771e-25 ,10.886e-25 ,11.000e-25 ,
     x           11.117e-25 ,11.234e-25 ,11.351e-25 ,11.470e-25 ,
     x           11.589e-25 ,11.709e-25 ,11.829e-25 ,11.951e-25 ,
     x           12.073e-25 ,12.196e-25 ,12.319e-25 ,12.444e-25 ,
     x           12.569e-25 ,12.695e-25 ,12.822e-25 ,12.950e-25 ,
     x           13.078e-25 , ! S+P-waves 
     x            4.003e-25 , 4.042e-25 , 4.088e-25 , 4.134e-25 , 
     x            4.181e-25 , 4.230e-25 , 4.280e-25 , 4.331e-25 ,
     x            4.383e-25 , 4.435e-25 , 4.491e-25 , 4.549e-25 , 
     x            4.602e-25 , 4.662e-25 , 4.723e-25 , 4.781e-25 , 
     x            4.844e-25 , 4.904e-25 , 4.968e-25 , 5.031e-25 ,
     x            5.097e-25 , 5.161e-25 , 5.225e-25 , 5.294e-25 , 
     x            5.362e-25 , 5.430e-25 , 5.500e-25 , 5.571e-25 , 
     x            5.640e-25 , 5.712e-25 , 5.785e-25 , 5.856e-25 ,
     x            5.930e-25 , 6.005e-25 , 6.080e-25 , 6.155e-25 , 
     x            6.231e-25 , 6.308e-25 , 6.385e-25 , 6.463e-25 , 
     x            6.542e-25 , 6.622e-25 , 6.702e-25 , 6.782e-25 ,
     x            6.864e-25 , 6.946e-25 , 7.028e-25 , 7.111e-25 , 
     x            7.195e-25 , 7.280e-25 , 7.365e-25 , 7.451e-25 , 
     x            7.537e-25 , 7.624e-25 , 7.712e-25 , 7.799e-25 , 
     x            7.888e-25 , 7.977e-25 , 8.068e-25 , 8.158e-25 ,
     x            8.250e-25 , 8.342e-25 , 8.434e-25 , 8.527e-25 ,
     x            8.621e-25 , 8.715e-25 , 8.810e-25 , 8.906e-25 , 
     x            9.002e-25 , 9.099e-25 , 9.196e-25 , 9.294e-25 ,
     x            9.393e-25 , 9.492e-25 , 9.592e-25 , 9.693e-25 ,
     x            9.794e-25 , 9.896e-25 , 9.998e-25 ,10.101e-25 ,
     x           10.205e-25 ,10.309e-25 ,10.414e-25 ,10.519e-25 ,
     x           10.625e-25 ,10.732e-25 ,10.839e-25 ,10.947e-25 ,
     x           11.056e-25 ,11.165e-25 ,11.275e-25 ,11.385e-25 ,
     x           11.497e-25 ,11.608e-25 ,11.721e-25 ,11.834e-25 ,
     x           11.947e-25 ,12.061e-25 ,12.176e-25 ,12.292e-25 ,
     x           12.408e-25 / ! S-waves 
c
      data s0up / 4.036e-25 , 4.013e-25 / , ! S(0)
     x     s0dw / 4.024e-25 , 4.003e-25 /
      data s1up / 11.95 , 11.43 / , ! S'(0)/S(0)
     x     s1dw / 11.93 , 11.41 /
      data s2up / 249.0 , 240.1 / , ! S''(0)/S(0)
     x     s2dw / 248.6 , 239.1 /
      data s3up /-1175.0 , -1459.0 / , ! S'''(0)/S(0)
     x     s3dw /-1191.0 , -1469.0 /
c
      amu=0.504 ! proton-proton reduced mass in amu=931.494 MeV
      eta=-0.702547503d0 !  eta=-2.*pi*0.1575*sqrt(amu)
c
      acost=2.627021d10 ! acost=3.73d10/sqrt(amu)/2.
      act=acost*t9**(-1.5)
c
      flx =0.d0
      dflx=0.d0
      flx_up=0.d0
      flx_dw=0.d0
c
      if(inter.eq.1)then
c
      nx=ne
      hx=0.001d0 ! step of 1 keV
c
      do i=1,ne
         ee=hx*(i-1)         
         dcost=act*dexp(eta/sqrt(ee)-11.605*ee/t9)         
         a_up(i)=dcost*sfup(i,ic)
         a_dw(i)=dcost*sfdw(i,ic)
      enddo
c
c     integration
      flx_up=b5(1,nx,0,0,hx,0.d0,0.d0,a_up,1)/22.5d0
      flx_dw=b5(1,nx,0,0,hx,0.d0,0.d0,a_dw,1)/22.5d0
c
      endif
      if(inter.eq.2)then
c
         s1fup=s0up(ic)*s1up(ic)
         s1fdw=s0dw(ic)*s1dw(ic)
c     
         s2fup=s0up(ic)*s2up(ic)/2.d0
         s2fdw=s0dw(ic)*s2dw(ic)/2.d0
c
         s3fup=s0up(ic)*s3up(ic)/6.d0
         s3fdw=s0dw(ic)*s3dw(ic)/6.d0
c
         nx=ne_int
         hx=he
         do i=1,ne_int
            ee=he*(i-1)
            dcost=act*dexp(eta/sqrt(ee)-11.605*ee/t9)
            sf_up=s0up(ic)+s1fup*ee+s2fup*ee*ee+s3fup*ee**3
            sf_dw=s0dw(ic)+s1fdw*ee+s2fdw*ee*ee+s3fdw*ee**3
            aint_up(i)=dcost*sf_up
            aint_dw(i)=dcost*sf_dw
         enddo
c     
c     integration
      flx_up=b5(1,nx,0,0,hx,0.d0,0.d0,aint_up,1)/22.5d0
      flx_dw=b5(1,nx,0,0,hx,0.d0,0.d0,aint_dw,1)/22.5d0
c
      endif
c
c
      flx =0.5d0*(flx_up+flx_dw)
      dflx=0.5d0*(flx_up-flx_dw)
c
 1000 format('flx_up = ',e15.8,/,'flx_dw = ',e15.8,/)
 1100 format('flx trace: ', e15.8)
c
      end


c___________________________________________________________________
      double precision function b5(jb,m1,m2,m3,h1,h2,h3,a,ias)
c___________________________________________________________________
      implicit real*8(a-h,o-z)
c
      dimension a(1)
c
      b5=0.d0
      n=m1
      h5=h1
      j0=ias
      ir=jb-2
 1    if(n-4) 44,2,2
 2    hl5=h5/32.d0
      nb=n/4
      n1=n-4*nb+1
      s2=0.d0
      s3=s2
      s4=s2
      l=j0
      do 4 i=1,nb
      s2=s2+a(l+2)
      s3=s3+a(l+1)+a(l+3)
      s4=s4+a(l)+a(l+4)
 4    l=l+4
      goto (8,12,16,20),n1
 8    ax=0.d0
      goto 40
 12   ax=hl5*(-19.d0*a(l-3)+106.d0*a(l-2)-264.d0*a(l-1)+646.d0*a(l)+
     >          251.d0*a(l+1))
      goto 40
 16   ax=hl5*(-8.d0*a(l-2)+32.d0*a(l-1)+192.d0*a(l)+992.d0*a(l+1)+
     >           232.d0*a(l+2))
      goto 40
 20   ax=hl5*(-27.d0*a(l-1)+378.d0*a(l)+648.d0*a(l+1)+918.d0*a(l+2)+
     >           243.d0*a(l+3))
 40   b5=b5+ax+h5*(7.d0*s4+32.d0*s3+12.d0*s2)
      if(ir) 60,50,55
 50   j0=m1+ias
      n=m2
      h5=h2
      ir=-1
      goto 1
 55   j0=m2+m1+ias
      n=m3
      h5=h3
      ir=0
      goto 1
 60   return
c     
 44   write(4,200)
 200  format(//1x,'  number of intervals lower than 4'/)
      stop
      end
c
c
