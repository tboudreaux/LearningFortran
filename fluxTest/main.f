       PROGRAM main
           implicit real*8(a-h,o-z)

           t9=1.55e-2
           flx =0.d0
           dflx=0.d0
           write(*,*)"Before flx is: ", flx, " and dflx is: ", dflx
           call ppflux(1, t9, 1, 1, flx, dflx)
           write(*,*)"After flx is: ", flx, " and dflx is: ", dflx

       END
