       PROGRAM main
         REAL *8 test1, test2, res, sub
         test1 = 10.d0 
         test2 = 11.d0
         write(*,*)test1, test2
         res = sub(test1,test2)
         write(*,*)res
       END

       double precision FUNCTION sub(a, b)
          REAL *4 a
          REAL *4 b 
          REAL *8 res
          write(*,*)"HERE"

          res = a + b
          sub = res
          
          return 

       END
