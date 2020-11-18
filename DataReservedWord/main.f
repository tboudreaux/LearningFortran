       PROGRAM MAIN
        INTEGER A,B
        REAL, DIMENSION(4) :: ARR
        ! DATA ARR/0.0, 1.0, 2.0, 3.0/
        A = 0
        B = 1
        CALL SUB(A, B, ARR)
        WRITE(*,*)"A is ", A, ", B is ", B
        WRITE(*,*)"ARR is (", ARR(1), ARR(2), ARR(3), ARR(4), ")" 
       END
