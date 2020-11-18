       SUBROUTINE SUB(A,B,ARR)
        REAL, DIMENSION(4) :: ARR
        LOGICAL LT,LA
        ARR = (/0,1,2,3/)    
        LT =.TRUE.
        LA = .FALSE.
        WRITE(*,*)"SUB CALLED"
       END
