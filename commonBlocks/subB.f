       SUBROUTINE SUBB
        COMMON/MAINBLOCK/MA,MB,MC
        COMMON/SUBBBLOCK/BSA,BSB,BSC
        COMMON/SUBABLOCK/ASA,ASB,ASC
        DATA BSA/10.0/,BSB/20.0/,BSC/30.0/
        CHARACTER(1)ASA
        CHARACTER(1)ASB
        CHARACTER(1)ASC
        WRITE(*,*)"In Subroutine B Local Block is",BSA,BSB,BSC
        WRITE(*,*)"In Subroutine B Main Block is",MA,MB,MC
        WRITE(*,*)"In Subroutine B A Block is ",ASA," ",ASB," ",ASC
        CALL SUBA
        WRITE(*,*)"In Subroutine B A Block is ",ASA," ",ASB," ",ASC
       END
