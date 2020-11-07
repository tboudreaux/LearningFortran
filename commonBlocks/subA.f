       SUBROUTINE SUBA
        COMMON/SUBABLOCK/ASA,ASB,ASC
        COMMON/MAINBLOCK/MA,MB,MC
        CHARACTER(1)ASA
        CHARACTER(1)ASB
        CHARACTER(1)ASC
        DATA ASA/'a'/,ASB/'b'/,ASC/'c'/
        WRITE(*,*)"In Subroutine A Local block ", ASA," ", ASB, " ", ASC
        WRITE(*,*)"In Subroutine A main block",MA,MB,MC
       END
