       PROGRAM MAIN
        COMMON/MAINBLOCK/MA,MB,MC
        DATA MA/1/, MB/2/, MC/3/
        WRITE(*,*)"In Main Program Main Block is",MA,MB,MC
        CALL SUBA
        WRITE(*,*)"In Main Program Sub A Block is",ASA,ASB,ASC
        CALL SUBB
       END
