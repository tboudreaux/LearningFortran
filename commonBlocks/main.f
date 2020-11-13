       PROGRAM MAIN
        COMMON/MAINBLOCK/MA,MB,MC
        DATA MA/1/, MB/2/, MC/3/
        WRITE(*,*)"In Main Program Main Block is",MA,MB,MC
        CALL SUBA
        CALL SUBB
       END
