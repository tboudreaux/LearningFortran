      PROGRAM MAIN
      real refx, refml, refL, regR
      real cxx, cxml, cxL, cxR
      real cmlc, cmlml, cmlL, cmlR
      real dldx,drdx,dlda,drda,da,dx

      refL = 4
      drdx = 2
      dldx = 7
      refR = 4
      drda = 3
      dlda = 6

      da = (refL*drdx/dldx - refR)/(drda - dlda*drdx/dldx)
      WRITE(*,*)da
      END
