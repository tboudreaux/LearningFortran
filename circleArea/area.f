      program circle
      real r, area, pi
      parameter(pi = 3.14159)
      write(*,*) 'Give radius r: '
      read(*,*) r
      area = pi*r*r
      write(*,*) 'Area = ', area
      stop 
      end
