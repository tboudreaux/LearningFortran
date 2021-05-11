       program main
              integer :: i
              character(32) :: arg
              write(*,*)"Hello, World!"
              do i = 1, iargc()
                     call getarg(i, arg)
                     write(*,*) arg
              end do

       end program main
