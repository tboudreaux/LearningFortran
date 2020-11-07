      subroutine subTest(input, output)
	  
	  parameter(const=2)

	  if (input.le.5)then
		 output = const
      else
		 output = 0
	  endif 


 	  write(*,*)"Called!"

	  end
