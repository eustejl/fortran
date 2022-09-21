PROGRAM basic_io
      IMPLICIT NONE
      REAL :: X

      PRINT*, 'Enter a real number:'

      READ*,x

      PRINT*,'You have entered the number ',x

      PRINT*,'The square of',x,' is ', x*x
END PROGRAM basic_io
