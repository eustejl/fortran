!PROGRAM myfirstprogram
!        IMPLICIT NONE
!
!        REAL :: x,y,z
!
!       x = 5.1
!        y = -17.2
!        z = x*y
!
!       PRINT*, 'The product of x and y is',z
!
!END PROGRAM myfirstprogram


PROGRAM myfirstprogram
        IMPLICIT NONE

        REAL :: x,y,z

        PRINT*, 'Enter first number:'

        READ*, x

        PRINT*, 'Enter second number:'

        READ*, y

        z = x*y

        PRINT*, 'The product of x and y is',z

END PROGRAM myfirstprogram


