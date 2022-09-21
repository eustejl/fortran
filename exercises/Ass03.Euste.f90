INTEGER(8) FUNCTION check_in(x,y) RESULT (ins)
      ! check if point is in (ins=1) or out (ins=0) of  the circle

      REAL(8), INTENT(IN) :: x, y ! coordinates
      REAL(8) :: r
      
      r = x**2 + y**2 ! Euclidean distance from the origin
      
      IF (r<1) THEN
              ins = 1 ! point inside the circle
      ELSE
              ins = 0
      END IF
END FUNCTION

SUBROUTINE count_in(m,pi,error)
        ! count number of points in circle
        ! then calculate pi
        ! then calculate abs error from trigonometric pi

        INTEGER(8), INTENT(IN) :: m
        INTEGER(8) :: ins
        INTEGER(8),EXTERNAL :: check_in
        REAL(8) :: x,y
        REAL(8), INTENT(out) :: pi,error

        ins = 0 ! add 1 to this variable each time random point in circle
        DO j=1,m
                CALL RANDOM_NUMBER(x) ! random x-coordinate
                CALL RANDOM_NUMBER(y) ! random y-coordinate

                ins = ins + check_in(x,y) ! add to count if inside circle

                pi = 4.0*ins/(1.0*m) ! calculate pi

                error = ABS(pi - 4.0*ATAN(1.0)) ! calculate abs error from trigonometric pi
        END DO
END SUBROUTINE



PROGRAM ass03_f90
      ! main program

      IMPLICIT NONE

      INTEGER(8) :: m ! variable for the total number of points
      REAL(8) :: pi,error


      m = 1 ! start with 1 point then continue incrementing until desired precision is achieved

      
      CALL count_in(m,pi,error) ! calculate pi and error using a subroutine above

      IF (error > 1.0E-4) THEN ! loop if error still greater than desired
        DO
                m = m+1 ! increment number of pts to randomize
                
                CALL count_in(m,pi,error) ! calculate pi and error using a subroutine above

                PRINT*,'total no. of pts=',m,'pts in circle=',pi,'error=',error
       
                IF (error <= 1.0E-4) THEN ! if error now less than or equal the desired, stop the loop
                        EXIT
                END IF
        END DO

      END IF

      PRINT*,'With',m,'random points, the estimate of pi as',pi,'deviates by less than 1.0E-4 from the value of trigonometric pi.'
      PRINT*,4.0*ATAN(1.0)
END PROGRAM ass03_f90
