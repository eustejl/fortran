! main program

PROGRAM ass03_f90

      IMPLICIT NONE

      INTEGER :: n
      INTEGER(8) :: m ! variable for the total number of points
      REAL(8) :: pi, error, average

      average = 0.0
      DO n = 1 , 100
            ! calculate pi and error using a subroutine above
            CALL count_in(m,pi,error)
            PRINT*,'total no. of pts=',m,'pts in circle=',pi,'error=',error
            average = average + m/100.0
      END DO

      PRINT*, 'Average = ', int(average)

      CONTAINS

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

            INTEGER(8), INTENT(OUT) :: m
            INTEGER(8) :: ins
            REAL(8) :: x, y, rpi
            REAL(8), INTENT(out) :: pi, error

            ins = 0 ! add 1 to this variable each time random point in circle
            m = 0
            rpi = 4.0*ATAN(1.0)
            DO
                m = m + 1
                CALL RANDOM_NUMBER(x) ! random x-coordinate
                CALL RANDOM_NUMBER(y) ! random y-coordinate
                ins = ins + check_in(x,y) ! add to count if inside circle
                pi = (4.0*ins)/m ! calculate pi
                ! calculate abs error from trigonometric pi
                error = ABS(pi - rpi)
                if ( error < 1.0E-4 ) exit
                if ( m > 1000000000 ) then
                    print *, 'No convergence...'
                    exit
                end if
            END DO
      END SUBROUTINE

END PROGRAM ass03_f90
