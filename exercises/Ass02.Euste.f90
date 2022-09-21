PROGRAM Ass02_Euste

      IMPLICIT NONE

      INTEGER :: m,j
      REAL :: pi=0.0 ! variable for estimate of pi
      REAL :: abs_diff


      ! read an integer greater than 1
      PRINT*, 'Enter integer greater than 1:'
      READ*, m
      
      ! check if integer is indeed greater than 1, if not, ask again
      IF (m <= 1) THEN ! if input is not an integer greater than 1
              DO
              PRINT*, 'Please input an INTEGER GREATER THAN 1'
              READ*, m
              IF (m > 1) THEN ! if input is now an integer greater than1 
                      EXIT
              END IF
              END DO
      END IF


      ! estimate pi by 4*sum (-1)^j/(2j+1) from j=0 to j=m
      DO j=0, m ! start from j=0, do operation until j reaches m
        pi = pi + 4 * ( ((-1)**j) / (2.0*j + 1) )
      END DO 
        
      abs_diff = abs( pi - 4*atan(1.0) ) ! absolute difference of the estimate and actual

      PRINT*,'The value of pi for m=', m, 'is', pi, 'which differs by', abs_diff, 'from the actual value.'


      ! find m so that absolute difference is less than 1.0E-2
      IF (abs_diff>1.0E-2) THEN ! if abs diff is greater than 1, keep incrementing m until abs diff is less than 1.0E-2
        DO
                
                m = m+1
             
                
                pi = 0.0
                DO j=0, m ! start from j=0, do operation until j reaches m
                        pi = pi + 4 * ( ((-1)**j) / (2.0*j + 1) )
                END DO

                abs_diff = abs( pi - 4*atan(1.0) ) ! absolute difference of the estimate and actual
                
                IF (abs_diff<1.0E-2) THEN ! if abs diff is now lower than 1.0E-2
                        PRINT*,'To reach an absolute difference less than 1.0E-2, the value of m must at least be',m
                        EXIT
                END IF
        END DO
      ELSE
              ! otherwise the value of m given by user already yields an abs diff lower than 1.0E-2
              PRINT*,'The absolute difference is already less than 1.0E-2.'
      END IF

END PROGRAM Ass02_Euste
