MODULE find_root
        IMPLICIT NONE
        CONTAINS

        REAL FUNCTION f(x) RESULT(y) ! function f(x)
            REAL :: x
            y = 2*exp(x) - 2*(x**3) - 3
        END FUNCTION

        REAL FUNCTION df(x) RESULT(dy) ! first derivative
            REAL :: x
            dy = 2*exp(x) - 6*x**2
        END FUNCTION

        
        SUBROUTINE newton(x0,eps) ! find root by Newton-Raphson method
            REAL, INTENT(IN) :: eps
            REAL, INTENT(INOUT) :: x0
            REAL :: x1
            INTEGER :: ios,iter

            OPEN(10,IOSTAT=ios,FILE='newton_out.txt',STATUS='replace',ACTION='write')

            IF (ios==0) THEN
                x1 = x0 - f(x0)/df(x0)
                iter = 1
                WRITE(10,'(f11.8)') x1
                IF (ABS(x0-x1)>eps) THEN
                    DO
                        x0 = x1
                        IF (ABS(df(x0)) > TINY(x0)) THEN ! if 1st derivative not zero
                                x1 = x0 - f(x0)/df(x0)
                                iter = iter+1
                                WRITE(10,'(f11.8)') x1

                                IF (ABS(x0-x1)<eps) EXIT ! if desired precision reached
                        ELSE
                                PRINT*,'Algorithm failure: first derivative = 0.'
                                EXIT
                        END IF
                    END DO
                END IF  
            ELSE
                CLOSE(10)
                PRINT*,'Error:file not opened'
            END IF
            PRINT*,x1,iter ! print root and number of iterations
         END SUBROUTINE 

END MODULE find_root


PROGRAM ass06_euste
        USE find_root
        IMPLICIT NONE
        
        REAL :: x0=0.0,eps=0.0 ! set first to zero, will be changed by user later

        IF (x0>=0) THEN ! ask user for negative initial guess
                DO
                        PRINT*,'Enter an initial guess (must be negative)'
                        READ*,x0
                IF (x0<0) EXIT
                END DO
        END IF

        IF (eps<=0) THEN ! ask user for positive precision value
                DO
                        PRINT*,'Enter the error tolerance (must be positive, e.g. 1E-7)'
                        READ*,eps
                IF (eps>0) EXIT
                END DO
        END IF             
        
        ! find root by Newton-Raphson method
        PRINT*,'Newton-Raphson method: (root and number of iterations)'
        CALL newton(x0,eps)
END PROGRAM ass06_euste
