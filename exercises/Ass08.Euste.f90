MODULE find_root
        IMPLICIT NONE
        CONTAINS

        REAL FUNCTION f(x) RESULT(y) ! function f(x)
            REAL :: x
            y = 2*exp(x) - 2*(x**2) - 4
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
                WRITE(10,*) x1
                IF (ABS(x0-x1)>eps) THEN
                    DO
                        x0 = x1
                        IF (ABS(df(x0)) > TINY(x0)) THEN ! if 1st derivative not zero
                                x1 = x0 - f(x0)/df(x0)
                                iter = iter+1
                                WRITE(10,*) x1

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
        
        SUBROUTINE secant(x0,x1,eps)
            REAL,INTENT(INOUT) :: x0,x1
            REAL, INTENT(IN) :: eps
            REAL :: x2
            INTEGER :: ios,iter

            OPEN(20,IOSTAT=ios,FILE='secant_out_08.txt',STATUS='replace',ACTION='write')
            
            WRITE(20,*)'#root estimate,    iteration step'
            IF ((ios==0).and.(ABS(f(x1)-f(x0))>TINY(x0))) THEN
                x2 = x1 - f(x1)*(x1-x0)/(f(x1)-f(x0))
                iter = 1
                WRITE(20,*) x2,iter

                IF ((ABS(x2-x1)>=eps).and.(ABS(f(x1)-f(x0))>TINY(x0))) THEN
                    DO
                        x0 = x1
                        x1 = x2
                        x2 = x1 - f(x1)*(x1-x0)/(f(x1)-f(x0))
                        iter = iter+1
                        WRITE(20,*) x2,iter

                        IF (ABS(x2-x1)<eps) EXIT
                    END DO
                END IF

            ELSE
                CLOSE(20)
                PRINT*,'Error:file not opened'
            END IF
            PRINT*,x2,iter ! print root and number of iterations
        END SUBROUTINE secant



        SUBROUTINE bisection(a,b,eps) ! find root by bisection method
            REAL, INTENT(INOUT) :: a,b
            REAL,INTENT(IN) :: eps
            REAL  :: c
            INTEGER :: ios,iter

            OPEN(10,IOSTAT=ios,FILE='bisect_out_08.txt',STATUS='replace',ACTION='write')

            WRITE(10,*)'#root estimate,    iteration step'    
            IF (ios==0) THEN
                c = (a+b)/2
                iter = 1
                WRITE(10,*) c,iter

                IF (b-c>eps) THEN
                    DO
                        IF (sign(1.0,f(b))*sign(1.0,f(c))<=0) THEN
                            a=c
                        ELSE
                            b=c
                        END IF

                        c = (a+b)/2
                        iter = iter+1
                        WRITE(10,*) c,iter

                        IF (b-c<=eps) EXIT
                    END DO
                END IF

            ELSE
                CLOSE(10)
                PRINT*,'Error:file not opened'
            END IF
            PRINT*,c,iter ! print root and number of iterations
         END SUBROUTINE bisection
END MODULE find_root


PROGRAM ass06_euste
        USE find_root
        IMPLICIT NONE
        
        REAL :: x0=0.0,x1=0.0,eps=0.0 ! set first to zero, will be changed by user later
        REAL :: a,b

        IF (x1<=x0) THEN ! ask user for bounds of initial estimate
                DO
                        PRINT*,'Enter lower bound of initial estimate (x_0)'
                        READ*,x0
                        PRINT*,'Enter upper bound of initial estimate (x_1)'
                        READ*,x1
                IF (x1>x0) EXIT
                END DO
        END IF

        ! store initial estimates in another variable
        ! because x0 and x1 will be changed when
        ! a subroutine is called
        a = x0
        b = x1

        IF (eps<=0) THEN ! ask user for positive precision value
                DO
                        PRINT*,'Enter the error tolerance (must be positive, e.g. 1E-7)'
                        READ*,eps
                IF (eps>0) EXIT
                END DO
        END IF             
        

        PRINT*,'secant method: (root and number of iterations)'
        CALL secant(x0,x1,eps)


        ! find root by bisection method
        PRINT*,'bisection method: (root and number of iterations)'
        CALL bisection(a,b,eps)
END PROGRAM ass06_euste
