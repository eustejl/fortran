MODULE find_root
        IMPLICIT NONE
        CONTAINS

        REAL FUNCTION f(x) RESULT(y) ! function f(x)
            REAL :: x
            y = x**3 - 5*x
        END FUNCTION

        REAL FUNCTION df(x) RESULT(dy) ! first derivative
            REAL :: x
            dy = 3*x**2 - 5
        END FUNCTION

        
        SUBROUTINE newton(x0,eps,iter) ! find root by Newton-Raphson method
            REAL, INTENT(IN) :: eps
            REAL, INTENT(INOUT) :: x0
            INTEGER, INTENT(INOUT) :: iter
            REAL :: x1
            INTEGER :: ios

            OPEN(10,IOSTAT=ios,FILE='newton_out.txt',STATUS='replace',ACTION='write')

            WRITE(10,*)'#iteration_num,guess_value,df'
            IF (ios==0) THEN
                x1 = x0 - f(x0)/df(x0)
                WRITE(10,*) iter,x1,ABS(x0-x1)
                IF (ABS(x0-x1)>eps) THEN
                    DO
                        x0 = x1
                        IF (ABS(df(x0)) > TINY(x0)) THEN ! if 1st derivative not zero
                                x1 = x0 - f(x0)/df(x0)
                                iter = iter+1
                                WRITE(10,*) iter,x1,ABS(x0-x1)

                                IF (ABS(x0-x1)<=eps) EXIT ! if desired precision reached
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
         END SUBROUTINE
        
        SUBROUTINE secant(x0,x1,eps,iter)
            REAL,INTENT(INOUT) :: x0,x1
            REAL, INTENT(IN) :: eps
            INTEGER, INTENT(INOUT) :: iter
            REAL :: x2,dx
            INTEGER :: ios

            OPEN(20,IOSTAT=ios,FILE='secant_out_08.txt',STATUS='replace',ACTION='write')
            
            WRITE(20,*)'#iteration_num,guess_value,dx'

            IF ((ios==0).and.(ABS(f(x1)-f(x0))>TINY(x0))) THEN
                x2 = x1 - f(x1)*(x1-x0)/(f(x1)-f(x0))
                dx = ABS(x2-x1)
                WRITE(20,*) iter,x2,dx

                
                IF ((dx>=eps).and.(ABS(f(x1)-f(x0))>TINY(x0))) THEN
                    DO
                        x0 = x1
                        x1 = x2
                        x2 = x1 - f(x1)*(x1-x0)/(f(x1)-f(x0))
                        iter = iter+1
                        dx = ABS(x2-x1)
                        WRITE(20,*) iter,x2,dx

                        IF (dx<=eps) EXIT
                    END DO
                END IF

            ELSE
                CLOSE(20)
                PRINT*,'Error:file not opened'
            END IF
        END SUBROUTINE secant



END MODULE find_root




PROGRAM prob_2_euste
    USE find_root
    IMPLICIT NONE
    
    REAL :: a=1.2,b=2.8,eps=1e-10
    INTEGER :: iter_secant=1,iter_newton=1

    PRINT*,SQRT(5.)


    CALL secant(a,b,eps,iter_secant)

    a=1.75
    eps=1e-10
    CALL newton(a,eps,iter_newton)

    IF (iter_newton<iter_secant) PRINT*,'Newton method is faster.'
    IF (iter_newton>iter_secant) PRINT*,'Secant method is faster.'
    IF (iter_newton==iter_secant) PRINT*,'No difference between the two methods.'



END PROGRAM prob_2_euste
