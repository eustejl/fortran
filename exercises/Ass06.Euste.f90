MODULE find_root
        IMPLICIT NONE
        CONTAINS

        REAL FUNCTION f(x) RESULT(y) ! function f(x)
            REAL :: x
            y = 2*exp(x) - 2*(x**2) - 3
        END FUNCTION
        
        SUBROUTINE bisection(a,b) ! find root by bisection method
            REAL, INTENT(IN) :: a,b           
            REAL  :: c,eps=1.0E-7
            INTEGER :: ios,iter

            OPEN(10,IOSTAT=ios,FILE='bisect_out.txt',STATUS='replace',ACTION='write')
              

            IF (ios==0) THEN
                c = (a+b)/2
                iter = 1
                WRITE(20,'(f9.7)') c

                IF (b-c>eps) THEN
                    DO
                        IF (sign(1.0,f(b))*sign(1.0,f(c))<=0) THEN
                            a=c
                        ELSE
                            b=c
                        END IF

                        c = (a+b)/2
                        iter = iter+1
                        WRITE(10,'(f9.7)') c
                               
                        IF (b-c<=eps) EXIT
                    END DO
                END IF     

            ELSE
                CLOSE(10)
                PRINT*,'Error:file not opened'
            END IF
            PRINT '(f9.7 i10)',c,iter ! print root and number of iterations
         END SUBROUTINE bisection

         SUBROUTINE false_position(a,b) ! find root by false position
            REAL, INTENT(IN) :: a,b
            REAL  :: c,eps=1.0E-7
            INTEGER :: ios,iter

            OPEN(20,IOSTAT=ios,FILE='false_pos_out.txt',STATUS='replace',ACTION='write')

            IF (ios==0) THEN
                c = (f(b)*a-f(a)*b)/(f(b)-f(a))
                iter = 1
                WRITE(20,'(f11.8)') c

                IF (b-c>eps) THEN
                    DO
                        IF (sign(1.0,f(b))*sign(1.0,f(c))<=0) THEN
                            a=c
                        ELSE
                            b=c
                        END IF

                        c = (f(b)*a-f(a)*b)/(f(b)-f(a))
                        iter = iter+1
                        WRITE(20,'(f11.8)') c

                        IF (b-c<=eps) EXIT
                    END DO
                END IF
            ELSE
                CLOSE(20)
                PRINT*,'Error:file not opened'
            END IF
            PRINT '(f9.7 i10)',c,iter ! print root and number of iterations
        END SUBROUTINE false_position
END MODULE find_root


PROGRAM ass06_euste
        USE find_root
        IMPLICIT NONE
        
        REAL :: a, b

        a = -2 ! lower bound of interval
        b = 2 ! upper bound of interval

        PRINT*,'Bisection method: (root and number of iterations)'
        CALL bisection(a,b)
        PRINT*,'False position method: (root and number of iterations)'
        CALL false_position(a,b)

END PROGRAM ass06_euste
