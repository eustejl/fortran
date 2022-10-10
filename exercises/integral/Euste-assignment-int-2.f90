MODULE num_int
    IMPLICIT NONE
    CONTAINS


    REAL FUNCTION f(x) RESULT(y)
          REAL,INTENT(IN) :: x
          y = exp(x)
    END FUNCTION f

    REAL FUNCTION area_rect(a,b,n) RESULT(area)
          ! midpoint method
          REAL :: a,b
          REAL :: xk,xl,h
          INTEGER :: n,k

          h = (b-a)/n

          area=0
          xk=a
          DO k=1,n
            xl = a + k*h
            area = area + f((xk+xl)/2)*h
            xk = xl
          END DO        
    END FUNCTION area_rect

END MODULE num_int


PROGRAM riemann_integral
        USE num_int
        IMPLICIT NONE
        REAL :: a,b,eps,diff
        INTEGER :: n1,n2

        PRINT*,'Enter lower then upper bound of the interval:'
        READ*,a,b
        PRINT*,'Enter error tolerance, e.g. 1e-5:'
        READ*,eps
        
        ! show expected integral for reference
        ! antiderivative of f(x)=exp(x) is still f(x)
        ! don't use if function is changed
        PRINT*,'Expected value of integral from analytical calculation',f(b)-f(a)


        n1 = 500 ! initial number of rectangles
        n2 = 2*n1 ! increase by 2 times for each iteration

        diff = abs(area_rect(a,b,n1) - area_rect(a,b,n2))

        OPEN(10,FILE='int2.txt',STATUS='replace',ACTION='write')
        WRITE (10,*) area_rect(a,b,n1),n1
        WRITE (10,*) area_rect(a,b,n2),n2
        IF (diff>eps) THEN
            DO
                n1 = n2
                n2 = 2*n1

                diff = abs(area_rect(a,b,n1) - area_rect(a,b,n2))
                WRITE (10,*) area_rect(a,b,n2),n2

                IF (diff<=eps) EXIT
            END DO
         END IF

         WRITE (10,*) area_rect(a,b,n2),n2
         CLOSE (10)

         PRINT*,'Integral using midpoint',area_rect(a,b,n2)

END PROGRAM riemann_integral
