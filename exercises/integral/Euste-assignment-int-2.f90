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

    REAL FUNCTION area_trap(a,b,n) RESULT(area)
          ! trapezoid  method
          REAL :: a,b
          REAL :: xk,xl,h
          INTEGER :: n,k

          h = (b-a)/n

          area=0
          xk=a
          DO k=1,n
            xl = a + k*h
            area = area + (f(xk)+f(xl))*h/2
            xk = xl
          END DO
    END FUNCTION area_trap



    REAL FUNCTION area_simp(a,b,n) RESULT(area)
            ! simpson method
          REAL,INTENT(INOUT) :: a,b
          REAL :: xk,h
          INTEGER :: n,k

          h = (b-a)/n

          area=f(a)
          DO k=1,n,2 ! odd
            xk = a + k*h
            area = area + 4*f(xk)
          END DO


          DO k=2,n,2 ! even
            xk = a + k*h
            area = area + 2*f(xk)
          END DO

          area=(h/3)*(area + f(b))

    END FUNCTION area_simp




END MODULE num_int


PROGRAM riemann_integral
        USE num_int
        IMPLICIT NONE
        REAL :: a,b,eps,diff,a2,b2
        INTEGER :: n1,n2

        PRINT*,'Enter lower then upper bound of the interval:'
        READ*,a2,b2
        PRINT*,'Enter error tolerance, e.g. 1e-5:'
        READ*,eps
        
        ! show expected integral for reference
        ! antiderivative of f(x)=exp(x) is still f(x)
        ! don't use if function is changed
        PRINT*,'Expected value of integral from analytical calculation',f(b2)-f(a2)


        n1 = 100 ! initial number of rectangles
        n2 = 2*n1 ! increase by 2 times for each iteration

        a = a2
        b = b2
        diff = abs(area_rect(a,b,n1) - area_rect(a,b,n2))


        OPEN(10,FILE='int2.txt',STATUS='replace',ACTION='write')
        WRITE (10,*) '#midpoint method'
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

         PRINT*,'Integral using midpoint',area_rect(a,b,n2)




        n1 = 100 ! initial number of rectangles
        n2 = 2*n1 ! increase by 2 times for each iteration


        a = a2
        b = b2
        diff = abs(area_trap(a,b,n1) - area_trap(a,b,n2))

        WRITE (10,*) '#trapezoid  method'
        WRITE (10,*) area_trap(a,b,n1),n1
        WRITE (10,*) area_trap(a,b,n2),n2
        IF (diff>eps) THEN
            DO
                n1 = n2
                n2 = 2*n1

                diff = abs(area_trap(a,b,n1) - area_trap(a,b,n2))
                WRITE (10,*) area_trap(a,b,n2),n2

                IF (diff<=eps) EXIT
            END DO
         END IF

         WRITE (10,*) area_trap(a,b,n2),n2


        PRINT*,'Integral using trapezoid method',area_trap(a,b,n2)




        n1 = 100 ! initial number of rectangles
        n2 = 2*n1 ! increase by 2 times for each iteration


        a = a2
        b = b2
        diff = abs(area_simp(a,b,n1) - area_simp(a,b,n2))

        WRITE (10,*) '#simpson  method'
        WRITE (10,*) area_simp(a,b,n1),n1
        WRITE (10,*) area_simp(a,b,n2),n2
        IF (diff>eps) THEN
            DO
                n1 = n2
                n2 = 2*n1

                diff = abs(area_simp(a,b,n1) - area_simp(a,b,n2))
                WRITE (10,*) area_simp(a,b,n2),n2

                IF (diff<=eps) EXIT
            END DO
         END IF

         WRITE (10,*) area_simp(a,b,n2),n2

        CLOSE(10)

        PRINT*,'Integral using Simpson method',area_simp(a,b,n2)


       

END PROGRAM riemann_integral
