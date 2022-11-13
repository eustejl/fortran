MODULE num_int
    IMPLICIT NONE
    CONTAINS


    REAL FUNCTION f(x) RESULT(y)
          REAL,INTENT(IN) :: x
          y = SIN(x**2)
    END FUNCTION f


    REAL FUNCTION area_trap(a,b,n) RESULT(area)
          ! sum of the areas of trapezoids
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


    REAL FUNCTION int_trap(a,b) RESULT(i)
        REAL :: a,b,diff,eps=1e-3
        INTEGER :: n1 = 1000 ! initial number of rectangles
        INTEGER :: n2


        n2 = 2*n1 ! increase by 2 times for each iteration


        diff = abs(area_trap(a,b,n1) - area_trap(a,b,n2))

        OPEN(10,FILE='int2.txt',STATUS='replace',ACTION='write')
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

         i = area_trap(a,b,n2)
         WRITE (10,*) area_trap(a,b,n2),n2
    END FUNCTION int_trap

END MODULE num_int


PROGRAM prob_3_euste
    USE num_int
    IMPLICIT NONE

    REAL :: a=0, b=5

    PRINT*,int_trap(a,b)



END PROGRAM prob_3_euste
