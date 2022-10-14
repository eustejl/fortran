MODULE methods
        IMPLICIT NONE
        CONTAINS

        REAL FUNCTION f(t,x) RESULT(xp)
                ! dx/dt = f(t,x(t))
                REAL,INTENT(IN) :: t,x
                xp = x
        END FUNCTION f


        SUBROUTINE euler(x0,n,h,x_eul)
                REAL, INTENT(INOUT) :: x0
                REAL :: xi1,ti,h,t0
                INTEGER :: n,i
                REAL,DIMENSION(n) :: x_eul
                
                t0 = 0 ! initially ti=0
                DO i=0,N-1
                        ti = t0  + i*h
                        xi1 = x0 + h*f(ti,x0)
                        x0 = xi1
                        x_eul(i+1) = xi1
                        PRINT*,xi1,EXP(ti),ti
                END DO

         END SUBROUTINE euler



END MODULE methods






PROGRAM diff_eq1
        USE methods

        IMPLICIT NONE

        REAL :: x0=1,h
        INTEGER :: n
        REAL,DIMENSION(:),ALLOCATABLE :: x_exp,x_eul

        n = 100
        ALLOCATE(x_exp(n))
        ALLOCATE(x_eul(n))

        CALL euler(x0,n,0.0001,x_eul)
        
        PRINT*,x_eul





END PROGRAM diff_eq1
