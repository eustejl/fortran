MODULE methods
        IMPLICIT NONE
        CONTAINS

        REAL FUNCTION f(t,x) RESULT(xp)
                ! dx/dt = f(t,x(t))
                REAL,INTENT(IN) :: t,x
                xp = x
        END FUNCTION f


        SUBROUTINE euler(x0,n,h,x_eul)
                REAL,INTENT(INOUT) :: x0
                REAL :: xi1,ti,h,t0
                INTEGER :: n,i
                REAL,DIMENSION(n) :: x_eul

                OPEN(10,FILE='int3-euler.txt',STATUS='replace',ACTION='write')

                WRITE(10,*) '#x_euler, x_exp, t'
                t0 = 0 ! initially ti=0
                WRITE(10,*) x0,EXP(t0),t0
                DO i=0,N-1
                        ti = t0  + i*h
                        xi1 = x0 + h*f(ti,x0)
                        x0 = xi1
                        x_eul(i+1) = xi1
                        WRITE(10,*) xi1,EXP(ti),ti
                END DO
                CLOSE(10)

         END SUBROUTINE euler


        SUBROUTINE midpoint(x0,n,h,x_eul)
                REAL, INTENT(INOUT) :: x0
                REAL :: xi1,ti,h,t0
                INTEGER :: n,i
                REAL,DIMENSION(n) :: x_eul

                OPEN(20,FILE='int3-midpoint.txt',STATUS='replace',ACTION='write')

                WRITE(20,*) '#x_midpoint, x_exp, t'
                t0 = 0 ! initially ti=0
                WRITE(20,*) x0,EXP(t0),t0
                DO i=0,N-1
                        ti = t0  + i*h/2
                        xi1 = x0 + (h/2)*f(ti,x0)
                        x0 = xi1
                        x_eul(i+1) = xi1
                        WRITE(20,*) xi1,EXP(ti),ti
                END DO
                CLOSE(20)

         END SUBROUTINE midpoint




END MODULE methods






PROGRAM diff_eq1
        USE methods

        IMPLICIT NONE

        REAL :: x0=1,h
        INTEGER :: n
        REAL,DIMENSION(:),ALLOCATABLE :: x_exp,x_eul,x_midpoint

        n = 100
        ALLOCATE(x_exp(n))
        ALLOCATE(x_eul(n))
        ALLOCATE(x_midpoint(n))

        CALL euler(x0,n,0.0001,x_eul)
        
        x0=1
        n = 100
        CALL midpoint(x0,n,0.0001,x_midpoint)





END PROGRAM diff_eq1
