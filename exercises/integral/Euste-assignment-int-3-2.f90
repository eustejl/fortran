MODULE methods
        IMPLICIT NONE
        CONTAINS

        REAL FUNCTION fx(t,x,y,a,b) RESULT(xt)
                ! dx/dt = ax-bxy
                REAL,INTENT(IN) :: t,x,y,a,b
                xt = a*x - b*x*y
        END FUNCTION fx


        REAL FUNCTION fy(t,x,y,d,g) RESULT(yt)
                ! dy/dt = dxy-gy
                REAL,INTENT(IN) :: t,x,y,d,g
                yt = d*x*y- g*y
        END FUNCTION fy




        SUBROUTINE midpoint(x0,y0,n,h,x_arr,y_arr,a,b,d,g)
                ! midpoint euler method
                ! input: init cond,num_steps,width_steps,func_arrays,func_param
                REAL, INTENT(INOUT) :: x0,y0
                REAL :: xi1,yi1,ti,h,t0,a,b,d,g
                INTEGER :: n,i
                REAL,DIMENSION(n) :: x_arr,y_arr

                OPEN(20,FILE='int3-lv.txt',STATUS='replace',ACTION='write')

                WRITE(20,*) '#t,x(t),y(t)'
                t0 = 0 ! initially ti=0
                WRITE(20,*) t0,x0,y0
                DO i=0,N-1
                        ti = t0  + i*h/2

                        xi1 = x0 + (h/2)*fx(ti,x0,y0,a,b)
                        x0 = xi1
                        x_arr(i+1) = xi1

                        yi1 = y0 + (h/2)*fy(ti,x0,y0,d,g)
                        y0 = yi1
                        y_arr(i+1) = yi1


                        WRITE(20,*) ti,xi1,yi1
                END DO
                CLOSE(20)

         END SUBROUTINE midpoint




END MODULE methods






PROGRAM diff_eq2
        USE methods

        IMPLICIT NONE

        REAL :: x0,y0,h,a,b,d,g
        INTEGER :: n
        REAL,DIMENSION(:),ALLOCATABLE :: x_arr,y_arr

        h = 0.1


        x0 = 1.0
        y0 = 0.6
        n = 100
        a = 2
        b = 1
        d = 1
        g = 1

        ALLOCATE(x_arr(n))
        ALLOCATE(y_arr(n))

        CALL midpoint(x0,y0,n,h,x_arr,y_arr,a,b,d,g)

        CALL EXECUTE_COMMAND_LINE('gnuplot -p ' // 'plot-lv.plt')




END PROGRAM diff_eq2
