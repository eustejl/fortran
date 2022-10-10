MODULE others
    IMPLICIT NONE
    CONTAINS

    REAL FUNCTION f(x,ave) RESULT(t)
        REAL :: x,ave
        INTEGER :: pi
        
        pi = 4.0*ATAN(1.0)
        t = 10.0*SIN(pi*(x*1.0 - 109.5)/183) + 12.2 - ave
    END FUNCTION



    SUBROUTINE bisection(a,b,ave) ! find root by bisection method
        REAL,INTENT(INOUT) :: a,b,ave
        REAL  :: c,eps=0.5   

        c = (a+b)/2
        IF (ABS(b-c)>eps) THEN
            DO
                IF (sign(1.0,f(b,ave))*sign(1.0,f(c,ave))<=0) THEN
                    a=c
                ELSE
                    b=c
                END IF

                c = (a+b)/2
                

                IF (ABS(b-c)<=eps) EXIT
            END DO
        END IF

        PRINT*,c ! print root and number of iterations
     END SUBROUTINE bisection


    SUBROUTINE sort_asc(arr,m)
            INTEGER, INTENT(IN) :: m
            REAL,DIMENSION(m),INTENT(INOUT) :: arr
            REAL:: aa ! placeholder for swapping array elements
            INTEGER :: i,j ! counters       


            DO i=1,m ! iterate over all elements
               DO j=i+1,m ! compare with each succeeding element
               IF (arr(i)>arr(j)) THEN ! swap with the next element if next element is smaller
                       aa = arr(j)
                       arr(j) = arr(i)
                       arr(i) = aa
               END IF
               END DO
            END DO
            PRINT*,'MAX',arr(m)
    END SUBROUTINE sort_asc



    SUBROUTINE sort_desc(arr,m)
            INTEGER, INTENT(IN) :: m
            REAL,DIMENSION(m),INTENT(INOUT) :: arr
            REAL :: aa ! placeholder for swapping array elements
            INTEGER :: i,j ! counters       


            DO i=1,m ! iterate over all elements
               DO j=i+1,m ! compare with each succeeding element
               IF (arr(i)<arr(j)) THEN ! swap with the next element if next element is larger
                       aa = arr(j)
                       arr(j) = arr(i)
                       arr(i) = aa
               END IF
               END DO
            END DO
            PRINT*,'MIN',arr(m)
    END SUBROUTINE sort_desc
END MODULE others









PROGRAM midterm_euste
    USE others
    IMPLICIT NONE
 
    INTEGER,DIMENSION(365) :: day
    REAL,DIMENSION(365) :: temp,temp_fxn,temp_adj,temp2
    REAL :: ave_temp,a,b
    INTEGER :: u,ios,n,i

    u = 10
    OPEN(UNIT=u,FILE='temperature_2010_Trieste.dat',IOSTAT=ios,STATUS='old',ACTION='read')

    IF (ios ==0) THEN
        DO i=1,365
                READ (u, '(i3,1x,f4.1)') day(i),temp(i)
        END DO
        CLOSE(u)
    ELSE
        PRINT '(a25)','Error: file not opened'
    END IF


    n = SIZE(temp)
    temp2 = temp
    CALL sort_asc(temp2,n)

    CALL sort_desc(temp2,n)
    ave_temp = SUM(temp)/n
    PRINT*,'AVERAGE',ave_temp

    ! temp calculated from function T(x)
    ! not yet shifted by average
    DO i=1,365
        temp_fxn(i) = f(day(i)*1.0,0.0)
    END DO

    ! temp shifted by average
    temp_adj = temp_fxn - ave_temp
    
    ! april starts at x=91 and ends at x=121
    a = 91.0
    b = 121.0
    CALL bisection(a,b,ave_temp)

    ! october
    a = 121.0+31.0+30.0+31.0+31.0+30.0+1.0
    b = a + 31.0
    CALL bisection(a,b,ave_temp)



    

END PROGRAM midterm_euste
