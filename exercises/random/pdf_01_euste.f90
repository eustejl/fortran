MODULE dens_est
        IMPLICIT NONE
        CONTAINS

        REAL FUNCTION normal(x,s,xi) RESULT(n)
            ! normal    
            REAL :: x,s,xi
            n = (1/(s*SQRT(2*4.0*ATAN(1.)))) * EXP(-0.5*((x-xi)/2.)**2)    
        END FUNCTION

        REAL FUNCTION f(x) RESULT(y)
            ! function f(x), f(x)d(x) probability of producing r.n. between x and x+dx
            !15x^2 N(x,0.25,-0.5) + 13N(x,0.3,-1.5) + 7N(x,1.,3.)
            REAL :: x
            y = 15*(x**2)*normal(x,0.25,-0.5) + 13*normal(x,0.3,-1.5) + 7*normal(x,1.,3.)
        END FUNCTION

        SUBROUTINE rejection(rand_array)
             ! rejection method for generating random number from a distribution f(x)
             ! generate random number u uniformly distributed in [xmin,xmax)
             ! generate another rand num r in [0,1)
             ! if r < f(u)/fmax, accept x
             REAL, DIMENSION(:),INTENT(INOUT) :: rand_array
             REAL :: fmax,u,r
             INTEGER :: i=1,j
             REAL :: xmin=-10,xmax=10

             fmax = 17.5 !max of f(x) in [xmin,xmax)=[-10,10)

             DO
                CALL RANDOM_NUMBER(u)
                CALL RANDOM_NUMBER(r)

                u = xmin + (xmax-xmin)*u !re-map u to desired interval

                IF (r<f(u)/fmax) THEN
                       rand_array(i) = u
                       i=i+1
                END IF
                IF (i==SIZE(rand_array)) EXIT 
             END DO
        END SUBROUTINE rejection





    SUBROUTINE sort_asc(arr,m)
            ! sort array arr of size m in ascending order
            INTEGER, INTENT(IN) :: m
            REAL,DIMENSION(m),INTENT(INOUT) :: arr
            REAL :: aa ! placeholder for swapping array elements
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
    END SUBROUTINE sort_asc

END MODULE dens_est









PROGRAM pdf_01
        USE dens_est
        IMPLICIT NONE
        INTEGER :: n,i
        REAL,DIMENSION(:),ALLOCATABLE :: rand_array
        INTEGER,DIMENSION(:),ALLOCATABLE :: seed
        
        n=5000 !~ number of points

        ! initialize seed for reproducibility
        ALLOCATE(seed(n))
        seed = 42
        CALL RANDOM_SEED(put=seed)
        DEALLOCATE(seed)

        ALLOCATE(rand_array(n))

        CALL rejection(rand_array) !generate random numbers

        CALL sort_asc(rand_array,n) !sort in asc order

        ! save sorted array to file
        OPEN(20,FILE='pdf1-cdf.txt',STATUS='replace',ACTION='write')
        DO i=1,n
                WRITE(20,*) rand_array(i),i/(n*1.0)
        END DO
        CLOSE(20)



END PROGRAM pdf_01
