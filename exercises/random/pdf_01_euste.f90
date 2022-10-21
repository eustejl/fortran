MODULE generate_random
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
             
             fmax = 17.5 !max of f(x) in [xmin,xmax)=[10,-10)
             
             OPEN(20,FILE='pdf1-rej.txt',STATUS='replace',ACTION='write')

             DO
                CALL RANDOM_NUMBER(u)
                CALL RANDOM_NUMBER(r)

                IF (r<f(u)/fmax) THEN
                       rand_array(i) = u
                       WRITE(20,*) rand_array(i)
                       i=i+1
                END IF
                IF (i==SIZE(rand_array)) EXIT 
             END DO
             CLOSE(20)
        END SUBROUTINE rejection


END MODULE generate_random












PROGRAM pdf_01
        USE generate_random
        IMPLICIT NONE
        INTEGER :: n
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
END PROGRAM pdf_01
