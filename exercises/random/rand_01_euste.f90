MODULE generate_random
        IMPLICIT NONE
        CONTAINS

        REAL FUNCTION f(x) RESULT(y)
            ! function f(x), f(x)d(x) probability of producing r.n. between x and x+dx
            REAL :: x
            y = 3*x**2
        END FUNCTION

        REAL FUNCTION G_inv(x) RESULT(u)
             ! x = G(u) -> inverse is u = G^-1(x) = integral f(x')dx' from x_min to x
             REAL :: x
             u = x**3
        END FUNCTION

        REAL FUNCTION G(u) RESULT(x)
             ! x = G(u) -> CDF
             REAL :: u
             x = u**(1./3)
        END FUNCTION

        SUBROUTINE transform(rand_array)
             ! inverse transform method for generating random number from a distribution f(x)
             ! f(x) -> u=G^-1(x)=int_xmin^x{f(x')dx' -> x=G(u)
             ! u is a random number [0,1) from uniform distribution
             REAL, DIMENSION(:),INTENT(INOUT) :: rand_array
             INTEGER :: i

             OPEN(10,FILE='rand1-transform.txt',STATUS='replace',ACTION='write')

             DO i=1,SIZE(rand_array)
                     rand_array(i) = G(rand_array(i))
                     WRITE(10,*)rand_array(i)
             END DO
             CLOSE(10)
        END SUBROUTINE transform


        SUBROUTINE boxmuller(n)
             ! generate Gaussian random variables x & y using Box-Muller method   
             INTEGER :: n,i
             REAL :: u,up,r,sigma=1,x,y,theta
                
             OPEN(30,FILE='rand1-bm.txt',STATUS='replace',ACTION='write')

             DO i=1,n
                CALL RANDOM_NUMBER(u)
                CALL RANDOM_NUMBER(up)

                r = SQRT((-2*sigma**2)*LOG(1-u))
                theta = 2*4*ATAN(1.0)*up

                x = r*SIN(theta)
                y = r*COS(theta)
                WRITE(30,*) x,y
             END DO
             CLOSE(30)
        END SUBROUTINE boxmuller

                
        SUBROUTINE rejection(rand_array)
             ! rejection method for generating random number from a distribution f(x)
             ! generate random number u uniformly distributed in [xmin,xmax)
             ! generate another rand num r in [0,1)
             ! if r < f(u)/fmax, accept x
             REAL, DIMENSION(:),INTENT(INOUT) :: rand_array
             REAL :: fmax,u,r
             INTEGER :: i=1,j
             
             fmax = 3*(1.)**2 !max of f(x) in [xmin,xmax)=[0,1)
             
             OPEN(20,FILE='rand1-rej.txt',STATUS='replace',ACTION='write')

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



PROGRAM rand_1 !generate 10000 rand pts [0,1) using box-muller & rejection
        USE generate_random
        IMPLICIT NONE
        INTEGER :: n=10000 ! number of random points to generate
        INTEGER,ALLOCATABLE,DIMENSION(:) :: seed
        REAL,ALLOCATABLE,DIMENSION(:) :: rand_array


        ! initialize seed for reproducibility
        ALLOCATE(seed(n))
        seed = 42
        CALL RANDOM_SEED(put=seed)
        DEALLOCATE(seed)

        ! generate array of random numbers from uniform distribution
        ALLOCATE(rand_array(n))
        CALL RANDOM_NUMBER(rand_array)

        CALL transform(rand_array)

        CALL boxmuller(n)

        CALL rejection(rand_array)

END PROGRAM rand_1
