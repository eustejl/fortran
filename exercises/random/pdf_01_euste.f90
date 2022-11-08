MODULE dens_est
        IMPLICIT NONE
        CONTAINS

        REAL FUNCTION normal(x,s,xi) RESULT(n)
            ! normal    
            REAL :: x,s,xi
            n = (1/(s*SQRT(2*4.0*ATAN(1.)))) * EXP(-((x-xi)/(4*s))**2)
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
             INTEGER :: i=1
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

    SUBROUTINE iqr(arr,cdf,iqr_val)
            ! interquartile range
            ! CDF(3/4) - CDF(1/4)
            REAL,DIMENSION(:),INTENT(IN) :: arr,cdf
            REAL,INTENT(OUT) :: iqr_val
            REAL :: eps=1e-5,fq,tq
            INTEGER :: i

            DO i=1,SIZE(arr) ! find 1st quartile
                IF (abs(cdf(i)-0.25)<eps) fq = arr(i)
                IF (abs(cdf(i)-0.75)<eps) THEN
                        tq = arr(i)
                        EXIT
                END IF
            END DO

            iqr_val = tq - fq

            PRINT*,'IQR=',iqr_val      
    END SUBROUTINE iqr

    SUBROUTINE freedman_diaconis(arr,cdf,n_bin,delta_x)
            !(Delta x)_prox = IQR*2/M**(1/3)
            !N_bin = floor((x_max - x_min)/ (Delta_x)_prox) + 1
            !(Delta x) = (x_max-x_min)/N_bin
            REAL,DIMENSION(:),INTENT(IN) :: arr,cdf
            REAL :: iqr_val,x_prox,x_max,x_min,delta_x
            INTEGER :: n_bin

            CALL iqr(arr,cdf,iqr_val)
            
            x_max = MAXVAL(arr)
            x_min = MINVAL(arr)
            x_prox = iqr_val*2/(SIZE(arr)**(1./3))
            PRINT*,'x_prox=',x_prox
            n_bin = FLOOR((x_max-x_min)/x_prox)+1
            PRINT*,'N_bins=',n_bin
            delta_x = (x_max-x_min)/n_bin
            PRINT*,'Delta x =',delta_x
    END SUBROUTINE freedman_diaconis


    SUBROUTINE kde(arr,cdf,p_kde)
        !kernel density estimation
        REAL,DIMENSION(:),INTENT(IN) :: arr,cdf
        REAL,DIMENSION(:),INTENT(INOUT) :: p_kde
        REAL :: ave,a,iqr_val,s,p
        INTEGER :: m,i,j

        m = SIZE(arr)
        ave = SUM(arr)/m
        a = ((SUM(arr-ave)**2.)/m)**(1./2) !std dev, change to iqr if smaller
        PRINT*,a
        CALL iqr(arr,cdf,iqr_val)
        
        IF ((iqr_val/1.34)<a) a=iqr_val/1.34
        PRINT*,a
        s = 0.9*a/(m**(1./5.)) !smoothing param       
        PRINT*,'s=',s
        
        DO i=1,SIZE(p_kde)
            p = 0
            DO j=1,m
                p = p + normal( -10. + (i-1)*20./SIZE(p_kde) , s, arr(j) )
            END DO
            p_kde(i) = p/m

        END DO            
        

    END SUBROUTINE kde


END MODULE dens_est









PROGRAM pdf_01
        USE dens_est
        IMPLICIT NONE
        INTEGER :: n,i,n_bin,j
        REAL,DIMENSION(:),ALLOCATABLE :: rand_array,cdf,hist,p_kde
        INTEGER,DIMENSION(:),ALLOCATABLE :: seed
        REAL :: delta_x,arr_min
        
        n=5000 !~ number of points

        ! initialize seed for reproducibility
        ALLOCATE(seed(n))
        seed = 42
        CALL RANDOM_SEED(put=seed)
        DEALLOCATE(seed)

        ALLOCATE(rand_array(n))
        ALLOCATE(cdf(n))

        CALL rejection(rand_array) !generate random numbers

        CALL sort_asc(rand_array,n) !sort in asc order

        ! generate CDF
        ! save sorted array to file
        OPEN(10,FILE='pdf1-cdf.txt',STATUS='replace',ACTION='write')
        DO i=1,n
                cdf(i) = i/(n*1.0)
                WRITE(10,*) rand_array(i),cdf(i)
        END DO
        CLOSE(10)

        !generate histogram
        CALL freedman_diaconis(rand_array,cdf,n_bin,delta_x)
        ALLOCATE(hist(n_bin))
        hist = 0 ! initiliaze histogram array of size n_bin to zero

        DO i=1,n
                j = FLOOR((rand_array(i)-MINVAL(rand_array))/delta_x)+1
                hist(j) = hist(j) + 1
        END DO

        ! save histogram to file
        arr_min = MINVAL(rand_array)
        OPEN(20,FILE='pdf1-hist.txt',STATUS='replace',ACTION='write')
        DO i=1,n_bin
                WRITE(20,*) arr_min+(i-0.5)*delta_x, hist(i)/(SUM(hist)*delta_x) !normalized hist
        END DO
        CLOSE(20)


        !kernel density estimation
        n = 10000
        ALLOCATE(p_kde(n))
        CALL kde(rand_array,cdf,p_kde)

        ! save kde to file
        OPEN(30,FILE='pdf1-kde.txt',STATUS='replace',ACTION='write')
        DO i=1,n
                WRITE(30,*) -10.+(i-1)*20./n ,p_kde(i)
        END DO
        CLOSE(30)

END PROGRAM pdf_01
