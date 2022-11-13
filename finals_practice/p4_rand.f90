MODULE generate_random
        IMPLICIT NONE
        CONTAINS

        REAL FUNCTION normal(x,s,xi) RESULT(n)
            ! normal    
            REAL :: x,s,xi
            n = (1/(s*SQRT(2*4.0*ATAN(1.)))) * EXP(-((x-xi)/(4*s))**2)
        END FUNCTION

        SUBROUTINE boxmuller(n,sigma,mu,arr)
             ! generate Gaussian random variables x & y using Box-Muller method   
             INTEGER :: n,i
             REAL :: u,up,r,x,y,theta,sigma,mu
             REAL,DIMENSION(:) :: arr           
   


             DO i=1,n
                CALL RANDOM_NUMBER(u)
                CALL RANDOM_NUMBER(up)

                r = SQRT((-2*sigma**2)*LOG(1-u))
                theta = 2*4*ATAN(1.0)*up

                x = r*SIN(theta) + mu
                y = r*COS(theta) + mu
                arr(i) = x
                WRITE(30,*) arr(i)
             END DO
             CLOSE(30)
        END SUBROUTINE boxmuller


    SUBROUTINE sort_asc(arr,m)
            ! sort array arr of size m in ascending order
            INTEGER, INTENT(IN) :: m
            REAL,DIMENSION(m),INTENT(INOUT) :: arr
            REAL :: aa ! placeholder for swapping array elements
            INTEGER :: i,j ! counters       


            DO i=1,m ! iterate over all elements
               DO j=i+1,m ! compare with each succeeding element
               IF (arr(i)>arr(j)) THEN ! swap with next element if next element smaller
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
        a = (SUM((arr-ave)**2.)/m)**(1./2) !std dev, change to iqr if smaller
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


END MODULE generate_random






PROGRAM prob_4_euste
    USE generate_random
    IMPLICIT NONE

    INTEGER,PARAMETER :: j=10000,k=3000,l=7000
    INTEGER :: i,n_bin,q
    REAL :: delta_x,arr_min
    REAL,DIMENSION(j) :: arr
    REAL,DIMENSION(k) :: arr1
    REAL,DIMENSION(l):: arr2
    REAL,DIMENSION(j) :: cdf
    REAL,DIMENSION(:),ALLOCATABLE :: hist

    

    CALL boxmuller(k,0.2,-0.5,arr1)
    CALL boxmuller(l,0.5,1.,arr2)

    OPEN(30,FILE='random.dat',STATUS='replace',ACTION='write')



    DO i=1,j
        IF (i<=k) THEN
            arr(i) = arr1(i)
        ELSE
            arr(i) = arr2(i-k)
        END IF
        WRITE(30,*) arr(i)
    END DO
    CLOSE(30)



    CALL sort_asc(arr,j) !sort in asc order

    ! generate CDF
    ! save sorted array to file
    OPEN(10,FILE='cumul.dat',STATUS='replace',ACTION='write')
    DO i=1,j
            cdf(i) = i/(j*1.0)
            WRITE(10,*) arr(i),cdf(i)
    END DO
    CLOSE(10)



    !generate histogram
    CALL freedman_diaconis(arr,cdf,n_bin,delta_x)
    ALLOCATE(hist(n_bin))
    hist = 0 ! initiliaze histogram array of size n_bin to zero

    DO i=1,j
            q = FLOOR((arr(i)-MINVAL(arr))/delta_x)+1
            hist(q) = hist(q) + 1
    END DO

    ! save histogram to file
    arr_min = MINVAL(arr)
    OPEN(20,FILE='histogram.dat',STATUS='replace',ACTION='write')
    DO i=1,n_bin
            WRITE(20,*) arr_min+(i-0.5)*delta_x, hist(i)/(SUM(hist)*delta_x) !normalized hist
    END DO
    CLOSE(20)
      



END PROGRAM prob_4_euste
