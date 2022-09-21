MODULE sort
    IMPLICIT NONE

    CONTAINS
        
    SUBROUTINE sort_asc(arr,m)
            ! sort array arr of size m in ascending order
            INTEGER, INTENT(IN) :: m
            REAL(8),DIMENSION(m),INTENT(INOUT) :: arr
            REAL(8) :: aa ! placeholder for swapping array elements
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


    SUBROUTINE sort_desc(arr,m)
            ! sort array arr of size m in descending order
            INTEGER, INTENT(IN) :: m
            REAL(8),DIMENSION(m),INTENT(INOUT) :: arr
            REAL(8) :: aa ! placeholder for swapping array elements
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
    END SUBROUTINE sort_desc



END MODULE sort


PROGRAM ass04_euste

            USE sort
            IMPLICIT NONE
            INTEGER :: m,i,j
            REAL(8),DIMENSION(:),ALLOCATABLE :: arr
            REAL(8) :: x 

            PRINT*, 'Enter dimension of array'
            READ*,m ! ask user for the dimension of array

            ALLOCATE(arr(m)) ! allocate array  dimension 

            DO i=1,m ! generate an array of random numbers with dimension m
                CALL RANDOM_NUMBER(x)
                arr(i) = x
            END DO

            PRINT*,'Random array generated'
            PRINT*,arr
            

            CALL sort_asc(arr,m)
            PRINT*,'Sorted in ascending order'
            PRINT*,arr

            CALL sort_desc(arr,m)
            PRINT*,'Sorted in ascending order'
            PRINT*,arr


END PROGRAM ass04_euste
