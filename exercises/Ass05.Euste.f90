MODULE stat
    IMPLICIT NONE
    CONTAINS

    REAL(8) FUNCTION mean(arr) RESULT(ave)
        ! calculate average value of arr
        REAL(8),DIMENSION(:),INTENT(IN) :: arr

        ave = SUM(arr)/SIZE(arr)
    END FUNCTION mean

END MODULE stat


PROGRAM ass04_euste
    USE stat
    IMPLICIT NONE
    INTEGER :: i,u=10,ios,nh,nl
    REAL(8),DIMENSION(:),ALLOCATABLE :: prec, temp
    CHARACTER(13) :: file_name='numerical.dat'
    CHARACTER(1) :: rand ! will be used to identify header

    ! count the number of lines
    OPEN(UNIT=u,IOSTAT=ios,FILE=file_name,STATUS='old',ACTION='read')

    nh = 0 ! counter variable for number of header lines, i.e. starting with #
    nl = 0 ! counter variable for number of lines for the data
    DO
        ! read a line and save 1st character of line to variable rand
        READ(u,*,IOSTAT=ios) rand

        IF (ios/=0) THEN 
            ! close file and exit loop when end of file is reached
            CLOSE(u)
            EXIT
        END IF

        IF (rand=='#') THEN
            nh=nh+1 ! count as header
        ELSE
            nl=nl+1 ! count as line with data
        END IF
    END DO

    PRINT*,'Number of header lines:',nh
    PRINT*,'Number of lines with data:',nl


    ! now that we know the number of lines with data,
    ! open the file again
    OPEN(UNIT=u,IOSTAT=ios,FILE=file_name,STATUS='old',ACTION='read')

    ALLOCATE(prec(nl)) ! allocate space for array to store precipitation
    ALLOCATE(temp(nl)) ! allocate space for array to store temperature

    DO i=1,nh
        READ (u,*) ! skip header lines
    END DO        
    
    IF (ios==0) THEN
        DO i=1,nl
                READ (u, '(5x,f7.5,3x,f9.5)') prec(i),temp(i)
        END DO
        CLOSE(u)
    ELSE
        PRINT '(a25)','Error: file not opened'
    END IF

    PRINT*, 'Mean of precipitation (mm/h):'
    PRINT '(f7.5)', mean(prec)
    PRINT*, 'Mean of temperature (K):'
    PRINT '(f9.5)', mean(temp)

END PROGRAM ass04_euste