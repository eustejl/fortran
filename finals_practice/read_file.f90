PROGRAM read_file_euste
    IMPLICIT NONE

    INTEGER :: i
    REAL :: x,y

    OPEN(10,FILE='sample.dat',STATUS='old',ACTION='read')
    DO i=1,2
        READ(10,*) x,y
        PRINT*, x,y
    END DO
END PROGRAM read_file_euste
