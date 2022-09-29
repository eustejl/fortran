MODULE stat





END MODULE stat




PROGRAM ass04_euste
        IMPLICIT NONE
        INTEGER :: i,j,u,ios,arrdim
        REAL(16),DIMENSION(492) :: tp
        REAL(16),DIMENSION(492) :: tm
        CHARACTER,DIMENSION(20) :: s1
        CHARACTER,DIMENSION(20) :: s2

        u=10
        OPEN(UNIT=u,IOSTAT=ios,FILE='numerical.dat',STATUS='old',ACTION='read')
        PRINT*,u,ios

        arrdim = 5
        IF (ios ==0) THEN
                DO i = 4,495+(4-1)
                        j = i-3
                        READ (u, '(5x,f7.5,3x,f9.5)') tp(j),tm(j)
                END DO
                CLOSE(u)
        ELSE
                PRINT '(a25)','Error: file not opened'
        END IF

        PRINT*, tm

END PROGRAM ass04_euste


