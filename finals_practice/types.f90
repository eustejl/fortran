PROGRAM types
    IMPLICIT NONE

    INTEGER,PARAMETER :: ki = SELECTED_INT_KIND(12)
    INTEGER(ki) :: x, y=1, z=1
    
    INTEGER,PARAMETER :: kr = SELECTED_REAL_KIND(16)
    REAL(kr) :: phi_est

    INTEGER :: n

    PRINT*,ki,kr

    DO n=3,50
        x = y + z
        phi_est = x/(1.0_kr*y)

        PRINT*, x, phi_est

        z=y
        y=x
    END DO



END PROGRAM types
