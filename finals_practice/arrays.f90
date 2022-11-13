PROGRAM arrays
    IMPLICIT NONE

    REAL,DIMENSION(3) :: v1
    REAL,DIMENSION(2,4) :: arr
    REAL,DIMENSION(-1:1) :: v2
    INTEGER :: a = 72
    CHARACTER, DIMENSION(5) :: s1

    s1 = (/ 'H', 'e', 'l' ,'l', 'o' /)

    v1 = (/ 1.1, 1.2, 1.3 /)
    v2 = v1
    
    arr = 2
    !arr(1,2) = 5.5

    PRINT'(3f3.1 i2)',v1,a
    PRINT*,v1*v1
    PRINT*,arr(2,3)

    PRINT*,LBOUND(v2)
    PRINT*,UBOUND(v2)
    PRINT*,SIZE(v2)
    PRINT'(f4.2)',v2(0)

    PRINT*,s1(2)//'f'

END PROGRAM arrays
