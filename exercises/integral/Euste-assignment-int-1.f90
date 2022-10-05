REAL FUNCTION f(x) RESULT(y)
      REAL,INTENT(IN) :: x
      y = exp(x)
END FUNCTION f

REAL FUNCTION area_rect(a,b,n) RESULT(area)
      REAL :: a,b
      REAL :: xk
      INTEGER :: n
      INTEGER :: k

      h = (b-a)/n

      area=0
      DO k=1,n
        xk = a + k*h
        area = area + f(xk)*h
      END DO        
END FUNCTION area_rect


PROGRAM riemann_integral
        IMPLICIT NONE
        REAL,EXTERNAL :: f,area_rect
        REAL :: a,b,eps

        PRINT*,'Enter lower then upper bound of the interval:'
        READ*,a,b
        PRINT*,'Enter precision, e.g. 1e-5:'
        READ*,eps

        PRINT*,f(1.0)

END PROGRAM riemann_integral
