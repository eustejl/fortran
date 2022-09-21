PROGRAM fibprog1
      IMPLICIT NONE
      INTEGER :: x,y,z
      REAL :: phi_est
      INTEGER ::  n = 3

      z = 1
      y = 1

      2 x = y + z

      ! golden ratio
      phi_est = x / (1.0 * y) ! multiplied to 1.0 to make it REAL

      PRINT*, x, phi_est

      z = y
      y = x

      n = n + 1

      IF (n<50) GO TO 2
END PROGRAM fibprog1
