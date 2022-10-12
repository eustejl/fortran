MODULE methods
        IMPLICITY NONE
        CONTAINS

        REAL FUNCTION f(t,x) RESULT xp
                ! dx/dt = f(t,x(t))
                REAL, INTENT(IN) :: t,x
                xp = x
        END FUNCTION f
END MODULE methods






PROGRAM diff_eq_1
        USE methods

        IMPLICIT NONE









END PROGRAM diff_eq1
