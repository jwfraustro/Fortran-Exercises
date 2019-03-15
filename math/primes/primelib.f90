!################################
!Author: Joshua Fraustro
!Github Library: https : //github.com/jwfraustro/FortranUtilities
!Created on Fri Mar 15 15:56:32 2019
!
!Some quick functions for dealing with prime numbers.
!#################################
!       UNFINISHED
!#################################

module primelib
    implicit none
    public is_prime

contains
    function is_prime(number)
        integer :: number, i, root
        logical :: is_prime

        is_prime = .true.

        number = ABS(number)

        if (number <= 1) then
            is_prime = .false.
            return
        end if

        root = SQRT(REAL(number))

        DO i=2, root
            if (MOD(number, i) == 0) then
                is_prime = .false.
                return
            end if
        END DO
    end function is_prime

    function sieve_er(n)
        !returns highest prime <= integer n
        integer :: n, mid, mult, i, j
        logical, dimension(n) :: A

        A(1:n) = .True.

        mid = n/2

        DO i = 2, mid
            IF (A(i)) THEN
                mult = n / i
                DO j = 2, mult
                    A(i * j) = .FALSE.
                END DO
            END IF
        END DO

        i = n
        DO WHILE(.NOT.A(i))
            i = i - 1
        END DO
        DEALLOCATE(A)

        sieverEr = i

    end function sieve_er

end module primelib