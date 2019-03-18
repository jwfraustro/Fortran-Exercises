! Title: Egyptian Fractions
! Author: Joshua Fraustro
! Repository: https://github.com/jwfraustro/FortranUtilities
!
! Fortran 90 program to print a fraction in its Egyptian (unit) form
! using Greedy Algorithm

program egypt_fractions
    implicit none
    integer :: nr, dr

    print *, "Egyptian Fractions"
    print *, "Enter your numerator:"
    read *, nr
    print *, "Enter your denominator:"
    read *, dr

    print '(A I1 A I1 A)', "The Egyptian fraction representation of ",nr,'/',dr,' is:'

    call compute(nr, dr)

contains

    subroutine compute(nr, dr)
        integer :: nr, dr, x, i
        integer, dimension(256) :: temp_list
        temp_list(:) = -1

        i = 1

        DO WHILE (nr /=0)
            x = CEILING(REAL(dr)/REAL(nr))
            temp_list(i) = x
            nr = x * nr - dr
            dr = dr * x
            i = i+1
        END DO

        DO i=1, SIZE(temp_list)
            if (temp_list(i) == -1) then
                exit
            end if

            write(*,'(A I3 A)', ADVANCE="NO") "1/", temp_list(i), "  "
        END DO

    end subroutine compute


end program egypt_fractions