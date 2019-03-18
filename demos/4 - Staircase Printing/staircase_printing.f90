! Title: Staircase Printing
! Author: Joshua Fraustro
! Repository: https://github.com/jwfraustro/FortranUtilities
!
! Given an integer n, this program will print a right-aligned
! staircase of # and spaces whose height and base are equal to n.
!
! Input Specifications:
! A single integer, n, denoting the size of the staircase.
! 0 < n <= 100

program staircase_printing
    implicit none
    integer :: n

    print *, "4 - Staircase Printing"
    read (*, *) n

    call print_staircase(n)

contains
    subroutine print_staircase(n)
        integer n, i
        character, dimension(n) :: row

        DO i=1, n
            row(:) = "#"
            row(1:n-i) = " "
            print*, row
        END DO

    end subroutine print_staircase
end program staircase_printing