! Title: Diagonal Difference
! Author: Joshua Fraustro
! Repository: https://github.com/jwfraustro/FortranUtilities
!
! Given a square matrix, this program calculates the absolute difference
! between the sum of its diagonals and outputs the result to stdout.
!
! Input Specifications:
! File: array.txt
! Line 1 contains a single integer n, the number of rows and columns
! in matrix arr.
! Remaining n lines describe each row, and consist of n space-separated integers.

program diagonal_difference
    implicit none
    integer, dimension(:,:), allocatable :: arr
    integer :: n, i, j

    print *, "Diagonal Difference"

    open(file='array.txt', unit=10)
    read (10, *) n

    allocate(arr(n,n))

    DO i=1, n
        read(10, *) arr(i,:)
    END DO
    close(10)

    DO i=1, n
        DO j=1, n
            write(*, "(I5)", ADVANCE = "NO") arr(i, j)
        END DO
        write(*,*)
    END DO

    call compute_difference(arr, n)

    deallocate(arr)

contains

    subroutine compute_difference(arr, n)
        integer, dimension(n,n) :: arr
        integer n, i, j, diag_1, diag_2

        diag_1 = 0
        diag_2 = 0

        DO i = 1, n
            diag_1 = diag_1 + arr(i,i)
            diag_2 = diag_2 + arr(i, n+1-i)
        END DO

        print *, "Result: ",abs(diag_1-diag_2)

    end subroutine compute_difference

end program diagonal_difference