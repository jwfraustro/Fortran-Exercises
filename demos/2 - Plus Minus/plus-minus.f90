! Title: Plus Minus
! Author: Joshua Fraustro
! Repository: https://github.com/jwfraustro/FortranUtilities
!
! Given an integer array, this program calculates the fraction
! of its elements that are positive, negative, or zeros.
!
! Input Specifications:
! File: array.txt
! Line 1 contains a single integer n, denoting the size of the array.
! in matrix arr.
! Line2  consists of n space-separated integers describing the array.

program plus_minus
    implicit none
    integer, dimension(:), allocatable :: arr
    integer n

    print *, "2 - Plus Minus"
    open(file='array.txt', unit=10)
    read (10, *) n

    allocate(arr(n))

    read(10, *) arr(:)
    close(10)
    call calc(arr, n)
    deallocate(arr)
contains
    subroutine calc(arr,n)
        implicit none
        integer, dimension(n) :: arr
        integer n, i, pos, neg, nil

        pos = 0
        neg = 0
        nil = 0

        DO i=1, n
            if (arr(i) < 0) then
                neg = neg + 1
            else if (arr(i) > 0 )then
                pos = pos + 1
            else
                nil = nil + 1
            end if
        END DO

        print *, "Positive:", real(pos)/n
        print *, "Negative:", real(neg)/n
        print *, "Zero:", real(nil)/n

    end subroutine calc

end program plus_minus
