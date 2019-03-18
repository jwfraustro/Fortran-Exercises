! Title: Mini Max
! Author: Joshua Fraustro
! Repository: https://github.com/jwfraustro/FortranUtilities
!
! Given five positive integers, this program outputs
! the minimum and maximum values that can be calculated by
! summing exactly four of the five integers.
!
! Input Specifications:
! A single line of five space-separated integers.

program mini_max
    implicit none
    integer, dimension(5) :: arr

    print *, "3 - Mini Max"
    read(*, *) arr(:)

    call miniMaxSum(arr)

contains
    subroutine miniMaxSum(arr)
        integer, dimension(5) :: arr
        integer i, j, max, min, val

        min = 10e+8
        max = -1

        DO i = 1, 5
            val = 0
            DO j=1, 5
                if (i == j) then
                    cycle
                else
                    val = val + arr(j)
                end if
            END DO
            if (val < min) then
                min = val
            end if
            if (val > max) then
                max = val
            end if
        END DO

        print *, "Max:", max
        print *, "Min:", min
    end subroutine miniMaxSum
end program mini_max