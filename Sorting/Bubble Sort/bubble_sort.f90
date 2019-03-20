! Title: Bubble Sort
! Author: Joshua Fraustro
! Language: Fortran 95
! Repository: https://github.com/jwfraustro/Fortran-Exercises
!
! A demonstration of the Bubble Sort method using Fortran 95.

program bubble_sort
    implicit none
    integer, dimension(10) :: arr

    arr = (/20, 12, 16, 6, 2, 8, 1, 7, 13, 17/)

    print *, 'Unsorted Array'
    print *, arr

    call sort(arr)

    print *, 'Sorted Array'
    print *, arr

contains

    subroutine sort(arr)
        integer, dimension(10) :: arr
        integer :: i, n, max
        logical :: swapped

        n = size(arr)
        DO WHILE (n > 1)
            max = 0
            DO i = 1, (n-1)
                if (arr(i)>arr(i+1)) then
                    call swap(arr, i, i+1)
                    max = i
                end if
            END DO
            n = max
        END DO

    end subroutine sort

    subroutine swap(arr, i, j)
        integer, dimension(*) :: arr
        integer :: i, j, temp

        temp = arr(i)
        arr(i) = arr(j)
        arr(j) = temp

    end subroutine swap

end program bubble_sort