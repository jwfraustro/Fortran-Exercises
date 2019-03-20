! Title: Selection Sort
! Author: Joshua Fraustro
! Language: Fortran 95
! Repository: https://github.com/jwfraustro/Fortran-Exercises
!
! A demonstration of the Selection Sort method using Fortran 95.

program insertion_sort
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
        integer i, j, min

        DO i = 1, size(arr)
            min = i
            DO j = i+1, size(arr)
                if (arr(j) < arr(min)) then
                    min = j
                end if
            END DO
            call swap(arr, min, i)
        END DO

    end subroutine sort

    subroutine swap(arr, i, j)
        integer, dimension(*) :: arr
        integer :: i, j, temp

        temp = arr(i)
        arr(i) = arr(j)
        arr(j) = temp

    end subroutine swap

end program insertion_sort