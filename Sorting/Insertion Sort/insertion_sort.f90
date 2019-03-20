! Title: Insertion Sort
! Author: Joshua Fraustro
! Language: Fortran 95
! Repository: https://github.com/jwfraustro/Fortran-Exercises
!
! A demonstration of the Insertion Sort method using Fortran 95.

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
        integer :: i, val, pos

        DO i = 1, size(arr)
            val = arr(i)
            pos = i

            DO WHILE (pos > 0 .and. arr(pos - 1) > val)
                arr(pos) = arr(pos-1)
                pos = pos -1
            END DO
            arr(pos) = val
        END DO

    end subroutine sort
end program insertion_sort