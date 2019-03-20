! Title: Merge Sort
! Author: Joshua Fraustro
! Language: Fortran 95
! Repository: https://github.com/jwfraustro/Fortran-Exercises
!
! A demonstration of the Merge Sort method using Fortran 95.

program merge_sort
    implicit none
    integer, dimension(10) :: arr
    integer, dimension((size(arr)+1)/2) :: arr2

    arr = (/20, 12, 16, 6, 2, 8, 1, 7, 13, 17/)

    print *, 'Unsorted Array'
    print *, arr

    call sort(arr, size(arr), arr2)

    print *, 'Sorted Array'
    print *, arr

contains

    recursive subroutine sort(arr, n, arr2)
        integer, dimension(10) :: arr
        integer, dimension((n+1)/2) :: arr2
        integer :: n
        integer :: n_a, n_b, swap

        if (n < 2) then
            return
        end if
        if (n == 2) then
            if (arr(1) > arr(2)) then
                swap = arr(1)
                arr(1) = arr(2)
                arr(2) = swap
            end if
            return
        end if

        n_a = (n+1)/2
        n_b = n - n_a

        call sort(arr, n_a, arr2)
        call sort(arr(n_a+1), n_b, arr2)

        if (arr(n_a) > arr(n_a+1)) then
            arr2(1:n_a) = arr(1:n_a)
            call merge(arr2, n_a, arr(n_a+1), n_b, arr, n)
        end if
        return

    end subroutine sort

    subroutine merge(arr_a, n_a, arr_b, n_b, arr, n)
        integer :: n_a, n_b, n
        integer :: arr_a(n_a), arr_b(n_b), arr(n)
        integer :: i, j, k

        i = 1
        j = 1
        k = 1

        DO WHILE(i<=n_a .and. j <= n_b)
            IF (arr_a(i) <= arr_b(j)) then
                arr(k) = arr_a(i)
                i = i +1
            else
                arr(k) = arr_b(j)
                j = j+1
            END IF
            k = k +1
        END DO
        DO WHILE(i <= n_a)
            arr(k) = arr_a(i)
            i = i +1
            k = k +1
        END DO
    end subroutine merge


end program merge_sort