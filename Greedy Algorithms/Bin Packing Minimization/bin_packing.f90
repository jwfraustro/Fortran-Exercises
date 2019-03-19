! Title: Bin Packing Minimization
! Author: Joshua Fraustro
! Language: Fortran 90
! Repository: https://github.com/jwfraustro/Fortran-Exercises
!
! Given n items of different weights and bins of capacity c, minimize the total bins
! need for all items.
!
! Input Specification: 'in.txt'
! Weights < c
! First Line: n - total number of items
! Second Line: space separated values representing the item weights
! Third Line: c - bin capacity

program bin_packing
    implicit none
    integer, dimension(:), allocatable :: items
    integer :: c, n

    open(file='in.txt', unit=10)
    read(10, *) n
    allocate(items(n))
    read(10, *) items(:)
    read(10, *) c

    call next_fit(items, n, c)

contains

    subroutine next_fit(items, n, c)
        integer, dimension(*) :: items
        integer :: n,c, bin_remain, result,i

        result = 0
        bin_remain = c

        DO i = 1,n
            if (items(i) > bin_remain) then
                result = result + 1
                bin_remain = c - items(i)
            else
                bin_remain = bin_remain - items(i)
            end if
        END DO

        print *, "Bins needed using Next Fit method:", result


    end subroutine next_fit

end program bin_packing

