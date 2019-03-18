! Title: Fractional Knapsack
! Author: Joshua Fraustro
! Repository: https://github.com/jwfraustro/FortranUtilities
!
! Fortran 90 program to find the optimal weight / value loading for a knapsack.
! TODO

program knapsack
    implicit none
    integer :: max_weight
    integer, dimension(3,2) :: items

    items(1,:) = (/60,20/)
    items(2,:) = (/100, 30/)
    items(3,:) = (/140, 40/)

    print *, items(1,1), items(1,2)

contains

    subroutine sort_by_value_ratio(items)
        integer, dimension(3,2) :: items
        integer, dimension(2) :: temp
        integer :: i, j, max

        !todo

    end subroutine sort_by_value_ratio


end program knapsack