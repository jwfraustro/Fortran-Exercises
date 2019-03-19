! Title: Fractional Knapsack
! Author: Joshua Fraustro
! Repository: https://github.com/jwfraustro/FortranUtilities
!
! Fortran 90 program to find the optimal weight / value loading for a knapsack.
!
! Input Specification:
! Filename: in.txt
! Line one contains an integer n specifying the number of items and
! integer w specifying the knapsack's maximum weight.
! The remaining lines consist of each items space separated weight and value.

program main
    implicit none

    type Item
        integer :: weight
        integer :: value
    end type Item

    type Knapsack
        integer :: max_weight
        type(Item), dimension(:), allocatable :: items
    end type Knapsack

    integer n, w, i
    type(Knapsack) :: sack

    open(file='in.txt', unit=10)
    read(10, *) n, w

    sack%max_weight = w
    allocate(sack%items(n))

    DO i=1, n
        read(10, *) sack%items(i)%weight, sack%items(i)%value
    END DO

    call sort_by_value_ratio(sack)
    call print_knapsack(sack)
    print *, compute_max_value(sack)

contains
    function value_ratio(this_item)
        type(Item) :: this_item
        real :: value_ratio

        value_ratio = real(this_item%value) / real(this_item%weight)

    end function value_ratio

    subroutine sort_by_value_ratio(sack)
        type(Knapsack) :: sack
        type(Item) :: temp
        integer :: num_pairs, last_swap, i

        num_pairs = size(sack%items) - 1

        DO
            IF (num_pairs == 0) exit

            last_swap = 1
            DO i = 1, num_pairs
                IF(value_ratio(sack%items(i))<value_ratio(sack%items(i + 1))) then
                    temp = sack%items(i)
                    sack%items(i) = sack%items(i + 1)
                    sack%items(i + 1) = temp

                    last_swap = i
                END IF
            END DO
            num_pairs = last_swap - 1
        END DO


    end subroutine sort_by_value_ratio

    subroutine print_knapsack(sack)
        type(Knapsack) :: sack
        integer :: i, w, v

        DO i = 1, size(sack%items)
            w = sack%items(i)%weight
            v = sack%items(i)%value
            print *, "Item", i, "Weight:", w, "Value:", v, "Value Ratio:", value_ratio(sack%items(i))
        END DO
    end subroutine print_knapsack

    function compute_max_value(sack)
        type(Knapsack) :: sack
        integer :: i, current_weight, remain_weight
        real :: final_value, compute_max_value

        current_weight = 0
        final_value = 0.0

        DO i=1, size(sack%items)
            if (current_weight+sack%items(i)%weight <= sack%max_weight) then
                current_weight = current_weight + sack%items(i)%weight
                final_value = final_value + sack%items(i)%value
            else
                remain_weight = sack%max_weight - current_weight
                final_value = final_value + sack%items(i)%value * (real(remain_weight) / sack%items(i)%weight)
                exit
            end if

        END DO

        compute_max_value = final_value
    end function compute_max_value
end program main