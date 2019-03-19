! Title: Traveling Salesman
! Author: Joshua Fraustro
! Language: Fortran 90
! Repository: https://github.com/jwfraustro/Fortran-Exercises
!
! An implementation of Traveling Salesman Problem in Fortran 90
! TODO

program salesman
    implicit none
    integer :: n, i, start
    integer, dimension(:,:), allocatable :: cities

    open(file='in.txt', unit=10)
    read(10, *) n, start
    allocate(cities(n,n))
    DO i = 1, n
        read(10, *) cities(i, :)
        print *, cities(i,:)
    END DO

    call travel(cities,start, n)

contains
    subroutine travel(cities, start, n)
        integer, dimension(*) :: cities
        integer, dimension(n-1) :: visited, unvisited
        integer :: n, start, min_path, current_path, i

        visited(1) = start

        DO i = 1, n
            if (i /= start) then

            end if
        END DO

        DO i


    end subroutine travel

end program salesman