! Title: Fitting Shelves
! Author: Joshua Fraustro
! Repository: https://github.com/jwfraustro/FortranUtilities
!
! Fortran 90 program to find the maximum number of shelves of a two lengths, prefering the larger sizes.
!
! Input Specification:
! Three integers representing available length, small and large shelf size

program fitting_shelves
    implicit none
    integer :: avail_space, small_size, large_size

    print *, "Fitting Shelves"
    read(*,*) avail_space, small_size, large_size
    call fit_shelves(avail_space, small_size, large_size)

contains
    subroutine fit_shelves(avail_space, small_size, large_size)
        integer :: avail_space, small_size, large_size
        integer :: num_s, num_l, rem
        integer :: s, l, r

        num_s = 0
        num_l = 0
        rem = avail_space

        s = 0
        l = 0
        r = 0

        DO WHILE (avail_space >= large_size)
            l = l+ 1
            avail_space = avail_space - large_size
            s = avail_space / small_size
            r = MOD(avail_space, small_size)
            if (r<=rem) then
                num_s = s
                num_l = l
                rem = r
            end if
        END DO

        print *, num_s, num_l, rem
    end subroutine fit_shelves
end program fitting_shelves