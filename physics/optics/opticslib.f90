!#####################################
!Small module containing Fortran utilities for optical physics
!#####################################

module opticslib
    implicit none

contains

    function deg_to_rad(deg)
        real :: deg, deg_to_rad

        deg_to_rad = deg * 3.14159 / 180

    end function deg_to_rad

    function rad_to_deg(rad)
        real :: rad, rad_to_deg

        rad_to_deg = rad * 180 / 3.14159

    end function rad_to_deg

    function lens_formula(f, o, i)
        real :: lens_formula
        real, optional :: f, o, i

        if (present(f) .and. present(o)) then
            lens_formula = 1/((1/f) - (1/o))
        else if (present(f) .and. present(i)) then
            lens_formula = 1/((1/f)-(1/i))
        else if (present(o) .and. present(i)) then
            lens_formula = 1/((1/o)+(1/i))
        end if
    end function lens_formula

    function snells_law(n1, n2, theta1, theta2)
        real :: snells_law
        real, optional :: n1, n2, theta1, theta2

        if (present(n1) .and. present(n2) .and. present(theta1)) then
            snells_law = rad_to_deg(ASIN((n1/n2)*SIN(deg_to_rad(theta1))))
        else if (present(n1) .and. present(n2) .and. present(theta2)) then
            snells_law = rad_to_deg(ASIN((1/((n1/n2)/SIN(deg_to_rad(theta2))))))
        else if (present(n1) .and. present(theta1) .and. present(theta2)) then
            snells_law = (1/(SIN(deg_to_rad(theta2))/SIN(deg_to_rad(theta1)))/n1)
        else if (present(n2) .and. present(theta1) .and. present(theta2)) then
            snells_law = (SIN(deg_to_rad(theta2))/SIN(deg_to_rad(theta1)))*n2
        end if

    end function snells_law

    function beam_displacement(n1, n2, theta, h)
        real, intent(in) :: n1, n2, theta, h
        real :: beam_displacement, rad_theta

        rad_theta = deg_to_rad(theta)

        beam_displacement = h*SIN(rad_theta)*(1-(COS(rad_theta)/SQRT((n2/n1)**2 - SIN(rad_theta)**2)))

    end function beam_displacement


end module opticslib