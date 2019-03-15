module vec2d
    implicit none
    real :: rad_to_deg = 3.141 / 180

    type Vector_2D
        real :: x, y
    end type Vector_2D
contains
    function length(this)
        type(Vector_2D), intent(in) :: this
        real :: length
        length = SQRT(this%x**2 + this%y**2)
    end function length

    subroutine print_length(this)
        type(Vector_2D), intent(in) :: this
        real :: length_result
        length_result = length(this)
        print *, 'Length:', length_result
    end subroutine print_length

    subroutine print(this)
        type(Vector_2D), intent(in) :: this
        print *, 'X:', this%x, 'Y:', this%y
    end subroutine print

    function equal(this, other)
        type(Vector_2D), intent(in) :: this, other
        logical equal
        if (this%x == other%x .and. this%y == other%y) then
            equal = .true.
        else
            equal = .false.
        end if
    end function equal

    function not_equal(this, other)
        type(Vector_2D), intent(in) :: this, other
        logical not_equal
        if (this%x /= other%x .or. this%y /= other%y) then
            not_equal = .true.
        else
            not_equal = .false.
        end if
    end function not_equal

    function add(this, other)
        type(Vector_2D), intent(in) :: this, other
        type(Vector_2D) :: add

        add = Vector_2D(this%x + other%x, this%y + other%y)

    end function add

    function subtract(this, other)
        type(Vector_2D), intent(in) :: this, other
        type(Vector_2D) :: subtract

        subtract = Vector_2D(this%x - other%x, this%y - other%y)

    end function subtract

    function mul(this, other)
        type(Vector_2D), intent(in) :: this, other
        type(Vector_2D) :: mul

        mul = Vector_2D(this%x*other%x, this%y*other%y)

    end function mul

    function neg(this)
        type(Vector_2D), intent(in) :: this
        type(Vector_2D) :: neg

        neg = Vector_2D(-this%x, -this%y)

    end function neg

    function pos(this)
        type(Vector_2D), intent(in) :: this
        type(Vector_2D) :: pos

        pos = Vector_2D(ABS(this%x), ABS(this%y))
    end function pos

    function length_squared(this)
        type(Vector_2D), intent(in) :: this
        real :: length_squared

        length_squared = length(this)**2

    end function length_squared

    subroutine set_length(this, value)
        type(Vector_2D):: this
        real, intent(in) :: value
        this%x = this%x * (value / length(this))
        this%y = this%y * (value / length(this))
    end subroutine set_length

    subroutine rotate(this, angle)
        type(Vector_2D) :: this
        real, intent(in) :: angle

        this%x = this%x * COS(angle)*rad_to_deg - this%y * SIN(angle) * rad_to_deg
        this%y = this%x * SIN(angle) * rad_to_deg + this%y *COS(angle) * rad_to_deg

    end subroutine rotate

    function rotated(this, angle)
        type(Vector_2D), intent(in) :: this
        type(Vector_2D) :: rotated
        real, intent(in) :: angle
        real :: x, y

        x = this%x * COS(angle) * rad_to_deg - this%y * SIN(angle) * rad_to_deg
        y = this%x * SIN(angle) * rad_to_deg + this%y *COS(angle) * rad_to_deg

        rotated = Vector_2D(x, y)
    end function rotated

    function get_angle(this)
        type(Vector_2D), intent(in) :: this
        real :: get_angle

        if (length_squared(this) == 0) then
            get_angle = 0
        else
            get_angle = ATAN(this%y, this%x) * rad_to_deg
        end if

    end function get_angle

    subroutine set_angle(this, angle)
        type(Vector_2D):: this
        real :: angle

        this%x = length(this)
        this%y = 0

        call rotate(this, angle)

    end subroutine set_angle

    function angle_between(this, other)
        type(Vector_2D), intent(in) :: this, other
        real :: angle_between, cross, dot

        cross = this%x * other%y - this%y * other%x
        dot = this%x * other%x + this%y * other%y

        angle_between = ATAN2(cross, dot)

    end function angle_between

    function normalized(this)
        type(Vector_2D), intent(in) :: this
        type(Vector_2D) :: normalized
        real :: length_val

        length_val = length(this)

        if (length_val /= 0) then
            normalized = Vector_2D(this%x/length_val, this%y/length_val)
        else
            normalized = this
        end if

    end function normalized

    function perpendicular(this)
        type(Vector_2D), intent(in) :: this
        type(Vector_2D) :: perpendicular

        perpendicular = Vector_2D(-this%x, this%y)
    end function perpendicular

    function perpendicular_normal(this)
        type(Vector_2D), intent(in) :: this
        type(Vector_2D) :: perpendicular_normal
        real :: length_val

        length_val = length(this)
        if (length_val /= 0) then
            perpendicular_normal = Vector_2D(-this%y / length_val, this%x / length_val)
        else
            perpendicular_normal = this
        end if
    end function perpendicular_normal

    function dot(this, other)
        type(Vector_2D), intent(in) :: this,other
        real :: dot

        dot = this%x * other%x + this%y * other%y

    end function dot

    function get_distance(this, other)
        type(Vector_2D), intent(in) :: this, other
        real :: get_distance

        get_distance = SQRT((this%x - other%x)**2 + (this%y - other%y))
    end function get_distance

    function get_dist_sqrd(this, other)
        type(Vector_2D), intent(in) :: this, other
        real :: get_dist_sqrd

        get_dist_sqrd = (this%x-other%x)**2 + (this%y - other%y)**2

    end function get_dist_sqrd

    function projection(this, other)
        type(Vector_2D), intent(in) :: this, other
        type(Vector_2D) :: projection

        real :: other_length_sqrd, projected_length_times_other_length, multiplier

        other_length_sqrd = other%x**2 + other%y**2
        projected_length_times_other_length = dot(this, other)
        multiplier = projected_length_times_other_length / other_length_sqrd
        projection = Vector_2D(other%x * multiplier, other%y * multiplier)

    end function projection

    function cross(this, other)
        type(Vector_2D), intent(in) ::this, other
        real :: cross

        cross = this%x * other%y - this%y*other%x
    end function cross

end module vec2d