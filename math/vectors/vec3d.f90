module vec3d
    implicit none
    real :: deg_to_rads = 3.141 / 180

    type Vector_3D
        real :: x, y, z
    end type Vector_3D
    
contains
    function length(this)
        type(Vector_3D), intent(in) :: this
        real :: length
        length = SQRT(this%x**2 + this%y**2 + this%z**2)
    end function length

    function length_squared(this)
        type(Vector_3D), intent(in) :: this
        real :: length_squared

        length_squared = length(this)**2

    end function length_squared

    subroutine set_length(this, value)
        type(Vector_3D) :: this
        real, intent(in) :: value
        this%x = this%x * (value / length(this))
        this%y = this%y * (value / length(this))
        this%z = this%z * (value / length(this))
    end subroutine set_length
    
    function add(this, other)
        type(Vector_3D), intent(in) :: this, other
        type(Vector_3D) :: add

        add = Vector_3D(this%x + other%x, this%y + other%y, this%z+other%z)

    end function add

    function subtract(this, other)
        type(Vector_3D), intent(in) :: this, other
        type(Vector_3D) :: subtract

        subtract = Vector_3D(this%x - other%x, this%y - other%y, this%z-other%z)

    end function subtract

    function mul(this, other)
        type(Vector_3D), intent(in) :: this, other
        type(Vector_3D) :: mul

        mul = Vector_3D(this%x * other%x, this%y * other%y, this%z*other%z)

    end function mul

    function neg(this)
        type(Vector_3D), intent(in) :: this
        type(Vector_3D) :: neg

        neg = Vector_3D(-this%x, -this%y, -this%z)

    end function neg

    function pos(this)
        type(Vector_3D), intent(in) :: this
        type(Vector_3D) :: pos

        pos = Vector_3D(ABS(this%x), ABS(this%y), ABS(this%z))
    end function pos

    subroutine rotate_around_z(this, degrees)
        type(Vector_3D) :: this
        real :: degrees, rads,x,y
        rads = degrees * deg_to_rads

        x = this%x * COS(rads) - this%y * SIN(rads)
        y = this%x * SIN(rads) + this%y * COS(rads)

        this%x = x
        this%y = y

    end subroutine rotate_around_z

    subroutine rotate_around_x(this, degrees)
        type(Vector_3D) :: this
        real :: degrees, rads, x, y
        rads = degrees * deg_to_rads

        y = this%y * COS(rads) - this%z * SIN(rads)
        z = this%y * SIN(rads) + this%z * COS(rads)

        this%y = y
        this%z = z

    end subroutine rotate_around_x

    subroutine rotate_around_y(this, degrees)
        type(Vector_3D) :: this
        real :: degrees, rads, x, y
        rads = degrees * deg_to_rads

        z = this%z * COS(rads) - this%x * SIN(rads)
        x = this%z * SIN(rads) + this%x * COS(rads)

        this%z = z
        this%x = x

    end subroutine rotate_around_y

end module vec3d