########################################
!A Fortran 95 module for helping with geometric functions.
!Triangles
!Circles
!Spheres
!Squares
!Cubes
!Cylinders
!Vertices
!Planes
!area
!volume
!surface area
!transformations
!scaling
!normals
!#########################################

module geometrylib
    implicit none

    real, parameter :: pi = 3.14159265359

    type Vertex_2D
        real :: x, y
    end type Vertex_2D

    type Vertex_3D
        real :: x,y,z
    end type Vertex_3D

    type Triangle
        type(Vertex_2D) :: v1, v2, v3
    end type Triangle

    type Circle
        type(Vertex_2D) :: center
        real :: radius
    end type Circle

    type Rectangle
        type(Vertex_2D) :: v1, v2, v3, v4
    end type Rectangle

    type Sphere
        type(Vertex_3D) :: center
        real :: radius
    end type Sphere

    type Plane
        type(Vertex_3D) :: v1, v2, v3
    end type Plane

    interface center
        module procedure triangle_center
        module procedure circle_center
        module procedure sphere_center
    end interface center

    interface radius
        module procedure circle_radius
        module procedure sphere_radius
    end interface radius

    interface area
        module procedure triangle_area
        module procedure circle_area
        module procedure sphere_area
    end interface area

    interface volume
        module procedure sphere_volume
    end interface volume

contains

    function triangle_center(this)
        type(Triangle) :: this
        type(Vertex_2D) :: triangle_center

        triangle_center%x = (this%v1%x + this%v2%x + this%v3%x) / 3
        triangle_center%y = (this%v1%y + this%v2%y + this%v3%y) / 3

    end function triangle_center

    function circle_center(this)
        type(Circle) :: this
        type(Vertex_2D) :: circle_center

        circle_center%x = this%center%x
        circle_center%y = this%center%y

    end function circle_center

    function sphere_center(this)
        type(Sphere) :: this
        type(Vertex_3D) :: sphere_center

        sphere_center%x = this%center%x
        sphere_center%y = this%center%y
        sphere_center%z = this%center%z

    end function sphere_center

    function sphere_radius(this)
        type(Sphere) :: this
        real :: sphere_radius

        sphere_radius = this%radius
    end function sphere_radius

    function circle_radius(this)
        type(Circle) :: this
        real :: circle_radius

        circle_radius = this%radius
    end function circle_radius

    function triangle_area(this)
        type(Triangle) :: this
        real :: triangle_area

        triangle_area = ABS((this%v1%x*(this%v2%y-this%v3%y)+this%v2%x*(this%v3%y-this%v1%y)+this%v3%x*(this%v1%y-this%v2%y))/2)

    end function triangle_area

    function circle_area(this)
        type(Circle) :: this
        real :: circle_area

        circle_area = pi * this%radius **2

    end function circle_area

    function sphere_area(this)
        type(Sphere) :: this
        real :: sphere_area

        sphere_area = 4*pi*this%radius**2
    end function sphere_area

    function sphere_volume(this)
        type(Sphere) :: this
        real :: sphere_volume

        sphere_volume = 4*pi*(this%radius**3)/3

    end function sphere_volume

end module geometrylib