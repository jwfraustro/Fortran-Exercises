!########################################
! A Fortran 95 module for helping with common matrix functions.
!Intended Features:
!2d matrix
!3d matrix
!identity matrix
!adjugate
!adjugated
!copy
!determinant
!identity   `
!invert
!inverted
!normalize
!normalized
!to_3x3
!to_4x4
!transpose
!zero
!is_orthogonal
!#########################################

module matrixlib
    implicit none

    type Matrix_2D
        real, dimension(2,2) :: arr
    end type Matrix_2D

    type Matrix_3D
        real, dimension(3,3) :: arr
    end type Matrix_3D

    interface determinant
        module procedure determinant_2d
        module procedure determinant_3d
    end interface determinant

    interface transpose
        module procedure transpose_2d
        module procedure transpose_3d
    end interface transpose

    interface inverse
        module procedure inverse_2d
    end interface inverse

    interface cofactor
        module procedure cofactor_2d
    end interface cofactor

contains

    function determinant_2d(this)
        type(Matrix_2D) :: this
        real :: determinant_2d

        determinant_2d = (this%arr(1,1)*this%arr(2,2)-this%arr(1,2)*this%arr(2,1))
    end function

    function determinant_3d(this)
        type(Matrix_3D) :: this
        real :: determinant_3d, det_x, det_y, det_z

        det_x = this%arr(1,1)*(this%arr(2,2)*this%arr(3,3)-this%arr(2,3)*this%arr(3,2))
        det_y = this%arr(1,2)*(this%arr(2,1)*this%arr(3,3)-this%arr(2,3)*this%arr(3,1))
        det_z = this%arr(1,3)*(this%arr(2,1)*this%arr(3,2)-this%arr(2,2)*this%arr(3,1))

        determinant_3d = det_x - det_y + det_z

    end function determinant_3d

    function transpose_2d(this)
        type(Matrix_2D) :: this
        type(Matrix_2D) :: transpose_2d

        transpose_2d%arr(1,1) = this%arr(1,1)
        transpose_2d%arr(2,2) = this%arr(2,2)

        transpose_2d%arr(1,2) = this%arr(2,1)
        transpose_2d%arr(2,1) = this%arr(1,2)

    end function transpose_2d
    
    function transpose_3d(this)
        type(Matrix_3D) :: this
        type(Matrix_3D) :: transpose_3d
        
        transpose_3d%arr(1,1) = this%arr(1,1)
        transpose_3d%arr(2,2) = this%arr(2,2)
        transpose_3d%arr(3,3) = this%arr(3,3)
        
        transpose_3d%arr(1,2) = this%arr(2,1)
        transpose_3d%arr(2,1) = this%arr(1,2)
        
        transpose_3d%arr(1,3) = this%arr(3,1)
        transpose_3d%arr(3,1) = this%arr(1,3)
        
        transpose_3d%arr(2,3) = this%arr(3,2)
        transpose_3d%arr(3,2) = this%arr(2,3)
        
    end function transpose_3d

    function cofactor_2d(this)
        type(Matrix_2D) :: this
        type(Matrix_2D) :: cofactor_2d

        cofactor_2d%arr(1,1) = this%arr(2,2)
        cofactor_2d%arr(1,2) = -this%arr(2,1)
        cofactor_2d%arr(2,1) = -this%arr(1,2)
        cofactor_2d%arr(2,2) = this%arr(1,1)

    end function cofactor_2d

    function inverse_2d(this)
        type(Matrix_2D) :: this
        type(Matrix_2D) :: inverse_2d
        real :: det_factor

        det_factor = 1 / (determinant(this))

        inverse_2d%arr(1,1) = this%arr(2,2)*det_factor
        inverse_2d%arr(2,2) = this%arr(1,1)*det_factor

        inverse_2d%arr(1,2) = -this%arr(1,2)*det_factor
        inverse_2d%arr(2,1) = -this%arr(2,1)*det_factor

    end function inverse_2d

end module matrixlib
