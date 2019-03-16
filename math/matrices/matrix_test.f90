program matrix_test
    use matrixlib
    implicit none

    type(Matrix_2D) :: mat_2
    type(Matrix_3D) :: mat_3

    mat_2%arr = reshape((/1,3,2,4/), shape(mat_2%arr))
    mat_3%arr = reshape((/1,4,7,2,5,8,3,6,9/), shape(mat_3%arr))

    print *, '2D Matrix: ', mat_2
    print *, '3D Matrix: ',mat_3

    print *, '2D Determinant: ', determinant(mat_2)
    print *, '3D Determinant: ', determinant(mat_3)

    print *, '2D Transpose: ', transpose(mat_2)
end program matrix_test