program vector_test
    use vec2d
    implicit none

    type(Vector_2D) :: v, u, a
    v = Vector_2D(1, 1)
    u = Vector_2D(2, 8)

    call print(v)
    call print(u)

    call print_length(v)
    call print_length(u)

    a = add(v, u)

    call print(a)

end program vector_test