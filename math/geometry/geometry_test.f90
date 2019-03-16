program geometry_test
    use geometrylib
    implicit none

    type(Triangle) :: tri
    type(Circle) :: cir
    type(Sphere) :: sph

    tri = Triangle(Vertex_2D(0,0), Vertex_2D(1,0), Vertex_2D(0,1))
    cir = Circle(Vertex_2D(0,0), 3)
    sph = Sphere(Vertex_3D(0,0,0), 5)

    print *, 'Triangle Center: ', center(tri)
    print *, 'Circle Center: ', center(cir)
    print *, 'Sphere Center: ', center(sph)

    print *, 'Circle Radius: ', radius(cir)
    print *, 'Sphere Radius: ', radius(sph)

    print *, 'Triangle Area: ', area(tri)
    print *, 'Circle Area: ', area(cir)
    print *, 'Sphere Surface Area: ', area(sph)

    print *, 'Sphere Volume: ', volume(sph)

end program geometry_test