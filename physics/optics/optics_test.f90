program optics_test
    use opticslib
    implicit none
    real :: f, i, o, n1, n2, theta1, theta2

    f = 1
    i = 2
    o = 3

    n1 = 1
    n2 = 2
    theta1 = 15
    theta2 = 30

    print *, 'Object Distance: ', lens_formula(f=f, i=i)
    print *, 'Image Distance: ', lens_formula(f=f, o=o)
    print *, 'Focal Length: ', lens_formula(i=i, o=o)

    print *, 'Theta 1: ', snells_law(n1=n1, n2=n2, theta2=theta2)
    print *, 'Theta 2: ', snells_law(n1=n1, n2=n2, theta1=theta1)
    print *, 'N_1: ', snells_law(n2=n2, theta1=theta1, theta2=theta2)
    print *, 'N_2: ', snells_law(n1=n1, theta1=theta1, theta2=theta2)

    print *, 'Beam Displacement: ', beam_displacement(n1=n1, n2=n2, theta=theta1, h=3.0)

end program optics_test