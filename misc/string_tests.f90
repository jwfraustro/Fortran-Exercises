program string_tests
    use string_utils
    implicit none

    character(13) :: string = 'Hello, World!'

    print *, 'String: ', string
    print *, 'Count Character (l): ', count_char(string, 'l')

    print *, 'Replace Character (o)->(x): ',replace_char(string, 'o', 'x')

    print *, 'Upper: ', upper(string)
    print *, 'Lower: ', lower(string)
    print *, 'Proper: ', proper(lower(string))

    print *, 'Left Pad (5 x #): ', lpad(string, '#', 5)
    print *, 'Right Pad (5 x #): ', rpad(string,'#',5)
    print *, 'Center Pad (5 x *): ',cpad(string, '*', 5)

    print *, 'abcd is digits: ', is_digits('abcd')
    print *, '123 is digits: ', is_digits('123')

    print *, 'abcd is letters: ', is_letters('abcd')
    print *, '123 is letters: ', is_letters('123')

end program string_tests