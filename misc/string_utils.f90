!#####################################
! Utilities for Manipulating Strings -- modeled after python's string functions
! Author: Joshua Fraustro
! Created: 3/15/2019
!#####################################

module string_utils
    implicit none

    character(10), parameter :: ascii_digits = '0123456789'
    character(27), parameter :: ascii_lowercase = 'abcdefghijklmnopqrstuvwxyz'
    character(27), parameter :: ascii_uppercase = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(54), parameter :: ascii_letters = ascii_lowercase//ascii_uppercase
    character(22), parameter :: hexdigits = '0123456789abcdefABCDEF'

contains

    function count_char(string, char)
        CHARACTER(*) :: string, char
        integer :: count_char, i

        count_char = 0

        DO i=0, len(string)
            if (string(i:i) == char) then
                count_char = count_char + 1
            end if
        END DO

    end function count_char

    function is_uppercase(char)
        character :: char
        logical :: is_uppercase

        is_uppercase = .false.

        if (char >= 'A' .and. char <= 'Z') then
            is_uppercase = .true.
        end if
    end function is_uppercase

    function is_lowercase(char)
        character :: char
        logical :: is_lowercase

        is_lowercase = .false.

        if (char >= 'a' .and. char <= 'z') then
            is_lowercase = .true.
        end if

    end function is_lowercase

    function replace_char(string, char1, char2)
        CHARACTER(*) :: string, char1, char2
        CHARACTER(LEN(string)) :: replace_char
        integer :: i

        DO i = 1, LEN(string)
            if (string(i:i) == char1) then
                replace_char(i:i) = char2
            else
                replace_char(i:i) = string(i:i)
            end if
        END DO
    end function replace_char

    function upper(string)
        CHARACTER(*) :: string
        CHARACTER(LEN(string)) :: upper
        integer, parameter :: upper_shift = ICHAR('A')-ICHAR('a')
        integer :: i

        DO i=1, LEN(string)
            if (is_lowercase(string(i:i))) then
                upper(i:i) = CHAR(ICHAR(string(i:i))+upper_shift)
            else
                upper(i:i) = string(i:i)
            end if
        END DO
    end function upper

    function lower(string)
        CHARACTER(*) :: string
        CHARACTER(LEN(string)) :: lower
        integer, parameter :: lower_shift = ICHAR('A') - ICHAR('a')
        integer :: i

        DO i = 1, LEN(string)
            if (is_uppercase(string(i:i))) then
                lower(i : i) = CHAR(ICHAR(string(i : i)) - lower_shift)
            else
                lower(i : i) = string(i : i)
            end if
        END DO
    end function lower

    function proper(string)
        character(*) :: string
        character(LEN(string)) :: proper
        integer :: i

        DO i = 1, LEN(string)
            if (i == 1 .and. is_lowercase(string(i:i))) then
                proper(i:i) = upper(string(i:i))
            else if (i > 1 .and. ICHAR(string(i - 1 : i - 1)) == 32 .and. is_lowercase(string(i : i))) then
                proper(i : i) = upper(string(i : i))
            else
                proper(i:i) = string(i:i)
            end if

        END DO
    end function proper

    function lpad(string, char, n)
        character(*) :: string, char
        integer :: n, i
        character(LEN(string)+LEN(char)*n) :: lpad

        DO i=1, n
            lpad(i:i) = char
        END DO

        DO i = 1, len(string)
            lpad(i+n:i+n) = string(i:i)
        END DO

    end function lpad

    function rpad(string, char, n)
        character(*) :: string, char
        integer :: n, i
        character(LEN(string)+LEN(char)*n) :: rpad

        DO i = 1, LEN(string)
            rpad(i:i) = string(i:i)
        END DO

        DO i = LEN(string), LEN(rpad)
            rpad(i:i) = char
        END DO
    end function rpad

    function cpad(string, char, n)
        character(*) :: string, char
        integer :: n, i
        character(LEN(string)+LEN(char)*n*2) :: cpad

        DO i = 1, n
            cpad(i:i) = char
        END DO
        DO i=1, LEN(string)
            cpad(i+n:i+n) = string(i:i)
        END DO
        DO i=LEN(string)+n+1, LEN(cpad)
            cpad(i:i) = char
        END DO

    end function cpad

end module string_utils