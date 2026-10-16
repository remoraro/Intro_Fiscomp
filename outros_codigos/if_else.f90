program if_else
    implicit none


    if (5 > 4) print *, "verdade"

    if (5 > 4) then
        print *, "verdade"
    end if

    if (5 < 4) then
        print *, "verdade"
    else if (5 > 3) then
        print *, "continua verdade"
    end if

    if (5 < 4) then
        print *, "verdade"
    else if (5 < 3) then
        print *, "continua verdade"
    else
        print *, "mentira"
    end if

    
end program if_else