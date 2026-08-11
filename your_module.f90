module your_module
    implicit none
    integer :: n = 2
end module

program main
    implicit none
    real :: x

    block
        use your_module, only: n ! you can import modules within blocks
        real :: y ! variável local
        y = 2.0
        x = y ** n
        print *, y
    end block
    ! print *, y ! não funciona pq y só existe dentro do bloco
    print *, x  ! prints 4.00000000
end program