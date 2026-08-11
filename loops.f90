program loop
    implicit none

    integer :: n, dobro

    ! o "do" do fortran é a mesma coisa do "for" do python
    ! a sintaxe precisa do começo e fim ("do" e "end do")
    ! é inclusivo no intervalo
    do n = 1, 10
        dobro = n * 2
        print *, dobro
    end do

end program loop