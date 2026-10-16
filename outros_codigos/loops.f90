program loop
    implicit none

    integer :: n, dobro

    ! o "do" do fortran é a mesma coisa do "for" do python
    ! a sintaxe precisa do começo e fim ("do" e "end do")
    ! é inclusivo no intervalo
    do n = 1, 10, 1
        dobro = n * 2
        print *, dobro
    end do

    do
        print *, "esse loop e infinito"
        print *, "para quebrar se usa exit"
        exit
    end do

    do while (n < 100)
        n = n + 10
        print *, n
    end do

end program loop