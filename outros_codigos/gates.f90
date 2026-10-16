program gates
    implicit none

    ! As portas lógicas que existem são:
    !   .not.
    !   .and.
    !   .or.
    !   .eqv. (igual =) somente para bool
    !   .neqv. (não igual !=) somente para bool

    logical :: estado
    character :: j1, j2, j3
    j1 = "f"
    j2 = "f"
    j3 = "a"

    print *, "Janela 1 esta aberta? ", j1 == "a"

    estado = j1 == "a" .or. j2 == "a" .or. j3 == "a"
    print *, "Alguma janela esta aberta? ", estado

    estado = j1 == "a" .and. j2 == "a" .and. j3 == "a"
    print *, "Todas as janelas estao abertas? ", estado

end program gates