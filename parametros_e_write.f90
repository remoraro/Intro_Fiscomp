program parametros_e_write
    implicit none

    ! variáveis já podem receber o valor quando forem declaradas
    real, parameter :: pi = 3.14 
    real, dimension(5) :: lista
    real, dimension(2,4) :: matrix ! 2 linhas, 4 colunas
    character(3), parameter :: texto = "ABC"

    ! write também é outro jeito de imprimir na tela
    ! write (unid, formato) uq vc quer imprimir
    ! formato pode ser:
    !   A (caracteres)
    !   I (inteiros)
    !   F (reais)
    !   E (notação exponencial de reais)
    !   ES (notação científica)
    !   X (espaço em branco)
    !   / (quebra de linha), pode ser new_line("a") no final tb
    write(*,*) "The value of pi is: ", pi
    write(*, '(A)') "O texto e: ", texto
    
    ! Quero um texto (A), um espaço em branco (1X), um num de ponto flutuante
    ! que ocupa 4 espaços (OBS: O PONTO TBM CONTA) e tem 2 casas decimais (F4.2), outro em branco (1X),
    ! outro texto (A) e quebrar a linha (/)
    write(*, '(A, 1X, F4.2, 1X, A, /)') "O dobro de pi (", pi, ") vale aprox 6"

    ! Para não termos que escrever tudo dentro do write, podemos criar um format
    write(*, 123) "O dobro de pi (", pi, ") vale aprox 6"
    123 format(A, 1X, F4.2, 1X, A, /)

    write(*, 456) pi
    456 format("O dobro de pi (", 1X, F4.2, 1X, ") vale aprox 6", /)


    


end program parametros_e_write