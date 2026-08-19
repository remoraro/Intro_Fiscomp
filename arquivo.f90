program arquivo
    implicit none
    
    
    integer:: M, N, pos, i
    integer :: unit_in, unit_out, status
    real(8) :: valor1
    real(8), allocatable :: menores(:)
    character(len=22) :: arq_entrada, arq_saida

    arq_entrada = 'tarefa-3-entrada-1.in'
    arq_saida   = 'tarefa-3-saida-1.txt'
    unit_in     = 10
    unit_out    = 11

    ! Abrir o arquivo de entrada (Apenas Leitura)
    open(unit=unit_in, file=arq_entrada, status='old', action='read', iostat=status)
    if (status /= 0) then
        print *, "Erro ao abrir o arquivo de entrada: ", arq_entrada
        stop
    end if

    ! Abrir o arquivo de saída (Escrita)
    ! 'replace' cria o arquivo ou sobrescreve se ele já existir
    open(unit=unit_out, file=arq_saida, status='replace', action='write', iostat=status)
    if (status /= 0) then
        print *, "Erro ao criar o arquivo de saída: ", arq_saida
        close(unit_in)
        stop
    end if

    
    ! Conta quantos elementos tem
    N = 0
    do
        ! Lê do arquivo de entrada
        read(unit_in, *, iostat=status) valor1
        if (status < 0) then
            print *, "Fim do arquivo"
            exit ! Fim do arquivo (EOF) atingido com sucesso
        else if (status > 0) then
            print *, "Erro de leitura nos dados!"
            exit
        end if

        N = N + 1
    end do
    
    ! Recebe M
    M = 0
    do
        write(*,*) "Digite o valor de M: "
        read(*,*) M
        if (M <= 0 .or. M > N) then
            print *, "M deve estar entre 1 e ", N
        else
            exit
        end if
    end do

    allocate(menores(M))
    menores = huge(0.0d0)


    ! Relê o arquivo e mantém os M menores em ordem crescente.
    rewind(unit_in)
    do
        read(unit_in, *, iostat=status) valor1
        if (status < 0) then
            exit
        else if (status > 0) then
            print *, "Erro de leitura nos dados!"
            exit
        end if

        if (valor1 < menores(M)) then
            pos = M
            do i = M - 1, 1, -1
                if (valor1 < menores(i)) then
                    menores(i + 1) = menores(i)
                    pos = i
                else
                    exit
                end if
            end do

            menores(pos) = valor1
        end if
    end do

    do i = 1, M
        write(unit_out, *) menores(i)
    end do


    ! Fecha ambos os arquivos
    close(unit_in)
    close(unit_out)

    print *, "Cópia concluída com sucesso!"
    print *, "Os ", M, " menores numeros sao:"
    do i = 1, M
        print *, menores(i)
    end do
end program arquivo