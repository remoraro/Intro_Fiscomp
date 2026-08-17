program arquivo
    implicit none
    
    
    integer:: M, N, pos, i, j
    integer :: unit_in, unit_out, status
    real(8) :: valor1, valor2, menor
    real, allocatable:: menores(:)
    character(len=22) :: arq_entrada, arq_saida

    arq_entrada = 'tarefa-3-entrada-1.in'
    arq_saida   = 'tarefa-3-entrada-1.txt'
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
    menor = 10e10
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
        if (M > N) then
            print *, "O valor de M é menor que ", N, " escolha um menor"
        else
            exit
        end if
    end do

    allocate(menores(M))
    menores = huge(1.0)



    ! Loop de Escrita
    do
        if (status < 0) then
            print *, "Fim do arquivo"
            exit ! Fim do arquivo (EOF) atingido com sucesso
        else if (status > 0) then
            print *, "Erro de leitura nos dados!"
            exit
        end if
        
        ! Escreve no arquivo de saída
        if (valor1 < menores(M)) then
            pos = M
            do i = M - 1, 1, -1
                if (valor1 < menores(M)) then
                    pos = i
                else
                    exit
                end if
            end do

            do j = M, pos + 1, -1
                menores(j) = menores(j - 1)
            end do

            menores(pos) = valor1
        end if

        write(unit_out, *) valor1
        N = N + 1
    end do


    ! Fecha ambos os arquivos
    close(unit_in)
    close(unit_out)

    print *, "Cópia concluída com sucesso!"
    print *, "O arquivo tem ", N, " numeros"
    print *, "O ", M
    do i = 1, M
        print *, menores(i)
        print *, i
    end do
end program arquivo