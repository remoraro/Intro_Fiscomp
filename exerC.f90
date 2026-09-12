program exerC
    implicit none

    integer :: g_n, recorde, numero_recordes, p_inicia
    integer :: A, B, i, j, n, numero_primos, unit_out, p_max
    integer :: p_merito
    integer :: numero_gaps
    real :: m, max_m, largura, esquerda, direita, ponto_medio
    real :: soma_gaps, gap_medio, razao
    logical, allocatable :: v1_dinam(:)
    integer, allocatable :: v2_dinam(:)
    character(len=13) :: arq_saida

    read(*,*) A, B 

    allocate(v1_dinam(B))
    v1_dinam = .true.


    do i = 2, int(sqrt(real(B))) !otimizacao

        if (v1_dinam(i)) then
            do j = i*i, B, i !otimizacao
                v1_dinam(j) = .false.
            end do
        end if
        
    end do


    n = 1
    allocate(v2_dinam(size(v1_dinam)))
    do i = A, B
        if (v1_dinam(i)) then
            v2_dinam(n) = i
            n = n + 1
        end if
    end do


    numero_primos = n - 1
    arq_saida = 'gaps_out.dat'
    unit_out = 11
    open(unit=unit_out, file=arq_saida, status='replace', action='write')

    recorde = 0
    numero_recordes = 0
    max_m = 0.0
    p_inicia = 0
    p_max = 0
    p_merito = 0

    do i = 1, numero_primos - 1
        g_n = v2_dinam(i+1) - v2_dinam(i)
        m = real(g_n) / log(real(v2_dinam(i))) !precisou tratar como real por conta do ln

        if (m > max_m) then
            max_m = m
            p_merito = v2_dinam(i)
        end if

        if (g_n > recorde) then
            recorde = g_n
            p_inicia = v2_dinam(i)
            p_max = v2_dinam(i)
            write(unit_out, *) v2_dinam(i), v2_dinam(i+1), recorde, m
            numero_recordes = numero_recordes + 1
        end if

    end do

    close(unit_out)

    print *, A, B, numero_primos, numero_recordes
    print *, "      indice      num_gaps gap_medio       ln_ponto_medio     razao"
    largura = real(B - A) / 10.0

    do i = 1, 10
        esquerda = real(A) + real(i - 1) * largura
        direita = real(A) + real(i) * largura
        ponto_medio = (esquerda + direita) / 2.0
        soma_gaps = 0.0
        numero_gaps = 0


        do j = 1, numero_primos - 1
            if (real(v2_dinam(j)) >= esquerda .and. &
                (real(v2_dinam(j)) < direita .or. i == 10)) then
                soma_gaps = soma_gaps + real(v2_dinam(j+1) - v2_dinam(j))
                numero_gaps = numero_gaps + 1
            end if
        end do

        if (numero_gaps > 0) then
            gap_medio = soma_gaps / real(numero_gaps)
            razao = gap_medio / log(ponto_medio)
        else
            gap_medio = 0.0
            razao = 0.0
        end if

        print *, i, numero_gaps, gap_medio, log(ponto_medio), razao

    end do

    print *, "  gap_medio_global      maior_gap & primo  maior_merito    &   primo   razao_cramer"
    print*, real(v2_dinam(numero_primos) - v2_dinam(1)) / &
            real(numero_primos - 1), recorde, p_inicia, max_m, p_merito, &
            real(recorde) / log(real(p_max)) ** 2

    print *, "(i) O teorema dos numeros primos afirma que a quantidade de primos menores &
    & ou iguais a um numero N e aproximadamente igual a N dividido pelo logaritmo natural de N (ln(N))."
    print *, "Na tabela isso e demonstrado com cada subintervalo se aproximando de ln(x) com as razoes ~1. &
    & A precisao relativa e proxima de 1%, no terceiro teste (maior) os erros ficam na casa de 0,1%"

    print *, "(ii) A razao de Cramar nao tendeu a 1 nos exemplos. Podemos concluir que os exemplos testados &
    & nao se aproximam do infinito 'suficientemente' para esse limite valer. Foi possivel observar que com o &
    & aumento do primeiro pro segundo teste (10^3) a razao foi aumentando (0.429 e 0.653)"

    print *, "(iii) Como usei alocacao dinamica e real4 teriamos 4 bytes para cada elemento entao para 10^9 &
    & elementos teriamos 4 GB de elementos, sem contar o resto do programa ou SO e afins. Para 10^12 teriamos &
    &4 TB. Fiz o teste pelo gerenciador de tarefas e assim que inicio o programa ele consome 4,08 GB sozinho &
    &de RAM e nem roda com 10^12. Entao o gargalo seria na memoria para o primeiro teste, precisando de ~4GB &
    & e no segundo precisaria de 4TB de RAM, claramente impossivel"

    deallocate(v1_dinam)
    deallocate(v2_dinam)

end program exerC
