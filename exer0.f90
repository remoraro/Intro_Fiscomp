program exer0
    implicit none

    real(4) :: v1(3), v2(3), v3(3), v4(3)
    real(4) :: v2_v1(3), v3_v1(3), v4_v1(3), v3_v2(3), v4_v2(3), v4_v3(3)
    real(4) :: volume, soma_area, area1, area2, area3, area4

    ! Lê os vetores
    read(*,*) v1
    read(*,*) v2
    read(*,*) v3
    read(*,*) v4

    v2_v1 = v2 - v1
    v3_v1 = v3 - v1
    v3_v2 = v3 - v2
    v4_v3 = v4 - v3
    v4_v2 = v4 - v2
    v4_v1 = v4 - v1



    volume = abs(dot_product(v2_v1, produto_vetorial(v3_v1, v4_v1))) / 6.0

    area1 = area(v2_v1, v3_v1)
    area2 = area(v3_v2, v4_v2)
    area3 = area(v4_v3, -v3_v1)
    area4 = area(-v4_v1, -v4_v2)

    soma_area = area1 + area2 + area3 + area4

    call data(volume, soma_area, area1, area2, area3, area4)


contains ! Aqui é como se fosse o def do python, em fortran vem depois do programa

    function produto_vetorial(a, b) result(c) ! Calcula o produto vetorial
        real, intent(in) :: a(3), b(3) ! intent fala para não mexer
        real :: c(3) ! aqui declaramos como será o resultado

        c(1) = a(2)*b(3) - a(3)*b(2)
        c(2) = a(3)*b(1) - a(1)*b(3)
        c(3) = a(1)*b(2) - a(2)*b(1)
    end function produto_vetorial

    function area(a, b) result(c) ! Calcula a área
        real, intent(in) :: a(3), b(3)
        real(4) :: c
        real(4) :: vetor_aux3(3)

        vetor_aux3 = produto_vetorial(a, b)
        c = sqrt((vetor_aux3(1))**2 + (vetor_aux3(2))**2 + (vetor_aux3(3))**2) / 2.0
    end function area

    function ordem_crescente(a1, a2, a3, a4) result(v) ! Bubble sort
        real, intent(in) :: a1, a2, a3, a4 ! números
        INTEGER :: i, j ! índices (precisa declarar)
        real(4) :: temp, eps ! auxiliares, temporário e epsilon
        real(4), DIMENSION(4) :: v

        v = [a1, a2, a3, a4] ! vetor auxiliar
        eps = 1.0e-6

        do i = 1, 4 ! começa do começo e vai até o fim
            do j = i + 1, 4 ! vai do item da direita até o fim

                ! se eles forem iguais, ele agora é zero
                if (abs(v(i)-v(j)) < eps) then 
                    v(i) = 0.0
                end if

                if (v(i) > v(j)) then ! se o atual for maior que o da direita
                    temp = v(i) ! temporário é o atual
                    v(i) = v(j) ! o atual vira o da direita
                    v(j) = temp ! o da direita vira o atual
                    ! no fim nós só trocamos o da esquerda (maior) com o da direita (menor)
                    ! ai agora o (j) vai incrementar e vai fazer a comparação láaa com o primeiro
                    ! se o primeiro ainda for maior, ele troca de novo e vai fazendo isso até
                    ! o final da lista
                end if
            end do
        end do

    end function ordem_crescente


    subroutine data(volume, soma_area, area1, area2, area3, area4)
        integer :: i
        real(4), intent(in) :: volume, soma_area, area1, area2, area3, area4
        real(4), dimension(4) :: x

        ! abre o arquivo
        open(unit=10, file='tetra_out.dat', status='replace', action='write')



        ! escreve o volume e a área
        write(10, *) volume
        write(10, *) soma_area

        ! escreve as áreas em ordem crescente e sem repetir (zeros)
        x = ordem_crescente(area1, area2, area3, area4)
        do i = 1, 4
            if (x(i) > 0.0) then
                write(10, *) x(i)
            end if
        end do



        ! fecha o arquivo
        close(10)
    end subroutine data


end program exer0