program exer0
    implicit none

    real(4) :: v1(3), v2(3), v3(3), v4(3), vetor_aux(4)
    real(4) :: v2_v1(3), v3_v1(3), v4_v1(3), v3_v2(3), v4_v2(3), v4_v3(3)
    real(4) :: volume, soma_area, area1, area2, area3, area4, aux

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
        real :: c(3)

        c(1) = a(2)*b(3) - a(3)*b(2)
        c(2) = a(3)*b(1) - a(1)*b(3)
        c(3) = a(1)*b(2) - a(2)*b(1)
    end function produto_vetorial

    function area(a, b) result(c) ! Calcula o produto vetorial
        real, intent(in) :: a(3), b(3) ! intent fala para não mexer
        real(4) :: c
        real(4) :: vetor_aux3(3)

        vetor_aux3 = produto_vetorial(a, b)
        c = sqrt((vetor_aux3(1))**2 + (vetor_aux3(2))**2 + (vetor_aux3(3))**2) / 2.0
    end function area

    function ordem_crescente(a1, a2, a3, a4) result(v)
        real, intent(in) :: a1, a2, a3, a4
        INTEGER :: i, j
        real(4) :: temp, eps
        real(4), DIMENSION(4) :: v

        v = [a1, a2, a3, a4]
        eps = 1.0e-6

        ! Método simples para ordenar (Bubble Sort)
        DO i = 1, 4
            DO j = i + 1, 4
                if (abs(v(i)-v(j)) < eps) then
                    v(i) = 0.0
                end if
                IF (v(i) > v(j)) THEN
                    temp = v(i)
                    v(i) = v(j)
                    v(j) = temp
                END IF
            END DO
        END DO

    end function ordem_crescente


    subroutine data(volume, soma_area, area1, area2, area3, area4)
        integer :: i
        real(4), intent(in) :: volume, soma_area, area1, area2, area3, area4
        real(4), dimension(4) :: x

        ! Open the .dat file
        open(unit=10, file='tetra_out.dat', status='replace', action='write')

        write(10, *) volume
        write(10, *) soma_area


        x = ordem_crescente(area1, area2, area3, area4)
        do i = 1, 4
            if (x(i) > 0.0) then
                write(10, *) x(i)
            end if
        end do



        ! Close the file
        close(10)
    end subroutine data


end program exer0