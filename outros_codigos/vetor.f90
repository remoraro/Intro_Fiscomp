program vetor

    implicit none
    real :: vector1(3), vector2(3), vector3(3), cross(3), v1_v2(3)
    real :: area, volume
    real, parameter :: eps = 1e-5


    ! Lê os vetores
    print *, 'enter the vector1: (size 3)'
    read(*,*) vector1
    print *, 'enter the vector2: (size 3)'
    read(*,*) vector2
    print *, 'enter the vector3: (size 3)'
    read(*,*) vector3

    ! Printa os vetores

    print *, 'the vector1 is: ', vector1
    print *, 'the vector2 is: ', vector2
    print *, 'the vector3 is: ', vector3

    ! Aqui cria o vetor v1 - v2
    v1_v2 = vector1 - vector2
    
    ! É o produto vetorial de v1 e v2
    cross = produto_vetorial(vector1, vector2)

    ! Verificação se v1 e v2 são paralelos (|v1 x v2| = 0)
    if (sqrt(sum(cross**2)) < eps) then
        print *, "the vectors v1 and v2 are parallel"
        stop
    end if

    
    ! O Volume é |v3 * (v1 X v2)
    volume = abs(dot_product(vector3, cross)) / 2.0 ! O produto vetorial já é nativo do f90
    
    ! Verificação se v3 é coplanar a v2 e v1, ou volume = 0
    if (abs(volume) < eps) then
        print *, "the vectors v3 is on the same plane of v2 and v1"
        stop
    end if
    
    ! Verificação manual se v3 é coplanar a v2 e v1
    ! area = vector1(1) * vector2(2) * vector3(3) + &
    ! vector1(2) * vector2(3) * vector3(1) + &
    ! vector1(3) * vector2(1) * vector3(2) - &
    ! vector1(1) * vector2(3) * vector3(2) - &
    ! vector1(2) * vector2(1) * vector3(3) - &
    ! vector1(3) * vector2(2) * vector3(1) 

    ! Calcula a área 
    area = sqrt(sum(produto_vetorial(vector1, vector3)**2)) + &
    sqrt(sum(produto_vetorial(vector2, vector3)**2)) + &
    sqrt(sum(produto_vetorial(v1_v2, vector3)**2))

    ! Printa o resultado
    print *, 'The area is: ', area
    print *, 'the volume is: ', volume


contains ! Aqui é como se fosse o def do python, em fortran vem depois do programa

    function produto_vetorial(a, b) result(c) ! Calcula o produto vetorial
        real, intent(in) :: a(3), b(3) ! intent fala para não mexer
        real :: c(3)

        c(1) = a(2)*b(3) - a(3)*b(2)
        c(2) = a(3)*b(1) - a(1)*b(3)
        c(3) = a(1)*b(2) - a(2)*b(1)
    end function produto_vetorial

end program vetor