program alocavel

    implicit none

    ! podemos criar vetores/listas assim:
    integer :: v1(3), i, j
    real :: v2(3, 4)

    ! mas também podemos fazer alocação dinâmica
    integer, allocatable :: v1_dinam(:)
    real, allocatable :: v2_dinam(:,:)

    print *, "Qual vai ser o tamanho de v1_dinam? "
    read(*,*) i

    allocate(v1_dinam(i))
    do j = 1, i
        print *, "Qual vai ser v1_dinam(", j, ")"
        read(*,*) v1_dinam(j) 
    end do
    do j = 1, i
        print *, "v1_dinam(", j, ") e igual a: ", v1_dinam(j) 
    end do
    allocate(v2_dinam(10,10))

    deallocate(v1_dinam)
    deallocate(v2_dinam)

end program alocavel