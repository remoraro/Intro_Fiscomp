program exerA
    implicit none

    real(8) :: x, i, d
    real(8) :: x0, r, eps
    integer :: unidade_out

    read(*,*) x0
    read(*,*) r
    read(*,*) eps



    unidade_out = 10
    open(unit=unidade_out, file="distA_out.dat", status="replace")

end program