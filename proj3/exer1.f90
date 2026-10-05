program exer1
    implicit none

    real(8) :: vi, v, tesao, deltat, v0
    real(8), parameter :: m = 80.0d0, pot = 400.0d0
    integer :: passo

    read *, tesao, deltat, v0
    passo = tesao / deltat
    print *, tesao, deltat, v0, passo


end program exer1