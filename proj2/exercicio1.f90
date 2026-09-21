program exercicio1
    implicit none

    real(8) :: f_linha_frente, f_linha_tras, f_linha_sim_3, f_linha_sim_5
    real(8) :: f_n, f_linhalinha_5, f_linhalinhalinha_5
    real(8) :: h, num_de_h, n, fac, fmenos2, fmenos1, f0, f1, f2
    real(8), parameter :: x = 3.0 ** (-1)

    
    h = 0.5 
    fmenos2 = exp(4.0*(x - 2.0*h)) * cos((x - 2.0*h)/2.0)
    fmenos1 = exp(4.0*(x - 1.0*h)) * cos((x - 1.0*h)/2.0)
    f0 = exp(4.0*(x)) * cos((x)/2.0)
    f1 = exp(4.0*(x + 1.0*h)) * cos((x + 1.0*h)/2.0)
    f2 = exp(4.0*(x + 2.0*h)) * cos((x + 2.0*h)/2.0)

    f_linha_frente = (f1 - f0) / h
    f_linha_tras = (f0 - fmenos1) / h
    f_linha_sim_3 = (f1 - fmenos1) / (2.0* h)
    f_linha_sim_5 = (-f2 + 8.0*f1 - 8.0*fmenos1 + fmenos2) / (12.0*h)
    f_linhalinha_5 = (-f2 + 16.0*f1 - 30.0*f0 + 16*fmenos1 - fmenos2) / (12.0*h**2.0)
    f_linhalinhalinha_5 = (f2 - 2.0*f1 + 2.0*fmenos1 - fmenos2) / (2.0*h**3.0)

    print *, "h derivada simétrica 3p derivada pra trás 2p derivada pra frente 2 &
    & pontos derivada simétrica 5 pontos derivada segunda simétrica 5 pontos &
    & derivada terceira anti-simétrica 5 pontos"
    print *, h, f_linha_sim_3, f_linha_tras, f_linha_frente, f_linha_sim_5, &
    & f_linhalinha_5, f_linhalinhalinha_5
    


end program exercicio1