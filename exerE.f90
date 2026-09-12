program exerE
    implicit none

    integer :: i, j, dimensao, indice_max
    real(8) :: precisao, lambda1, lambda_anterior, norma
    real(8) :: rho, sinal
    real(8), allocatable :: M(:, :), x(:), Mx(:)

    read(*, *) precisao
    read(*, *) dimensao


    allocate(M(dimensao, dimensao), x(dimensao), Mx(dimensao))

    do i = 1, dimensao
        read(*, *) (M(i, 1), j = 1, dimensao)
    end do

    x = 1.0_8 / sqrt(real(dimensao, 8)) !precisa ser real e normalizado
    lambda_anterior = huge(1.0_8) !cria grande só pra n dar problema

    do
        !Mx
        Mx = matmul(M, x)

        ! quociente de Rayleigh: (x . Mx) / (x . x)
        lambda1 = dot_product(x, Mx) / dot_product(x, x)

        !norma
        norma = sqrt(dot_product(Mx, Mx))
        if (norma == 0.0_8) then
            error stop 
        end if
        
        ! x_k normalizado
        x = Mx / norma

        ! verifica se ta menor que o erro
        if (abs(lambda1 - lambda_anterior) <= precisao) exit
        lambda_anterior = lambda1
    end do


    ! aqui é só colocar o vetor como positivo pra legibilidade

    ! maxloc encontra o maior numero do vetor x e retorna sua posicao
    indice_max = maxloc(abs(x), dim=1) 
    ! sign retorna o primeiro argumento (1) com o sinal do segundo (maior numero de x)
    sinal = sign(1.0_8, x(indice_max))

    !agora sabemos o sinal do maior numero (em modulo) do x. Se for + não da nada e se for - da +
    x = sinal * x
    
    Mx = matmul(M, x)

    rho = sqrt(dot_product(Mx - lambda1 * x, Mx - lambda1 * x)) / max(abs(lambda1), 1.0_8)

    print *, 'lambda1 =', lambda1
    do i = 1, dimensao
        print *, x(i)
    end do
    print *, 'residuo relativo =', rho

    deallocate(M, x, Mx)

    print *, "(i) No primeiro teste M = (2 1 1 2) os autovalores são 3 e 1 e autovetores (1,1)/sqrt(2) e&
    & (1, -1)/sqrt(2) respectivamente. Vemos que lambda2/lambda1 = 1/3. Se escrevermos x como alpha*autovet1 &
    &+ beta*autovet2 e multiplicarmos por M nos 2 lados vamos ter Mx = alhpa3^k *autovet1 + beta1^k * autovet2. &
    &A razao entre as componentes de autovet2 e autovet1 é beta/alpha*(1/3)^k logo |lambda2/lambda1|^k = *(1/3)^k.&
    & O quociente de Rayleigh tira o erro de primeira ordem então fica |lambda2/lambda1|^2k = *(1/3)^2k."
    print *, "Se x0 for (1,0) lambda seria (3*9^k + 1)/(9^k + 1) enquanto o erro na direção do autovetor é (1/3)^k &
    & e isso se traduz que se o criterio de parado é de 1e-12 no autovalor o erro no vetor fica a raiz disso, 1e-6. &
    & Logo ele teria metade dos digitos corretos (6 em vez de 12)"

    print *, "(ii) isso acontece porque os autovalores são +-1, que quebra o metodo das potencias que supoe que &
    & deve existir um autovalor dominante em módulo. x0 = alpha*autovet1+ + beta*autovet2- logo Mx0 = alphaautovet1+&
    & + (-1)^k beta*autovet2- então o quociente de rayleigh fica (alpha^2 - beta^2) / (alpha^2 + beta^2), que é &
    &independente do numero da iteracao, so alternando o sinal. Então duas iteracoes podem ser iguais, afirmando a &
    &convergencia no programa Um exemplo é x0 = (1,0) então x0 = (autovet1+ + autovet2-) / sqrt(2) que tem lambda = 0 &
    &. Então para cada k ficaria alternando entre (1,0), (0,1), (1,0), ..."

    print *, "(iii) Se ||u1|| = 1 e o residuo = Mu1 - lambda1u1, como M é simétrica, seus vetores formam uma base&
    & ortonormal, então u1 = soma(c_i*v_i) com c_i^2 = 1. Logo residuo =  soma(c_i*(lambda_i - lambda_1)*v_i) e&
    & ||residuo||^2 = soma(c_i^2 (lambda_i - lambda_1)^2) que é a média ponderada das distancias ao quadrado de lambda1&
    & e os autovalores de M. Como c_i^2 somam 1 temos que |lambda_i - lambda1| <= ||r|| sempre. Podemos reescrever como &
    & rho*max(|lambda1|, 1) >= |lambda_i - lambda1|. Essa afirmação é mais forte que o criterio de parada porque ele só &
    & fala de duas estimativas consecutivas e isso só afirma que deixou de mudar significativamente, não fala se chegou &
    & perto de um resultado. O resído verifica na equação então ele dá uma garantia"
end program exerE
