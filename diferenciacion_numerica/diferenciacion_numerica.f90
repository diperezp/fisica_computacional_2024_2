program diferenciacion_numerica
    real,dimension(256)::X
    real,dimension(256)::Y
    real,dimension(256)::YP3P
    real,dimension(256)::YP2PUP
    real,dimension(256)::YP2PDN
    real,dimension(256)::YP5P
    real,dimension(256)::YP4PUP
    real,dimension(256)::YP4PDN
    real,dimension(256)::Y2P4P
    real,dimension(256)::Y3P4P
    real,dimension(256)::Y2P5P
    real,dimension(256)::Y3P5P

    real::a=-3.141516*10         !limite inferior del intervalo
    real::b=3.141516*10       !limite superior del intervalo
    integer::n=256      !cantidad de datos
    integer::i,j        !iteradores

    !asignamos los valores a los arreglos
    do i=1,n
        X(i)=a+((b-a)/n)*i
        Y(i)=f(X(i))
        YP3P(i)=fp3p(X(i),(b-a)/n)
        YP2PUP(i)=fp2pup(X(i),(b-a)/n)
        YP2PDN(i)=fp2pdn(X(i),(b-a)/n)
        YP5P(i)=fp5p(X(i),(b-a)/n)
        YP4PUP(i)=fp4pup(X(i),(b-a)/n)
        YP4PDN(i)=fp4pdn(X(i),(b-a)/n)
        Y2P4P(i)=f2p4p(X(i),(b-a)/n)
        Y3P4P(i)=f3p4p(X(i),(b-a)/n)
        Y2P5P(i)=f2p5p(X(i),(b-a)/n)
        Y3P5P(i)=f3p5p(X(i),(b-a)/n)



    end do

    


    !creamos 
    open(UNIT=1,FILE='datos.txt')

    do i=1,n
        WRITE(1,*)X(i),Y(i),YP3P(i),YP2PUP(i),YP2PDN(i),YP5P(i),YP4PUP(i),YP4PDN(i),Y2P4P(i),Y3P4P(i),Y2P5P(i),Y3P5P(i)
    end do

    close(UNIT=1)

end program diferenciacion_numerica


function f(x) result(valor)
    implicit none
    real, intent(in) :: x
    real :: valor

    ! Aquí puedes definir la expresión matemática que desees evaluar
    valor = EXP(-((0.1*x)**2))*SIN(x)

end function f


function fp3p(x,h) result(y)
    real,intent(in)::x      !punto a evaluar
    real,intent(in)::h      !paso de diferenciacion

    y=(f(x+h)-f(x-h))/(2*h)

end function fp3p

real function fp2pup(x,h)
    real::x
    real::h
    
    fp2pup=(f(x+h)-f(x))/h
end function fp2pup

real function fp2pdn(x,h) 
    real::x
    real::h
    
    fp2pdn=(f(x)-f(x-h))/h
end function fp2pdn

real function fp5p(x,h)
    real::x,h

    fp5p=(f(x-2*h)-8*f(x-h)+8*f(x+h)-f(x+2*h))/(12*h)
end function

real function fp4pup(x,h)
    real::x,h
    fp4pup=(-2*f(x-h)-3*f(x)+6*f(x+h)-f(x+2*h))/(6*h)
end function

real function fp4pdn(x,h)
    real::x,h
    fp4pdn=(2*f(x+h)+3*f(x)-6*f(x-h)+f(x-2*h))/(6*h)
end function

real function f2p4p(x,h)
    real::x,h
    f2p4p=(f(x-h)-2*f(x)+f(x+h))/(h**2)
end function

real function f3p4p(x,h)
    real::x,h
    f3p4p=(-f(x-h)+3*f(x)-3*f(x+h)+f(x+2*h))/(h**3)
end function

real function f2p5p(x,h)
    real::x,h
    f2p5p=(-f(x-2*h)+16*f(x-h)-30*f(x)+16*f(x+h)-f(x+2*h))/(12*h**2)
end function

real function f3p5p(x,h)
    real::x,h
    f3p5p=(-f(x-2*h)+2*f(x-h)-2*f(x+h)+f(x+2*h))/(2*h**3)
end function

