program integracion_numerica
    implicit none
    real::PI=3.14159265
    real::a                        !limite inferior del intervalo de integracion
    real::b            !limite superior del intervalo de integracion
    integer::n=1000                   !numero de interaciones
    real::y
    real::integral_trapezoidal
    real::integral_trapezoidal_abierta
    real::integral_simpson_abierto
    real::integral_simpson
    real::integral

    a=0
    b=PI/36

    integral=2*PI*SQRT(0.5/9.8)/4
    write(*,*)"Periodo",integral

    y=integral_trapezoidal_abierta(a,b,n)*SQRT(0.5/(2*9.8))
    write(*,*)"Regla trapezoidal",y
    write(*,*)"Error trapezoidal",(abs(integral-y)/integral)*100


    y=integral_simpson_abierto(a,b,n)*SQRT(0.5/(2*9.8))
    write(*,*)"Regla Simpson",y
    write(*,*)"Error simpson",(abs(integral-y)/integral)*100



end program integracion_numerica


real function f(x)
    real::x
    real::PI=3.14159265
    f=1/SQRT(cos(x)-cos(PI/36))
end function

real function integral_trapezoidal(a,b,n)
    real::a             !limite inferior del intervalo de integracion
    real::b             !limite superior del intervalo de integracion
    integer::n          !numero de subrectangulos
    real::h             !paso de integracion
    real::suma          !suma

    !asignamos el valor del paso de integracion
    h=(b-a)/n

    !integramos numericamente la expresion
    suma=(f(a)+f(b))**(h/2)
    do i=1,n-1
        suma=suma+2*f(a+i*h)*(h/2)
    end do
    suma=suma
    integral_trapezoidal=suma
end function

real function integral_simpson(a,b,n)
    real::a             !limite inferior del intervalo de integracion
    real::b             !limite superior del intervalo de integracion
    integer::n          !numero de subrectangulos
    real::h             !paso de integracion
    real::suma          !suma

    !asignamos el valor del paso de integracion
    h=(b-a)/n

    !integramos numericamente la expresion
    suma=(f(a)+f(b))*(h/3)
    do i=1,n-1
        if (mod(i, 2) == 0) then
            suma = suma + 2 * f(a + i * h)*(h/3)
        else
            suma = suma + 4 * f(a + i * h)*(h/3)
        end if
    end do
    suma=suma
    integral_simpson=suma

end function


real function integral_trapezoidal_abierta(a,b,n)
    real::a             !limite inferior del intervalo de integracion
    real::b             !limite superior del intervalo de integracion
    integer::n          !numero de subrectangulos
    real::h             !paso de integracion
    real::suma          !suma

    !asignamos el valor del paso de integracion
    h=(b-a)/n

    !integramos numericamente la expresion
    suma=(f(a))*(h/2)
    do i=1,n-1
        suma=suma+2*f(a+i*h)*(h/2)
    end do
    suma=suma
    integral_trapezoidal_abierta=suma
end function

real function integral_simpson_abierto(a,b,n)
    real::a             !limite inferior del intervalo de integracion
    real::b             !limite superior del intervalo de integracion
    integer::n          !numero de subrectangulos
    real::h             !paso de integracion
    real::suma          !suma

    !asignamos el valor del paso de integracion
    h=(b-a)/n

    !integramos numericamente la expresion
    suma=(f(a))*(h/3)
    do i=1,n-1
        if (mod(i, 2) == 0) then
            suma = suma + 2 * f(a + i * h)*(h/3)
        else
            suma = suma + 4 * f(a + i * h)*(h/3)
        end if
    end do
    suma=suma
    integral_simpson_abierto=suma

end function