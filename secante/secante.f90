program  secante
    !Definicion de la estructuras de datos que se van a utilizar
    real::a=0         !limite inferior del intervalo
    real::b=1         !limite superior del intervalo
    real::e=1e-6      !error
    integer::n=100    !numero maximo de iteraciones
    integer::i        !contador de iteraciones
    real::root        !raiz de la ecuacion

    !inicializacion de los puntos de la secante
    a=0
    b=5

    !iteraciones del metodo
    do i=1,n
        !evaluamos la isema iteracion
        root=b-((b-a)/(f(b)-f(a)))*f(b)
        print*,root
        print*,"**"
        !comparamos con el error
        if(abs(f(root))<e) then
            print *, "Iteraciones del programa",i
            exit
        end if

        a=b
        b=root
    end do
end program  secante

function f(x) RESULT(resultado)
    !declaracion de la variables
    real::x            !argumento de la funcion
    real::resultado    !valor que devuelve la funcion
    resultado=(x**2)-(6*x)+8        !f matematica
end function f