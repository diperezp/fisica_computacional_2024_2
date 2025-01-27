program Raphson_Newton
    !definicion de las estructura de los datos que se van a utilizar en el programa
    real::x=-1          !valor de inicio del metodo
    real::y             !valor de la funcion
    integer::i,j        !iteradores
    real::e=1e-6        !error
    integer::n=1000     !numero de iteraciones
    real::root          !raiz de la ecuacion

    do i=1,n
        !evaluamos la isema iteracion
        y=x-f(x)/f_prima(x)
        print*,y
        print*,"**"
        !comparamos con el error
        if(abs(f(y))<e) then
            x=y
            print *, "Iteraciones del programa",i
            exit
        end if

        x=y
    end do
    !asignamos el valor de la raiz
    root=x
    !imprimimos el valor de la raiz
    print *, "El valor de la raiz de la funcion es:",root

end program Raphson_Newton

function f(x) RESULT(resultado)
    !declaracion de la variables
    real::x            !argumento de la funcion
    real::resultado    !valor que devuelve la funcion
    resultado=(x**2)-(6*x)+8        !f matematica
end function f

function f_prima(x) RESULT(resultado)
    !declaracion de la variables
    real::x            !argumento de la funcion
    real::resultado    !valor que devuelve la funcion
    resultado=2*x-6    !f matematica
end function f_prima