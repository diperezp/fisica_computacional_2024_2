program Fixed_Point

    !Definicion de las estructura de datos que se van a utilizar en el programa
    real::x=0.5       !inicio del programa
    real::y         !almacena el valor de la funcion
    integer::i,j    !iteradores
    real::e=1e-6    !error 
    integer::n=10 !numero de iteraciones
    real::root      !raiz de la funcion

    do i=1,n
        !evaluamos la iesima iteracion
        y=x+expresion(x)
        print*,x
        print*,"**"
        !evaluamos el valor
        if(e>abs(x-y)) then
            root=y
            print *, "Iteraciones del programa:",i
            exit   !detenemos el bucle
        end if
        x=y
    end do

    print *, "La raiz de la ecuacion es", root

end program Fixed_Point

function expresion(x) RESULT(resultado)
    !declaracion de la variables
    real::x            !argumento de la funcion
    real::resultado    !valor que devuelve la funcion
    resultado=log(x)        !expresion matematica
end function expresion