program Regula_Falsi
    !Definicion de las estructuras de datos a utilizar en el programa
    real::a=3               !limite inferior
    real::b=5               !limite superior
    real::y                 !valor de la funcion
    integer::i,j            !iteradores
    integer::n=1000        !numero de iteraciones
    real::e=1e-6            !error
    real::root              !raiz


    !decreciente
    if (f(b)<=0 .and. f(a)>=0) then
        !iniciamos con la iteracion
        do i=1,n
            print*,a,b
            print*,"**"
            y=b-f(b)*((a-b)/(f(a)-f(b)))
            if (abs(f(y))<e) then
                print*,"Numero de Iteraciones:",i
                exit
            else if((f(y))>0) then
                a=y
            else if (f(y)<0) then
                b=y
            end if

        end do
    !creciente
    else if (f(b)>=0 .and. f(a)<=0) then
        !iniciamos con la iteracion
        do i=1,n
            print*,a,b
            print*,"**"
            y=b-f(b)*((a-b)/(f(a)-f(b)))
            if(abs(f(y))<e) then
                print*, "Numero de Iteraciones:",i
                exit
            else if (f(y)>0) then
                b=y
            else if (f(y)<0) then
                a=y
            end if
        end do
    
    end if
    !asignamos el valor de la raiz
    root=y
    !presentamos el valor de la raiz
    print *,"la raiz de la ecuacion es:",root
end program Regula_Falsi

function f(x) RESULT(resultado)
    !declaracion de la variables
    real::x                         !argumento de la funcion
    real::resultado                 !valor que devuelve la funcion
    resultado=(x**2)-(6*x)+8        !f matematica
end function f