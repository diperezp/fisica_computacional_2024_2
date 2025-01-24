Program Bisection
    !inicializamos la variable a utilizar
    integer::n=1000 !numero de iteraciones
    integer::i      !iterador
    real:: a=0   !limite izquierdo del intervalo
    real:: b=3      !limite derecho del intervalo
    real:: pm=0     !punto medio
    real:: e=1e-6   !error 
    real:: y=0      !evaluacion de la funcion
    real:: raiz=1   !raiz de la 
    print*, expresion(a),"-",expresion(b)
    Print*, "Init"
    !bisection decreciente
    if (expresion(a)>=0 .and. expresion(b)<=0) then
        print *,"bisection decreciente"
        do i=0,n
            !hallamos el punto medio del intervalo
            pm=(a+b)/2
            y=expresion(pm)
            Print *,pm
            Print *,y
            
            if (abs(y)<e) then
                print *,"Se realizaron",i," iteraciones"
                exit
            else if (y<0) then
                b=pm
            else if (y>0) then
                a=pm
            end if
            Print*,a,b
            Print *,"****"
            
        end do
    !bisection creciente
    else if (expresion(a)<=0 .and. expresion(b)>=0) then
        print *,"bisection creciente"
        !bisection creciente
        do i=0,n
            !hallamos el punto medio del intervalo
            pm=(a+b)/2
            y=expresion(pm)
            Print *,pm
            Print *,y
            
            if (abs(y)<e) then
                print *,"Se realizaron",i," iteraciones"
                exit
            else if (y<0) then
                a=pm
            else if (y>0) then
                b=pm
            end if
            Print*,a,b
            Print *,"****"
            
        end do
    end if
    raiz=pm
    !presentamos los datos en la consola
    Write (*,*) "la raiz de la ecuacion es:",raiz
End Program Bisection

function expresion(x) RESULT(resultado)
    !declaracion de la variables
    real::x            !argumento de la funcion
    real::resultado    !valor que devuelve la funcion
    resultado=(x**2)-(6*x)+8        !expresion matematica
end function expresion