! ----------------------------------------------------------------------
! Author: Diego Alcides Perez Pineda
! Date: 28/01/2024
! Description:
! ----------------------------------------------------------------------
program solucionParcial
    !Definicion de las estructuras de datos a utilizar
    real::a=350         !limite inferior del intervalo
    real::b=400         !limite superior del intervalo
    real::a_regula=200  !limite inferior del intervalo
    real::e=1e-6        !error
    integer::n=10    !numero maximo de iteraciones
    real::root

    !Llamado a la funcion secante
    call secante(a, b, e, n, root)

    !Imprimir la raiz
    print *, "La raiz es: ", root, "Ohmios"

    !Llamado a la funcion regula_falsi
    call regula_falsi(a_regula, b, e, n, root)

    !Imprimir la raiz
    print *, "La raiz es: ", root, "Ohmios"


end program solucionParcial

subroutine secante(inferior, superior, e, max_iter, root)
    external :: Funcion                           !Definicion de la funcion a utilizar
    !Definicion de la estructuras de datos que se van a utilizar
    real,intent(in)::inferior                     !limite inferior del intervalo
    real,intent(in)::superior                     !limite superior del intervalo
    real,intent(in)::e                            !error
    integer, intent(in) :: max_iter               !numero maximo de iteraciones
    integer::i                                    !contador de iteraciones
    real, intent(out) :: root                     !raiz de la ecuacion
    real::a                           
    real::b

    a=inferior
    b=superior



    if (abs(Funcion(a)) < e) then
        root = a
        return
    end if

    if (abs(Funcion(b)) < e) then
        root = b
        return
    end if
    do i = 1, max_iter
        root = b - Funcion(b) * (b - a) / (Funcion(b) - Funcion(a))
        if (abs(Funcion(root)) < e) then
            print *, "Iteraciones del programa", i
            exit
        end if
        a = b
        b = root
    end do
end subroutine secante


subroutine regula_falsi(inferior, superior, e, max_iter, root)

    external :: Funcion                         !Definicion de la funcion a utilizar
    !Definicion de las estructuras de datos que se van a utilizar
    real, intent(in) :: inferior, superior, e           !limite inferior, limite superior, error
    integer, intent(in) :: max_iter                     !numero maximo de iteraciones
    real, intent(out) :: root                           !raiz de la ecuacion
    real :: a, b, c, fa, fb, fc                         !variables auxiliares
    integer :: i                                        !contador de iteraciones                                       


    !Inicializacion de las variables
    a = inferior
    b = superior
    fa = Funcion(a)
    fb = Funcion(b)


    if (fa * fb > 0.0) then
        print *, "Error: La función no cambia de signo en el intervalo dado."
        return
    end if

    do i = 1, max_iter
        c = b - fb * (b - a) / (fb - fa)
        fc = Funcion(c)

        if (abs(fc) < e) then
            root = c
            print *, "Iteraciones del programa", i
            return
        end if
        if (fa * fc < 0.0) then
            b = c
            fb = fc
        else
            a = c
            fa = fc
        end if
    end do

    root = c
    print *, "Número máximo de iteraciones alcanzado."
end subroutine regula_falsi

function Funcion(x) result(fx)
    implicit none
    real, intent(in) :: x
    real :: fx
    real :: L=5.0       !Inductancia
    real :: C=1e-4      !Capacitancia
    real :: t=0.05     !Tiempo
    real :: q_q0=0.01   !relacion de carga

    ! Define the internal function aquí
    fx = EXP(-(t*x)/(2*L))*COS(t*SQRT((1/(L*C))-(x/(2*L))**2)) - q_q0

end function Funcion