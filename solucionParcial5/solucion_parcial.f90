!---------------------------------------------------------------
! Este programa presenta la solucion del parcial 5
!---------------------------------------------------------------
!@author: Diego Alcides Perez
!@date: 2024/02/18
!---------------------------------------------------------------

program main


    implicit none
    real::PI=3.14159265359
    real,dimension(3)::x0                     ! Condiciones iniciales
    real,dimension(3)::y_sol                  ! Solucion
    real::h=0.01                              ! Paso
    real::t_final=3                           ! Tiempo final
    integer::n=0                              ! numero de interaciones
    integer::i                                ! Contador
    real::g=9.8                               ! Aceleracion de la gravedad
    real::l=0.5                               ! Longitud de la cuerda

    !asignamos valores iniciales
    x0=[0.0,PI/6,0.0]
    !calculo del numero de iteraciones
    n=INT((t_final-x0(1))/h)

    !---------------------------------------------------------------
    ! Ciclo para calcular la solucion del sistema de ecuaciones
    OPEN(1,FILE='solucion_parcial5.dat',STATUS='unknown')
        do i=1,n
            call runge_kutta_4(x0,h,y_sol)
            x0=y_sol
            write(1,*) x0,(PI/6)*cos(sqrt(g/l)*x0(1))
        end do
    CLOSE(1)

    !---------------------------------------------------------------
    ! Este contenedor contiene las funciones que se van a utilizar
    ! en el programa principal, ademas evita tener que declarar
    ! las funciones antes de ser utilizadas
    !---------------------------------------------------------------
    contains
    function f(x) result(res)
        real::x(:)
        real::res(size(x))
        real::g=9.8         ! Aceleracion de la gravedad
        real::l=0.5         ! Longitud de la cuerda
        res(1)=1
        res(2)=x(3)
        res(3)=-(g/l)*sin(x(2))
    end function f

    !---------------------------------------------------------------
    ! Funciones para el metodo de Runge-Kutta de cuarto orden
    !---------------------------------------------------------------
    function F1(x,h) result(res)
        implicit none
        real::x(:)
        real::res(size(x))
        real::h
        res=h*f(x)
    end function F1

    function F2(x,h) result(res)
        implicit none
        real::x(:)
        real::res(size(x))
        real::h
        res(:)=h*f(x(1:)+F1(x(1:),h)/2)
    end function F2

    function F3(x,h) result(res)
        implicit none
        real::x(:)
        real::res(size(x))
        real::h
        res(:)=h*f(x(:)+F2(x,h)/2)
    end function F3

    function F4(x,h) result(res)
        implicit none
        real::x(:)
        real::res(size(x))
        real::h
        res(:)=h*f(x+F3(x,h))
    end function F4

    !---------------------------------------------------------------
    ! Metodo de Runge-Kutta de cuarto orden
    !---------------------------------------------------------------
    subroutine runge_kutta_4(x0,h,y_sol)
        implicit none
        real::x0(:)
        real::h
        real::y_sol(:)
        real::x1(size(x0)),x2(size(x0)),x3(size(x0)),x4(size(x0))
        real,external::f
        x1=F1(x0,h)
        x2=F2(x0,h)
        x3=F3(x0,h)
        x4=F4(x0,h)
        y_sol=x0+(x1+2*x2+2*x3+x4)/6
    end subroutine runge_kutta_4
end program main