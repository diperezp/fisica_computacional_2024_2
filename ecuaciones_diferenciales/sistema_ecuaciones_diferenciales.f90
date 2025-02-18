
program main
    implicit none
    real,dimension(4)::x0=[0.0,3.0,1.0,-1.0]  !valores iniciales
    real,dimension(4)::y_sol
    real::h=0.1
    real::t_final=10
    integer::i,nx

    nx=INT((t_final-x0(1))/h)
    !hacemos las iteraciones para resolver el sistema de ecuaciones diferenciales
    open(1, file="sistema_ecuaciones_diferenciales.dat")
    do i=1,nx
        x0=x0(1)+h*(i-1)
        call runge_kutta_4(x0,h,y_sol)
        write(1,*) y_sol
        x0=y_sol
    end do

    contains
    function f(x) result(res)
        real::x(:)
        real::res(size(x))

        res(1)=1
        res(2)=x(3)
        res(3)=x(4)
        res(4)=-2*COS(x(1))+x(2)-(x(3)**2)-(x(4)**2)

    end function f

    function F1(x,h) result(res)
        implicit none
        real::x(:)
        real::res(size(x))
        real::h
        res(:)=h*f(x(1:))
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
        res(:)=h*f(x(:)+F3(x,h))
    end function F4

    subroutine runge_kutta_4(x0,h,y_sol)
        implicit none
        real::x0(:)
        real::h
        real::y_sol(:)
        real::x1(size(x0)),x2(size(x0)),x3(size(x0)),x4(size(x0))
        real,external::f

        x1=x0+F1(x0,h)
        x2=x0+F2(x0,h)
        x3=x0+F3(x0,h)
        x4=x0+F4(x0,h)

        y_sol=x0+(x1+2*x2+2*x3+x4)/6

    end subroutine runge_kutta_4


end program main

