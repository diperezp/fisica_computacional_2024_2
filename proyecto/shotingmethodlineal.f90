program main
    implicit none
    real,dimension(3)::x0=[1.0,5.0,0.0]
    real,dimension(3)::y_sol
    real::t_final=2.2
    real::h=0.001
    integer:: i,nx

    nx=INT((t_final-x0(1))/h)

    open(1,file='shotingmethodD1.dat')

    do i=1,nx
        call runge_kutta_4(x0,h,f,y_sol)
        x0=y_sol
        write(1,*) x0
    end do
    close(1)

    x0=[1.0,0.0,1.0]

    open(2,file='shotingmethodD2.dat')

    do i=1,nx
        call runge_kutta_4(x0,h,g,y_sol)
        x0=y_sol
        write(2,*) x0
    end do
    close(2)



contains
    function f(x) result(res)
        implicit none
        real, intent(in)::x(:)
        real::res(size(x))

        res(1)=1
        res(2)=x(3)
        res(3)=cos(x(1))-exp(-x(1))*x(3)+atan(x(1))*x(2)
    end function f

    function g(x) result(res)
        implicit none
        real, intent(in)::x(:)
        real::res(size(x))

        res(1)=1
        res(2)=x(3)
        res(3)=cos(x(1))-exp(-x(1))*x(3)+atan(x(1))*x(2)
    end function g

    

    function F1(x,h,fun) result(res)
        implicit none
        real, intent(in)::x(:)
        real,intent(in)::h
        real::res(size(x))
        interface
            function fun(x) result(y)
                real, intent(in)::x(:)
                real:: y(size(x))
            end function fun
        end interface
        res=h*fun(x)
    end function

    function F2(x,h,fun) result(res)
        implicit none
        real, intent(in)::x(:)
        real, intent(in)::h
        real::res(size(x))
        interface
            function fun(x) result(y)
                real, intent(in)::x(:)
                real:: y(size(x))
            end function fun
        end interface
        res=h*fun(x(1:)+0.5*F1(x,h,fun))
    end function F2

    function F3(x,h,fun) result(res)
        implicit none
        real, intent(in)::x(:)
        real, intent(in)::h
        real::res(size(x))
        interface
            function fun(x) result(y)
                real, intent(in)::x(:)
                real:: y(size(x))
            end function fun
        end interface
        res=h*fun(x(1:) + 0.5*F2(x,h,fun))
    end function F3

    function F4(x,h,fun) result(res)
        implicit none
        real, intent(in)::x(:)
        real, intent(in)::h
        real::res(size(x))
        interface
            function fun(x) result(y)
                real, intent(in)::x(:)
                real:: y(size(x))
            end function fun
        end interface
        res=h*fun(x(1:) + F3(x,h,fun))
    end function F4

    subroutine runge_kutta_4(x, h, fun, y_sol)
        implicit none
        real, intent(in):: x(:)
        real, intent(in):: h
        real, intent(out):: y_sol(size(x))
        interface
            function fun(x) result(y)
                real, intent(in):: x(:)
                real:: y(size(x))
            end function fun
        end interface
        real:: k1(size(x)), k2(size(x)), k3(size(x)), k4(size(x))
        k1 = F1(x, h, fun)
        k2 = F2(x, h, fun)
        k3 = F3(x, h, fun)
        k4 = F4(x, h, fun)
        y_sol = x + (k1 + 2*k2 + 2*k3 + k4) / 6.0
    end subroutine runge_kutta_4

    subroutine shootingmethodlineal(x,h,fun,gun,y_sol)
        implicit none
        real, intent(in)::x(:)
        real,intent(in)::h
        real,intent(out)::y_sol(size(x))
        real::y_temp
        interface

        function fun(x) result(y)
            real, intent(in):: x(:)
            real:: y(size(x))
        end function fun

        function gun(x) result(y)
            real, intent(in):: x(:)
            real:: y(size(x))
        end function gun
        end interface


end program main
