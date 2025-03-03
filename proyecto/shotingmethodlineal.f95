
program main
    implicit none
    real,dimension(3)::x0=[0.0,3.141516/10,0.0]  !valores iniciales
    real,dimension(3)::y_sol
    real::h=0.001
    real::t_final=10
    integer::i,nx
    

    nx=INT((t_final-x0(1))/h)

    print*,F1(x0,h,f)

contains
    function f(x) result(res)
        real::x(:)
        real::res(size(x))

        res(1)=1
        res(2)=x(3)
        res(3)=-(9.8/0.5)*sin(x(2))
    end function f

    function F1(x,h,fun) result(res)
        implicit none
        real::x(:)
        real::res(size(x))
        real::h
        interface
            function fun(x) result(y)
                real, intent(in) :: x(:)
                real :: y(size(x))
            end function fun
        end interface
        res=h*fun(x)
    end function F1

    
end program main

