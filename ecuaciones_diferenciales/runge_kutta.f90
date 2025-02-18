program main

    implicit none
    real :: xini, xend, xstep, yini, x, ysol
    integer :: nx, i
    real, parameter :: C=0.0001, R=40000, V=5
    real, external :: f,g

    xini=0.
    xend=16.
    xstep=0.1
    nx=INT((xend-xini)/xstep)
    yini=0

    open(1, file="carga_runge_kutta.dat")
    do i=1,nx
        x=xini+xstep*(i-1)
        call runge_kutta_4(f,x, yini, xstep, ysol)
        write(1,*) x+xstep,ysol/C,V*(1-EXP(-((x+xstep)/(R*C))))
        yini=ysol
    end do
    close(1)

    open(2, file="descarga_runge_kutta.dat")
    xini=16.
    xend=32.
    xstep=0.001
    nx=INT((xend-xini)/xstep)
    yini=4.90839911*C
    do i=1,nx
        x=xini+xstep*(i-1)
        call runge_kutta_4(g,x, yini, xstep, ysol)
        write(2,*) x+xstep,ysol/C,4.90839911*EXP(-(((x+xstep)-16)/(R*C)))
        yini=ysol
    end do
    close(2)

end program main

real function f(t,x)
    implicit none
    real::x
    real::t
    real::C=0.0001
    real::R=40000
    real::V=5
    f=-(x-C*V)/(R*C)         !expresion
end function

real function g(t,x)
    implicit none
    real::x
    real::t
    real::C=0.0001
    real::R=40000
    real::V=5
    g=-x/(R*C)         !expresion
end function

real function F1(f,t,x,h)
    real, intent(in) :: t,x,h
    real::f
    external::f
    F1 = h*f(t,x)
end function F1

real function F2(f,t,x,h)
    real, intent(in) :: t,x,h
    real::f
    external::f
    F2 = h*f(t+h/2,x+F1(f,t,x,h)/2)
end function F2

real function F3(f,t,x,h)
    real, intent(in) :: t,x,h
    real::f
    external::f
    F3 = h*f(t+h/2,x+F2(f,t,x,h)/2)
end function F3

real function F4(f,t,x,h)
    real, intent(in) :: t,x,h
    real::f
    external::f
    F4 = h*f(t+h,x+F3(f,t,x,h))
end function F4


subroutine runge_kutta_2(f,x0, y0, h, y)
    real, intent(in) :: x0, y0, h
    real, intent(out) :: y
    real::f
    external::f
    real :: k1, k2
    k1 = F1(f,x0, y0, h)
    k2 = F2(f,x0, y0, h)
    y = y0 + (k1 + k2)/2
end subroutine runge_kutta_2

subroutine runge_kutta_4(f,x0, y0, h, y)
    real, intent(in) :: x0, y0, h
    real, intent(out) :: y
    real::f
    external::f
    real :: k1, k2, k3, k4
    k1 = F1(f,x0, y0, h)
    k2 = F2(f,x0, y0, h)
    k3 = F3(f,x0, y0, h)
    k4 = F4(f,x0, y0, h)
    y = y0 + (k1 + 2*k2 + 2*k3 + k4)/6
end subroutine runge_kutta_4