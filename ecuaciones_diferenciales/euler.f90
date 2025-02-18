program main
    implicit none
    real::h,xini,xend,xstep,x,yini,ysol,f,g
    integer::nx,i
    EXTERNAL f,g
    real::C=1e-4
    real::R=40000
    real::V=5
    OPEN(UNIT=1,FILE="car.dat")
        xini=0.
        xend=16.
        xstep=0.001
        nx=INT((xend-xini)/xstep)
        yini=0

        do i=1,nx
            x=xini+xstep*(i-1)
            call euler(f,x,yini,xstep,ysol)
            write(1,*) x+xstep,ysol/C,V*(1-EXP(-((x+xstep)/(R*C))))
            !write(*,*) x+xstep,ysol,(x+xstep)**2*EXP(x+xstep)
            yini=ysol
        end do

    close(1)

    OPEN(UNIT=2,FILE="des.dat")
    xini=16.
    xend=32.
    xstep=0.001
    nx=INT((xend-xini)/xstep)
    yini=4.90839911*C

    do i=1,nx
        x=xini+xstep*(i-1)
        call euler(g,x,yini,xstep,ysol)
        write(2,*) x+xstep,ysol/C,4.90839911*EXP(-(((x+xstep)-16)/(R*C)))
        !write(*,*) x+xstep,ysol,(x+xstep)**2*EXP(x+xstep)
        yini=ysol
    end do

    close(2)
end program main


subroutine euler(f,ti,xi,h,xn)
    implicit none
    real::f
    EXTERNAL f      !expresion
    real::ti         !abcisa   iesima
    real::xi        !ordenada iesima
    real::h         !paso de avance
    real::xn
    xn=xi+f(ti,xi)*h
end subroutine


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


