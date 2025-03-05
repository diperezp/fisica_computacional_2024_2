program main
    implicit none
    real,dimension(3)::x0=[1.0,5.0,0.0]
    real,dimension(3)::y_sol
    real::t_final=2.2
    real::PI=3.14159265
    real::h=0.001
    integer:: i,nx
    character(len=100)::dataset='solucion.dat'
    real::res
    real,dimension(2,2)::Xa
    Xa=reshape([0.0,PI/2,0.0,1.0],[2,2])


    call shootingmethodlineal(dataset,f,g,Xa,h)

contains
    function f(x) result(res)
        implicit none
        real, intent(in)::x(:)
        real::res(size(x))

        res(1)=1
        res(2)=x(3)
        res(3)=-25*x(2)
    end function f

    function g(x) result(res)
        implicit none
        real, intent(in)::x(:)
        real::res(size(x))

        res(1)=1
        res(2)=x(3)
        res(3)=-25*x(2)
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

    real function lagrange_lineal_simple(Xa,x)
        !se ingresa un arreglo con los dos puntos a interpolar y un valor (x) entre x0-x1
        real,dimension(2,2)::Xa
        real::x
        !asiganmos el valor de la interpolacion
        lagrange_lineal_simple=Xa(1,2)*((x-Xa(2,1))/(Xa(1,1)-Xa(2,1)))+Xa(2,2)*((x-Xa(1,1))/(Xa(2,1)-Xa(1,1)))
    end function

    function regresion_lineal(Dataset,x,nx) result(res)
        implicit none
        character(len=100),intent(in)::Dataset
        real,intent(in)::x
        integer,intent(in)::nx
        integer::i
        real::res
        integer::estado
        real,dimension(2,3)::Xa
        !abrimos el documento donde esta almacenado el dataset
        open(unit=5,file=Dataset,iostat=estado)
        
        !recorremos el data set hasta encontrar un valor mayor o igual a x
        do i=1,nx,2
            !leemos los datos de 1 en 1
            if(estado/=0) then
                exit
            end if

            read(5,*) Xa(1,1),Xa(1,2),Xa(1,3)
            read(5,*) Xa(2,1),Xa(2,2),Xa(2,3)

            if(Xa(2,1)>=x) then
                exit
            end if
            
        end do
        if(estado/=0) then
         !print*,"el valor no esta dentro los limites del dataset"
        else
            res= lagrange_lineal_simple(Xa(:,1:),x)
        end if
        close(unit=5)
    end function regresion_lineal

    subroutine shootingmethodnolineal(dataset,fun,gun,Xa,h)
        implicit none
        character(len=*),intent(in)::dataset   !archivo donde se almacenara los resultados de la solucion de la ecuacion diferencial
        real,intent(in)::Xa(:,:)               !valores de frontera
        real,intent(in)::h                     !paso con el que se desea resolver la ecuacion diferencial
        integer::i,nx                             !iteradores y numero de iteraciones
        real::X01(size(Xa(1,:))+1)             !condiciones iniciales para PVI1
        real::X02(size(Xa(1,:))+1)             !condiciones iniciales para PVI2
        real::y_sol(size(Xa(1,:))+1)           !variable temporal
        real::y_sol_temp(size(Xa(1,:))+1)           !variable temporal
        real::temp                             !variable temporal
        real::temp1
        integer::estado1,estado2,estado3
        character(len=100)::PVI1='shotingmethodD1.dat'
        character(len=100)::PVI2='shotingmethodD2.dat'


                
        !interface de los campos que representan el sistema de ecuaciones diferenciales PVI
        interface
            function fun(x) result(y)
                real, intent(in)::x(:)
                real:: y(size(x))
            end function fun

            function gun(x) result(y)
                real, intent(in)::x(:)
                real:: y(size(x))
            end function gun
        end interface
        !establecemos la condiciones inciales para cada PVI
        X01=[Xa(1,1),Xa(1,2),0.0]
        X02=[Xa(1,1),0.0,1.0]
        !establecemos el numero de iteraciones
        nx=INT((Xa(2,1)-Xa(1,1))/h)
        ! Evaluar si nx es par y si no sumarle 1
        if (mod(nx, 2) /= 0) then
            nx = nx + 1
        end if

        !resolvemos los sistemas de ecuaciones diferenciales PVI
        !almacenados en documento separados
        open(1,file=PVI1)
            do i=1,nx
                call runge_kutta_4(X01,h,fun,y_sol)
                X01=y_sol
                write(1,*) X01
            end do
            rewind(1)
        close(1)

        open(2,file=PVI2)
            do i=1,nx
                call runge_kutta_4(X02,h,gun,y_sol)
                X02=y_sol
                write(2,*) X02
            end do
            rewind(2)
        close(2)
        temp=(Xa(2,2)-regresion_lineal(PVI1,Xa(2,1),nx))/regresion_lineal(PVI2,Xa(2,1),nx)
        print*,temp
        print*,Xa(2,2)
        temp1=regresion_lineal(PVI2,Xa(2,1),nx)
        print*,1/temp1

        open(7,file=PVI1,iostat=estado1)
        open(8,file=PVI2,iostat=estado2)
        open(9,file=dataset,iostat=estado3)

        do i=1,nx
            if(estado1/=0 .or. estado2/=0 .or. estado3/=0) then
                exit
            end if
            read(7,*)y_sol_temp(1),y_sol_temp(2),y_sol_temp(3)
            read(8,*)y_sol
            write(9,*)y_sol(1),y_sol_temp(2:)+temp*y_sol(2:)
        end do


        close(7)
        close(8)
        close(9)


    end subroutine shootingmethodlineal

end program main
