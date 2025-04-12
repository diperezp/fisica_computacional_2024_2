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
    real::epsil=1e-6
    Xa=reshape([0.0,PI/2,0.0,1.0],[2,2])


    call shootingmethodnolineal(dataset,f,Xa,h,epsil)

contains
    function f(x) result(res)
        implicit none
        real, intent(in)::x(:)
        real::res(size(x))

        res(1)=1
        res(2)=x(3)
        res(3)=-50*x(2)
    end function f

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

    subroutine solucion_runge_kutta_4(dataset,nx,X0,h,fun)
        implicit none
        character(len=*),intent(in)::dataset            !nombre del archvio donde se almacenara los datos
        integer,intent(in)::nx                          !numero de iteraciones 
        real,intent(in)::h                              !paso de la solucion de la ecuacion
        real,intent(in)::X0(:)                          !condiciones iniciales
        real::x1(size(X0))                              !evaluacion
        integer::i                                      !iterador
        real::y_sol(size(X0))

        
        interface
            function fun(x) result(y)
                real, intent(in)::x(:)
                real:: y(size(x))
            end function fun
        end interface

        x1=X0
        open(1,file=dataset)
            do i=1,nx
                call runge_kutta_4(x1,h,fun,y_sol)
                x1=y_sol
                write(1,*)x1
            end do
            rewind(1)
        close(1)
    end subroutine

    subroutine shootingmethodnolineal(dataset,fun,Xa,h,epsil)
        implicit none
        character(len=*),intent(in)::dataset   !archivo donde se almacenara los resultados de la solucion de la ecuacion diferencial
        real,intent(in)::Xa(:,:)               !valores de frontera
        real,intent(in)::h                     !paso con el que se desea resolver la ecuacion diferencial
        real,intent(in)::epsil                 !tolerancia en la solucion de la ecuacion
        integer::i,nx                          !iteradores y numero de iteraciones
        real::X01(size(Xa(1,:))+1)             !condiciones iniciales para PVI1
        real::X02(size(Xa(1,:))+1)             !condiciones iniciales para PVI2
        real::y_sol(size(Xa(1,:))+1)           !variable temporal
        real::y_sol_temp(size(Xa(1,:))+1)      !variable temporal
        real::temp                             !variable temporal
        real::shoot
        real::shoot0                           !angulo de tiro inicial
        real::shoot1                           !angulo de tiro secundario
        real::sol_shoot0                       !evaluacion del tiro inicial
        real::sol_shoot1                       !evaluacion del tiro secundario
        integer::n=100                         !numero de iteraciones que se van a aplicar en el metodo de la secante
        integer::estado1,estado2,estado3
        character(len=100)::PVI1='shotingmethodnolinealD1.dat'


                
        !interface de los campos que representan el sistema de ecuaciones diferenciales PVI
        interface
            function fun(x) result(y)
                real, intent(in)::x(:)
                real:: y(size(x))
            end function fun
        end interface

        !establecemos el numero de iteraciones
        nx=INT((Xa(2,1)-Xa(1,1))/h)
        ! Evaluar si nx es par y si no sumarle 1
        if (mod(nx, 2) /= 0) then
            nx = nx + 1
        end if

        !establecemos el primer angulo de tiro
        shoot0=(Xa(2,2)-Xa(1,2))/(Xa(2,1)-Xa(1,1))
        shoot1=2*shoot0

        !asignamos los valores iniciales de las ecuaciones diferenciales
        X01=[Xa(1,1),Xa(1,2),shoot0]
        X02=[Xa(1,1),Xa(1,2),shoot1]

        !resolvemos la ecuacion diferencial
        call solucion_runge_kutta_4(PVI1,nx,X01,h,fun)
        !evaluamos la ecuacion diferencial en b
        sol_shoot0=regresion_lineal(PVI1,Xa(2,1),nx)-Xa(2,2)
        !resolvemos la ecuacion diferencial
        call solucion_runge_kutta_4(PVI1,nx,X02,h,fun)
        !evaluamos la ecuacion diferencial en b
        sol_shoot1=regresion_lineal(PVI1,Xa(2,1),nx)-Xa(2,2)

        !aplicamos el metodo de la secante
        do i=1,n
            shoot=shoot0-((shoot0-shoot1)/(sol_shoot0-sol_shoot1))*sol_shoot0

            !evaluamos el error 
            if(abs(regresion_lineal(PVI1,Xa(2,1),nx)-Xa(2,2))<epsil) then
                exit
            end if
            shoot0=shoot1
            shoot1=shoot
            sol_shoot0=sol_shoot1
            !resolvemos la ecuacion para el numero angulo de tiro
            X02=[Xa(1,1),Xa(1,2),shoot1]
            !resolvemos la ecuacion diferencial
            call solucion_runge_kutta_4(PVI1,nx,X02,h,fun)
            !evaluamos la ecuacion diferencial en b
            sol_shoot1=regresion_lineal(PVI1,Xa(2,1),nx)-Xa(2,2)
        end do

        !resolvemos la ecuacion para el angulo de tiro con menor error
        X02=[Xa(1,1),Xa(1,2),shoot]
        call solucion_runge_kutta_4(dataset,nx,X02,h,fun)
    end subroutine shootingmethodnolineal

end program main
