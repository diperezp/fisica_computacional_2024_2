!***********************************************************************
!@author:diperezp
!@date: 2025-02-11
!@description: Este programa presenta la solucion del parcial 4
!***********************************************************************
program solucionParcial4

    implicit none
    real::PI=3.14159265                 !pi
    real::a                             !limite inferior del intervalo de integracion
    real::b                             !limite superior del intervalo de integracion
    integer::n=1000                     !numero de interaciones
    real::y                             !solucion
    real::L=0.5                         !longitud de la cuerda del pendulo (m)
    real::g=9.8                         !aceleracion de la gravedad
    real::integral_simpson_abierto
    real::aproximacion_paraxial
    integer::i
    real,dimension(8,2)::tabla
    real::lagrange_lineal_simple



    !-----------------------------------------------------------------
    !--------1er
    !-----------------------------------------------------------------

    !intervalo de integracion
    a=0
    b=PI/18

    !aproximacion paraxial
    aproximacion_paraxial=2*PI*SQRT(L/g)/4

    y=integral_simpson_abierto(a,b,n,b)*SQRT(L/(2*g))
    write(*,*)"Tiempo:",y
    write(*,*)"Error simpson:",(abs(aproximacion_paraxial-y)/aproximacion_paraxial)*100

    !-----------------------------------------------------------------
    !-------2ndo
    !-----------------------------------------------------------------
    !tabla de valores
    write(*,*)"Tabla de valores"
    open(UNIT=1,FILE='tabla.txt')
        do i=1,8
            tabla(i,1)=b*i
            tabla(i,2)=integral_simpson_abierto(a,b*i,n,b*i)*SQRT(L/(2*g))
            write(1,*)tabla(i,1),tabla(i,2)
            write(*,*)tabla(i,1),tabla(i,2)
        end do
    close(1)

    !-----------------------------------------------------------------
    !-------3ero
    !-----------------------------------------------------------------
    write(*,*)"Interpolacion"
    write(*,*)"Solucion:",lagrange_lineal_simple(tabla(1:2,1:2),b*1.5)
end program solucionParcial4


!---------------------------------------------------------------------
!Funcion de temporal
!---------------------------------------------------------------------
real function f(x,theta)
    real::x                 !valor a evaluar
    real::theta             !angulo inicial
    real::PI=3.14159265     !Pi
    f=1/SQRT(cos(x)-cos(theta)) !expresion 
end function


!---------------------------------------------------------------------
!Integracion numerica abierta por la derecha
!---------------------------------------------------------------------
real function integral_simpson_abierto(a,b,n,theta)
    real::a             !limite inferior del intervalo de integracion
    real::b             !limite superior del intervalo de integracion
    integer::n          !numero de subrectangulos
    real::h             !paso de integracion
    real::suma          !suma
    real::theta         !angulo incial

    !asignamos el valor del paso de integracion
    h=(b-a)/n

    !integramos numericamente la expresion
    suma=(f(a,theta))*(h/3)
    do i=1,n-1
        if (mod(i, 2) == 0) then
            suma = suma + 2 * f(a + i * h,theta)*(h/3)
        else
            suma = suma + 4 * f(a + i * h,theta)*(h/3)
        end if
    end do
    suma=suma
    integral_simpson_abierto=suma

end function

real function lagrange_lineal_simple(Xa,x)
    !se ingresa un arreglo con los dos puntos a interpolar y un valor (x) entre x0-x1
    real,dimension(2,2)::Xa
    real::x
    !asiganmos el valor de la interpolacion
    lagrange_lineal_simple=Xa(1,2)*((x-Xa(2,1))/(Xa(1,1)-Xa(2,1)))+Xa(2,2)*((x-Xa(1,1))/(Xa(2,1)-Xa(1,1)))

end function