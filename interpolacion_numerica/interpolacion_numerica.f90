program interpolacion_numerica
    implicit none
    integer::orden=3               !orden del polinomio
    real,allocatable::Xa(:,:)       !arreglo de puntos
    real::x=0.5                     !valor a evaluar
    integer :: i                    !iteradores
    real::y                         !resultado
    real::lagrange_lineal_simple
    real::lagrange_cuadrado_simple
    real::lagrange_simple
    integer::n=256                      !numero de datos
    real::datos(256,2)
    real::abcisa(256)
    real::ordenada(256)
    allocate(Xa(orden+1,2))
    ! allocate(datos(2,n))
    ! allocate(abcisa(n))
    ! allocate(ordenada(n))

    open(unit=10, file='datos.txt', status='old', action='read')
    do i = 1, orden + 1
        read(10, *) Xa(i, 1), Xa(i, 2)
    end do
    close(10)

    print*,lagrange_simple(Xa,orden,1.0)
    !llenamos los datos con el polinomio generado
    do i=1,n
        datos(i,1)=((3.0)/n)*i
        datos(i,2)=lagrange_simple(Xa,orden,datos(i,1))
    end do

    open(UNIT=1,FILE='resultados.txt', status='old', action='write')
    do i=1,n
        WRITE(1,*)datos(i,1),datos(i,2)
    end do
    close(1)





    
end program interpolacion_numerica


real function lagrange_lineal_simple(Xa,x)
    !se ingresa un arreglo con los dos puntos a interpolar y un valor (x) entre x0-x1
    real,dimension(2,2)::Xa
    real::x
    !asiganmos el valor de la interpolacion
    lagrange_lineal_simple=Xa(1,2)*((x-Xa(2,1))/(Xa(1,1)-Xa(2,1)))+Xa(2,2)*((x-Xa(1,1))/(Xa(2,1)-Xa(1,1)))

end function

real function lagrange_cuadrado_simple(Xa,x)
    !se ingresa un arreglo con los dos puntos a interpolar y un valor (x) entre x0-x1
    real,dimension(3,2)::Xa
    real::x
    real::producto=1
    integer::i,j        !iteradores
    lagrange_cuadrado_simple=0

    do i=1,3
        producto=1
        producto=producto*Xa(i,2)
        do j=1,3
            if(i/=j) then
                producto=producto*(((x-Xa(j,1))/(Xa(i,1)-Xa(j,1))))
            end if
        end do
        lagrange_cuadrado_simple=lagrange_cuadrado_simple+producto
    end do

end function


real function lagrange_simple(Xa,orden,x)
    integer::orden                  !orden del polinomio interpolante
    real,dimension(orden+1,2)::Xa   !arreglo que contiene los puntos a interpolar
    real::x                         !valor a evaluar
    
    lagrange_simple=0
    do i=1,orden+1
        producto=1
        producto=producto*Xa(i,2)
        do j=1,orden+1
            if(i/=j) then
                producto=producto*(((x-Xa(j,1))/(Xa(i,1)-Xa(j,1))))
            end if
        end do
        lagrange_simple=lagrange_simple+producto
    end do
end function




