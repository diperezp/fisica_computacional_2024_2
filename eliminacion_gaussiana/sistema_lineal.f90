program sistema_lineal
    IMPLICIT NONE

    !limrow: numero maximo de filas en la matriz
    !limcol: numero maximo de columnas en la matriz
    !n     : numero de incognitas
    !i,j   : iteradores
    !lin   : matriz del sistema lineal de ecuaciones
    !singul: bandera del proceso
    !
    !ENTRADA: numero de ecuaciones y la matriz del sistema
    !SALIDA : vector solucion del sistema o un mensaje de error indicando que el sistema es cercanamente singular

    INTEGER, PARAMETER :: limrow = 10, limcol = limrow + 1
    real(4):: lin(limrow,limcol),x(limrow)
    INTEGER :: n, i, j
    LOGICAL :: singul

    !se leen el numero de incognitas y la matriz del sistema
    write(*,*) 'Introduce el numero de incognitas(n): '
    read(*,*) n
    entradas:do i=1,n
        write(*,*) 'Introduce los coeficientes de la ecuacion ',i
        read(*,*) (lin(i,j),j=1,n+1)
    end do entradas

    !se llama la subrutina gauss para encontrar la solucion y desplegarla
    call gauss(lin,limrow,limcol,n,x,singul)
    if(.not. singul) then
        write(*,*) 'La solucion del sistema es: '
        do i=1,n
            write(*,1)i,x(i)
        1   format(1x,"x(",I2,")",f8.3)
        end do
    else
        write(*,*) 'El sistema es cercanamente singular'
    end if
    write(*,*) 'Fin del programa'
end program sistema_lineal

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!gauss: subrutina que resuelve un sistema de ecuaciones lineales mediante
!       el metodo de eliminacion gaussiana
!-----------------------------------------------------------------------
subroutine gauss(lin,limrow,limcol,n,x,singul)
    real(4):: lin(limrow,limcol),x(limrow),temp,mult
    real(4),PARAMETER::tol=1.0e-6
    integer:: n,pivrow
    logical:: singul
    singul=.false.

    !se realiza la eliminacion gaussiana
    do i=1,n
        !localiza el pivote
        abspiv=abs(lin(i,i))
        pivrow=i
        do k=i+1,n
            if(abs(lin(k,i))>abspiv) then
                abspiv=abs(lin(k,i))
                pivrow=k
            end if
        end do

        !chequea si la matriz es cercanamente singular
        if(abspiv<tol) then
            singul=.true.
            return
        end if

        !si no es cercanamente singular, entonces intercambia las filas pivrow e i si es necesario
        if (pivrow/=i) then
            do j=i,n+1
                temp=lin(i,j)
                lin(i,j)=lin(pivrow,j)
                lin(pivrow,j)=temp
            end do
        end if
        !elimina la variable i-esima de las ecuaciones restantes i+1,...,n
        do j=i+1,n
            mult=-lin(j,i)/lin(i,i)
            do k=i,n+1
                lin(j,k)=lin(j,k)+mult*lin(i,k)
            end do
        end do
    end do

    !Encuentra la solucion por sustitucion regresiva
    x(n)=lin(n,n+1)/lin(n,n)
    do i=n-1,1,-1
        x(i)=lin(i,n+1)
        do j=i+1,n
            x(i)=x(i)-lin(i,j)*x(j)
        end do
        x(i)=x(i)/lin(i,i)
    end do
end subroutine gauss