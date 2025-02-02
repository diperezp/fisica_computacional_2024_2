! Programa para encontra una solucion aproximada a un sistema 
! lineal Ax=b usando el metodo de Jacobi_Seidel. Proceso iterativo
! se realiza hasta que un numero maximo de iteraciones 
! o si dos aproximaciones sucesivas difieren en cada compononente
! a lo sumo por ina valor epsil.
! Entrada: numeqn,a,b,numits,epsil,xold
! Salida: una secuencia de soluciones aproximadas a un mensaje
! de error indicando que el sistema es cercanamente singular
!-----------------------------------------------------------------------
program Jacobi_Seidel

IMPLICIT NONE
!lim : numero maximo de ecuaciones
!numeqn : numero de ecuaciones(numero de incognitas)
!a : matriz de coeficientes
!b : vector de terminos independientes
!numits : numero maximo de iteraciones
!epsil : tolerancia
!done : bandera del proceso
!xold : vector de soluciones anteriores
!xnew : vector de soluciones actuales
!suma : suma de los productos de los coeficientes y las soluciones
!i,j : iteradores
!n : numero de iteraciones

integer::numeqn,numits,i,j,n
integer,parameter::lim=10
real::a(lim,lim),b(lim),xold(lim),xnew(lim)
real::suma,epsil
logical::done

write(*,*) 'Introduce el numero de ecuaciones(numeqn): '
read(*,*) numeqn
write(*,*) 'Introduce los coeficientes matriciales  (fila a fila) '
do i=1,numeqn
    read(*,*) (a(i,j),j=1,numeqn)
end do

write(*,*) 'Introduce el vector constante'
read (*,*) (b(i),i=1,numeqn)

write(*,*) 'Introduce la tolerancia(epsil) y el maximo de iteraciones(numits)'
read(*,*) epsil,numits

write(*,*) 'Introduce las soluciones iniciales'
read(*,*) (xold(i),i=1,numeqn)

done=.false.
n=0

!iniciamos xnew
xnew=xold

do while(.not. done)
    n=n+1
    do i=1,numeqn
        suma=b(i)
        !suma de los productos de los coeficiente y las soluciones anteriores al indice i
        do j=1,i-1
            suma=suma-a(i,j)*xnew(j)
        end do
        !suma de los productos de los coeficientes y las soluciones posteiores al indice i
        do j=i+1,numeqn
            suma=suma-a(i,j)*xold(j)
        end do
        xnew(i)=suma/a(i,i)
    end do


    !cheque si la condicion de convergencia se satisfacce
    i=1
    done =abs(xold(i)-xnew(i))<epsil
    !mientras done =false e i<numeqn, realice
    do while(.not. done .and. i<numeqn)
        i=i+1
        done=abs(xold(i)-xnew(i))<epsil
    end do
    done=done .or. n>numits



    !despliegue en la consola una solucion aproximada
    write(*,*)
    WRITE(*,*) 'ITERACIONES #: ',n
    do i=1,numeqn
        write(*,1) "x(",i,")=",xnew(i)
    1   format(1x,a2,I3,a6,f8.4)
    end do

    !copia xnew en xold 
    do i=1,numeqn
        xold(i)=xnew(i)
    end do

end do



end program Jacobi_Seidel