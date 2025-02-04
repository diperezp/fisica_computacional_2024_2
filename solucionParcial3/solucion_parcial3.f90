!***********************************************************************
!@author:diperezp
!@date: 2021-06-10
!@description: Este programa presenta la solucion del parcial 3 
!***********************************************************************
!esta es la matriz que se debe de ingresar en el archivo para el implementar 
!la solucion del problema planteado en el parcial
!1.0 0.0 70.0 636.0
!1.0 -1.0 -60.0 -518.0
!0.0 1.0 -40.0 307.0
!sin embargo para poder implementar el metodo jacobi_seidel esta debe de ser diagonal dominante
!8.0 -3.0 0.0 779.0
!1.0 -2.5 0.0 -978.5
!0.0 1.0 -40.0 307.0
!***********************************************************************

program solucionparcial3
    real,allocatable::lin_gauss(:,:) !matriz dinamica
    real,allocatable::lin_seidel(:,:) !matriz dinamica
    integer::n, m, i,j,stat
    character(len=100)::linea
    ! Definimos las variables necesarias para la subrutina
    real :: tol                                         ! tolerancia en la solucion del sistema
    logical :: singul                                   !bandera de singularidad en la solucion
    real, allocatable :: solucion_gauss(:)              !vector dinamica que almacena la solucion del sistema lineal aplicando metodo de gauss_jordan
    real,allocatable::solucion_seidel(:)                !vector dinamico que almacena la solucion del sistema lineal aplicando el metodo de jacobi_seidel
    real,allocatable::xold(:)                           !Vector dinamica que alamacena la solucion inicial utilizada en el metodo de jacobi_seidel
    integer::numits=100                                 !numero de iteraciones

    ! Establecemos la tolerancia
    tol = 1.0e-6                                        !tolerancia

    !************************************************************************************
    !Preprocesamiento de los datos
    !************************************************************************************
    !abrimos el archivo que contiene la matriz del sistema lineal
    open(unit=1,file='sistema.txt')
    !determinamos la dimensiones de la matriz
    n=0
    m=0
    do 
        read(1,'(A)',iostat=stat)linea
        if(stat/=0) exit
        n=n+1
        !contamos la columnas en la primera fila
        if(n==1)then
            m=count_columns(linea)
        end if
    end do

    !verificamos que el archivo si tuviese una matriz
    if(n==0 .or. m==0) then
        write(*,*)"El archivo esta vacio o no tiene datos validos"
        stop
    end if
    !asignamos memoria a la matriz
    allocate(lin_gauss(n,m))
    allocate(lin_seidel(n,m))
    !asignamos memoria a los vectores
    allocate(solucion_gauss(n))
    allocate(solucion_seidel(n))
    allocate(xold(n))
    !cerramos el archivo
    close(1)
    !abrimos nuevamente el archivo
    open(unit=1,file='sistema.txt')
    !almacenamos los datos del archivo en la matriz
    do i=1,n
        read(1,*) (lin_gauss(i,j),j=1,m)
    end do
    !cerramos el archivo
    close(1)
    !asignamos el valor de la matriz lin_seidel
    lin_seidel=lin_gauss
    !***********************************************************************************
    !***********************************************************************************

    
    
    !imprimos la matriz leida
    print *, "Matriz leida"
    do i=1,n
        print *,(lin_gauss(i,j),j=1,m)
    end do
    write(*,*)"*********************************"


    write(*,*) "Implementacion del metodo gauss_jordan"
    !************************************************************************************
    !Solucion exacta utilizando el metodo de eliminacion de gauss_jordan
    !************************************************************************************
    ! Llamamos a la subrutina gauss_jordan
    call gauss_jordan(lin_gauss, tol, singul, solucion_gauss)

    ! Verificamos si la matriz es singular
    if (singul) then
        print *, "La matriz es singular o casi singular"
    else
        print *, "La solucion del sistema es:"
        do i=1,n
            write(*,1)i,(solucion_gauss(i))
            1 format(1x,"x(",I2,")=",f8.3)
        end do
    end if
    !*************************************************************************************
    !*************************************************************************************
    write(*,*)"*********************************"


    write(*,*) "Implementacion del metodo jacobi_seidel"
    !*************************************************************************************
    !Solucion aproximada utilizando el metodo iterativo jacobi_seidel
    !*************************************************************************************
    !pedimos por consola la primera aproximacion del sistema
    write(*,*)"Introduce las soluciones iniciales"
    read(*,*) (xold(i),i=1,n)
    !Llamamos a la subrutina jacobi_seidel
    call jacobi_seidel(lin_seidel,xold,tol,numits,solucion_seidel)

    !presentamos la solucion del sistema
    write(*,*) "La solucion del sistema es:"
    do i=1,n
        write(*,1)i,(solucion_seidel(i))
    end do
    !************************************************************************************
    !************************************************************************************
    write(*,*)"*********************************"


    write(*,*)"Comparacion entre los dos resultados"
    !************************************************************************************
    !Comparacion entre los dos metodos
    !************************************************************************************
    do i=1,n
        write(*,2)i,((abs(solucion_seidel(i)-solucion_gauss(i))/solucion_gauss(i))*100)
        2 format(1x,"Error ",I2,"-esima componente:",f8.5,"%")
    end do
    !************************************************************************************





!*****************************************************************************************
!Contenedor de las funciones y las subrutinas para uso en el programa
!*****************************************************************************************
contains
    !-------------------------------------------------------------------------------------
    !Funcion count_columns esta funcion cuenta las columnas que existen en un arrreglo 
    !ingresado desde un archivo
    !---------------------------------------------------------------------------------------
    function count_columns(linea) result(m)
        character(len=*), intent(in) :: linea
        integer :: m
        integer :: pos, last_pos
        integer :: lenght
        lenght=len_trim(linea)
        m = 0
        last_pos = 1
        pos = index(linea(last_pos:), ' ')  ! Buscar espacios

        do while (pos > 0 .and. last_pos<lenght)
            m = m + 1
            last_pos = last_pos + pos
            pos = index(linea(last_pos:), ' ')
        end do

        ! Asegurarse de contar la última columna
        if (len_trim(linea(last_pos:)) > 0) then
            m = m + 1
        end if
    end function count_columns
    !---------------------------------------------------------------------------------------
    !---------------------------------------------------------------------------------------

    !---------------------------------------------------------------------------------------
    !Metodo de elimiacion matricial gauss_jordan
    !---------------------------------------------------------------------------------------
    subroutine  gauss_jordan(lin,tol,singul,solucion)
        implicit none
        !Defincion de los paramentros de entrada
        real:: lin(:,:)
        real::tol
        logical:: singul
        integer:: n,m,pivrow
        real::solucion(:)
        real::temp
        real::mult
        integer :: i, j, k
        real :: abspiv


        m=size(lin,dim=2)
        n=size(lin,dim=1)

    

        !se realiza la eliminacion gauss_jordaniana
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
            !divide la fila i por el pivote
            lin(i,i:n+1)=lin(i,i:n+1)/lin(i,i)

            !elimina la variable i-esima de las ecuaciones restantes i+1,...,n
            do j=i+1,n
                mult=-lin(j,i)/lin(i,i)
                do k=i,n+1
                    lin(j,k)=lin(j,k)+mult*lin(i,k)
                end do
            end do
        
        end do

        !elimina la variable i-esima de las ecuaciones restantes i+1,...,n
        do i=n,2,-1
            do j=i-1,1,-1
                mult=-lin(j,i)/lin(i,i)
                do k=i,n+1
                    lin(j,k)=lin(j,k)+mult*lin(i,k)
                end do
            end do
        end do

        solucion=lin(:,m)
    end subroutine gauss_jordan
    !----------------------------------------------------------------------------------------
    !----------------------------------------------------------------------------------------

    !----------------------------------------------------------------------------------------
    !Metodo de aproximacion jacobi_seidel
    !----------------------------------------------------------------------------------------
    subroutine jacobi_seidel(lin,xold,tol,numits,xnew)

        !definicion de las estructuras que se van a utilizar en la implementacion del metodo jacobi_seidel
        implicit none
        real::lin(:,:)                      !matriz del sistema 
        real,allocatable::intercepcion(:)   !vector que almacena la intercepcion del sistema
        real::xnew(:)                       !vector dinamico (solucion i+1-esima)
        real::xold(:)                       !vector dinamico (solucion i-esima)
        real::suma,tol                      !variable auxiliar y tolerancia de la solucion
        logical::done                       !bandera
        integer::row,column                 !dimensiones de la matriz de entrada
        integer::n,i,j                      !contadores e iteradores
        integer::numits

        !estados inciales
        done=.false.
        n=0

        !establecemos la dimension de la matriz
        row=size(lin,dim=1)
        column=size(lin,dim=2)

        !Separamos a memoria para los arreglos dianamicas
        allocate(intercepcion(row))

        intercepcion=lin(:,column)

        !inicializamos el vector xnew
        xnew=xold


        do while(.not. done)
            n=n+1
            do i=1,row
                suma=intercepcion(i)
                !suma de los productos de los coeficientes y las soluciones anteriores al indice i
                do j=1,i-1
                    suma=suma-lin(i,j)*xnew(j)
                end do
                !suma de los productos de los coeficientes y las soluciones posteriores al indice i
                do j=i+1,row
                    suma=suma-lin(i,j)*xold(j)
                end do
                xnew(i)=suma/lin(i,i)
            end do

            !ceque si la condicion de convergencia se satisface 
            i=1
            done=abs(xold(i)-xnew(i))<tol
            
            !mientras done=false e i<row
            do while(.not. done .and. i<row)
                i=i+1
                done=abs(xold(i)-xnew(i))<tol
            end do
            
            done =done .or. n>numits

            !copia xnew en xold
            xold=xnew
        end do


    end subroutine jacobi_seidel
    !----------------------------------------------------------------------------------------
    !----------------------------------------------------------------------------------------

end program solucionparcial3
