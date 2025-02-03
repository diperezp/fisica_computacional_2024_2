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
!***********************************************************************

program solucionparcial3
    real,allocatable::lin(:,:) !matriz dinamica
    integer::n, m, i,j,stat
    character(len=100)::linea
    ! Definimos las variables necesarias para la subrutina
    real :: tol        ! tolerancia en la solucion del sistema
    logical :: singul  !bandera de singularidad en la solucion
    real, allocatable :: solucion(:) !vactor dinamica que almacena la solucion del sistema lineal

    ! Establecemos la tolerancia
    tol = 1.0e-6

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
    allocate(lin(n,m))
    !cerramos el archivo
    close(1)
    !abrimos nuevamente el archivo
    open(unit=1,file='sistema.txt')
    !almacenamos los datos del archivo en la matriz
    do i=1,n
        read(1,*) (lin(i,j),j=1,m)
    end do
    !cerramos el archivo
    close(1)
    !***********************************************************************************
    !***********************************************************************************



    !imprimos la matriz leida
    print *, "Matriz leida"
    do i=1,n
        print *,(lin(i,j),j=1,m)
    end do



    !************************************************************************************
    !Solucion exacta utilizando el metodo de eliminacion de gauss_jordan
    !************************************************************************************
    ! Llamamos a la subrutina gauss_jordan
    call gauss_jordan(lin, tol, singul, solucion)

    ! Verificamos si la matriz es singular
    if (singul) then
        print *, "La matriz es singular o casi singular"
    else
        print *, "La solucion del sistema es:"
        do i=1,n
            write(*,1)i,(solucion(i))
            1 format(1x,"x(",I2,")=",f8.3)
        end do
    end if
    !*************************************************************************************
    !*************************************************************************************




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
    real,allocatable::solucion(:)
    real::temp
    real::mult
    integer :: i, j, k
    real :: abspiv


    m=size(lin,dim=2)
    n=size(lin,dim=1)
    !inciamos la dimension de solucion
    allocate(solucion(n))

    

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

    subroutine jacobi_seidel(lin,first_solucion) result(solucion)




    end subroutine jacobi_seidel

end program solucionparcial3









