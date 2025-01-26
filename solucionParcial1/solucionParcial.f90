! ----------------------------------------------------------------------
! Author: Diego Alcides Perez Pineda
! Date: 25/01/2024
! Description:
! ----------------------------------------------------------------------
program SolucionParcial
    !definicion de las estructura de datos a utilizar 
    !------------------------------------------------------
    !definimos dos arreglos de tipo complejo
    complex, dimension(4,5):: mat1, mat2, matsuma
    real, dimension(4,5):: mat_modulo
    real, dimension(5):: vec_fila
    !dimensiones
    integer:: row=4,column=5
    !iteradores
    integer:: i=4,j=5,index=1
    !nombre de los archivos a leer
    character(len=100):: file1, file2
    !------------------------------------------------------

    !Ejecucion del programa
    !------------------------------------------------------

    !leemos la matriz de los arhivos suministrados
    !-------------------------------------
    !asignacion
    file1="mat1.txt"
    file2="mat2.txt"

    !abrimos el archivo mat1
    open (UNIT=1, FILE=file1)

    !leemos los datos de los archivos
    mat1_filas: do i=1,row
        read(1,*) (mat1(i,j),j=1,column)
    end do mat1_filas

    open (UNIT=2,FILE=file2)
    mat2_filas: do i=1,row
        read(2,*) (mat2(i,j),j=1,column)
    end do mat2_filas

    !cerramos ambos archivos
    close(UNIT=1)
    close(UNIT=2)
    !-------------------------------------
    ! Imprimimos las matrices mat1 y mat2
    print *, 'Matriz mat1:'
    do i = 1, row
        print *, (mat1(i, j), j = 1, column)
    end do

    print *, 'Matriz mat2:'
    do i = 1, row
        print *, (mat2(i, j), j = 1, column)
    end do
    
    print *, "suma"
    !-------------------------------------
    !sumamos la dos matrices utilizando la subrutina mat_sum
    call mat_sum(mat1, mat2, matsuma, row, column)

    do i=1,row
        print *,(matsuma(i,j), j=1,column)
    end do
    !-------------------------------------

    !-------------------------------------
    !implimentamos la subrutina mat_mod
    call mat_mod(matsuma,mat_modulo,row,column)

    do i=1,row
        print *,(mat_modulo(i,j), j=1,column)
    end do
    !-------------------------------------

    !-------------------------------------
    !-------------------------------------
    ! Llamamos a la función mini_val y asignamos el resultado a vec_fila
 

    ! Imprimimos el vector fila con la magnitud mínima
    print *, 'Vector fila con la magnitud mínima:'
    print *, mat_modulo(mini_val(mat_modulo, row, column),:)
    print*, mini_val(mat_modulo, row, column)
    !-------------------------------------



end program SolucionParcial


!------------------------------------------------------------
!------------------------------------------------------------
! Subrutina suma
!------------------------------------------------------------
subroutine mat_sum(mat1, mat2, result, row, column)
    integer::row,column
    complex, dimension(row, column), intent(in) :: mat1, mat2
    complex, dimension(row, column), intent(out) :: result

    ! Iteradores
    integer :: i, j

    do i = 1, row
        do j = 1, column
            result(i, j) = mat1(i, j) + mat2(i, j)
        end do
    end do
end subroutine mat_sum
!------------------------------------------------------------


!------------------------------------------------------------
!------------------------------------------------------------
! Subrutina modulo
!------------------------------------------------------------
subroutine mat_mod(mat, result, row, column)
    integer:: row, column
    complex, dimension(row, column), intent(in) :: mat
    real, dimension(row, column), intent(out) :: result

    ! Iteradores
    integer :: i, j

    do i = 1, row
        do j = 1, column
            result(i, j) = abs(mat(i, j))
        end do
    end do
end subroutine mat_mod
!------------------------------------------------------------


!------------------------------------------------------------
!------------------------------------------------------------
! Función mini_val
!------------------------------------------------------------
function mini_val(mat, row, column) result(index)
    implicit none
    integer, intent(in) :: row, column
    real, dimension(row, column), intent(in) :: mat
    real, dimension(column) :: vec_fila

    ! Variables
    integer :: i, j,index
    real :: min_magnitude, current_magnitude

    !calculamos la magnitud de la primera fila
    do j=1,column
        min_magnitude=min_magnitude + mat(1,j)**2
        index=1
    end do
    min_magnitude=sqrt(min_magnitude)

    ! Iteramos sobre las filas restantes
    do i = 2, row
        current_magnitude = 0.0
        do j = 1, column
            current_magnitude = current_magnitude + mat(i, j)**2
        end do
        current_magnitude = sqrt(current_magnitude)
        ! Comparamos y actualizamos si encontramos una magnitud menor
        if (current_magnitude < min_magnitude) then
            min_magnitude = current_magnitude
            index=i
        end if
    end do
end function mini_val
!------------------------------------------------------------
