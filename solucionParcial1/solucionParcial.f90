program SolucionParcial
    !definicion de las estructura de datos a utilizar 
    !------------------------------------------------------
    !definimos dos arreglos de tipo complejo
    complex, dimension(4,5):: mat1, mat2, matsuma
    real, dimension(4,5):: mat_modulo
    !dimensiones
    integer:: row=4,column=5
    !iteradores
    integer:: i=4,j=5
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

    !-------------------------------------
    !sumamos la dos matrices utilizando la subrutina mat_sum
    call mat_sum(mat1, mat2, matsuma, row, column)

    do i=1,row
        print *,(matsuma(i,j), j=1,column)
    end do
    !-------------------------------------

    !-------------------------------------
    !implimentamos la subrutina mat_mod
    call mat_mod(mat1,mat_modulo,row,column)
    do i=1,row
        print *,(mat_modulo(i,j), j=1,column)
    end do



end program SolucionParcial


!------------------------------------------------------------
!------------------------------------------------------------
! Subrutina suma
!------------------------------------------------------------
subroutine mat_sum(mat1, mat2, result, row, column)
    integer::row,column
    complex, dimension(row, column), intent(in) :: mat1, mat2
    real, dimension(row, column), intent(out) :: result

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
