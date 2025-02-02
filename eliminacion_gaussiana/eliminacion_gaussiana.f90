
!-----------------------------------------------------------------------
!  @author: diperezp
!  @date: 2019-08-29
!  @version: 1.0
!  @description: Programa que resuelve un sistema de ecuaciones lineales
!                mediante el método de eliminación gaussiana.
!-----------------------------------------------------------------------
program eliminacion_gaussiana
    real, dimension(4,5) :: A
    integer :: n

    ! Definimos la matriz 4x5
    
    A = reshape([4,4,-5,2,7], [3,3,5,-1,9], [2,1,-1,1,4], [-1,1,-1,1,1]], [4,5])

    n = 4
    print *, "Matriz original:"
    do i=1,n
        print *, A(i,:)
    end do

    ! Llamamos a la subrutina para realizar la eliminación gaussiana
    call lin(A, n)

    ! Imprimimos la matriz resultante
    print *, "Matriz después de la eliminación gaussiana:"
    do i=1,n
        print *, A(i,:)
    end do

end program eliminacion_gaussiana

subroutine lin(A,n)
    integer,intent(in) :: n
    real, dimension(n,n+1), intent(inout) :: A
    integer :: i, j, k
    real :: factor
    real :: mayor
    real, dimension(n) :: temp

    !recorremos las filas de la matriz
    do i=1,n
        !buscamos el mayor elemento de la columna i
        mayor = abs(A(i,i))
        do j=i+1,n
            if (abs(A(j,i)) > mayor) then
                mayor = abs(A(j,i))
                temp=A(i,:)
                A(i,:)=A(j,:)
                A(j,:)=temp
            end if
        end do
        !hacemos ceros en la columna i
        do j=i+1,n
            if(A(i,i) /= 0.0 .and. A(j,i) /= 0.0) then
                factor = A(j,i)/A(i,i)
                A(j,:) = A(j,:) - factor*A(i,:)
            end if
        end do
    end do
end subroutine lin