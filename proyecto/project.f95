PROGRAM QuantumShooting
  IMPLICIT NONE

  ! Constantes físicas en unidades SI
  DOUBLE PRECISION, PARAMETER :: HBAR = 1.054571817D-34  ! J·s
  DOUBLE PRECISION, PARAMETER :: ME = 9.10938356D-31  ! kg (masa del electrón)
  DOUBLE PRECISION, PARAMETER :: HBAR2M = HBAR**2 / (2.0D0 * ME)  ! J·m²

  DOUBLE PRECISION :: E_found, E0

  ! Estimación inicial de la energía en julios (J)
  E0 = 1.0D-19  ! Valor positivo ya que el pozo infinito no tiene estados ligados negativos
  
  ! Buscar el valor propio de energía usando el método de Newton-Raphson
  E_found = NewtonRaphson(E0)

  PRINT *, "Energía propia encontrada (J):", E_found
  PRINT *, "Energía en eV:", E_found / 1.60218D-19

CONTAINS

  ! Método de Newton-Raphson para encontrar los valores propios de energía
  FUNCTION NewtonRaphson(E0) RESULT(E)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: E0  ! Estimación inicial de la energía (J)
    DOUBLE PRECISION :: E, dE, tol, fE, fE_prime
    INTEGER :: iter, max_iter
    
    E = E0
    tol = 1.0D-6  ! Tolerancia para la convergencia
    max_iter = 50  ! Número máximo de iteraciones
    dE = 1.0D-4  ! Pequeño incremento para derivada numérica
    
    DO iter = 1, max_iter
      fE = ShootingError(E)  ! Evaluación de la función de error
      fE_prime = (ShootingError(E + dE) - fE) / dE  ! Aproximación de la derivada
      
      IF (ABS(fE) < tol) EXIT  ! Si el error es menor que la tolerancia, detener
      E = E - fE / fE_prime  ! Actualización de Newton-Raphson
    END DO
  END FUNCTION NewtonRaphson

  ! Función para calcular el error del método del disparo
  FUNCTION ShootingError(E) RESULT(error)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: E  ! Energía tentativa (J)
    DOUBLE PRECISION :: error, x, y(2), h, L
    INTEGER :: i
    
    ! Definir ancho del pozo
    L = 1.0D-9  ! 1 nm
    
    ! Condiciones iniciales para la integración
    x = -L/2.0D0  ! Comenzamos en -L/2
    y(1) = 0.0D0  ! Condición de frontera ψ(-L/2) = 0
    y(2) = 1.0D0  ! Derivada inicial arbitraria
    h = L / 1000.0D0  ! Paso de integración pequeño
    
    ! Integración de la ecuación de Schrödinger
    DO i = 1, 1000
      CALL RK4(x, y, h)
    END DO
    
    ! Cálculo del error en la frontera derecha (ψ debe ser 0 en +L/2)
    error = y(1)
  END FUNCTION ShootingError

  ! Método de Runge-Kutta de 4to orden para resolver EDOs
  SUBROUTINE RK4(x, y, h)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: x, y(2)  ! Variable independiente y vector de estado
    DOUBLE PRECISION, INTENT(IN) :: h  ! Tamaño de paso para la integración
    DOUBLE PRECISION :: k1(2), k2(2), k3(2), k4(2), y_temp(2)
    INTEGER :: i
    
    ! Cálculo de coeficientes de Runge-Kutta
    CALL Schrodinger(x, y, k1)
    DO i = 1, 2
      y_temp(i) = y(i) + 0.5*h*k1(i)
    END DO
    CALL Schrodinger(x + 0.5*h, y_temp, k2)
    DO i = 1, 2
      y_temp(i) = y(i) + 0.5*h*k2(i)
    END DO
    CALL Schrodinger(x + 0.5*h, y_temp, k3)
    DO i = 1, 2
      y_temp(i) = y(i) + h*k3(i)
    END DO
    CALL Schrodinger(x + h, y_temp, k4)
    
    ! Actualización de valores de y
    DO i = 1, 2
      y(i) = y(i) + (h/6.0)*(k1(i) + 2.0*k2(i) + 2.0*k3(i) + k4(i))
    END DO
    x = x + h
  END SUBROUTINE RK4

  ! Ecuación de Schrödinger para el pozo infinito
  SUBROUTINE Schrodinger(z, y, dydz)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: z  ! Posición en metros (m)
    DOUBLE PRECISION, INTENT(IN) :: y(2)  ! Vector de estado (y(1) = ψ, y(2) = dψ/dz)
    DOUBLE PRECISION, INTENT(OUT) :: dydz(2)  ! Derivadas calculadas
    
    ! Transformación de Schrödinger en un sistema de ecuaciones diferenciales
    dydz(1) = y(2)  ! dψ/dz = ψ'
    dydz(2) = -y(1)  ! d²ψ/dz² = -ψ dentro del pozo infinito
  END SUBROUTINE Schrodinger

END PROGRAM QuantumShooting

