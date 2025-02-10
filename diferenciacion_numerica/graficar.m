% Cargar los datos
datos = load('datos.txt');

% Separar en vectores
x = datos(:, 1);
y = datos(:, 2);

% Graficar
plot(x, y, 'b');
xlabel('Eje X');
ylabel('Eje Y');
title('Gráfico de datos desde archivo');
grid on;

% Opcional: Guardar la gráfica
print -dpng grafico.png;