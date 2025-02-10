datos = load('datos.txt');

% Separar en vectores
x = datos(:, 1);
y1 = datos(:, 2);
y2= datos(:,3);



% Graficar la primera función
plot(x, y1, 'r', 'LineWidth', 2);  % Línea roja
hold on;  % Mantener la gráfica actual

% Graficar la segunda función
plot(x, y2, 'b', 'LineWidth', 2);  % Línea azul punteada

% Personalizar la gráfica
title('');
xlabel('x');
ylabel('y1');
legend('sin(x)', 'cos(x)');  % Añadir leyenda
grid on;  % Activar la cuadrícula

% Opcional: Guardar la gráfica
print -dpng grafico.png;