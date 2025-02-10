datos = load('datos.txt');

% Separar en vectores
x = datos(:, 1);
y1 = datos(:, 2);
y2= datos(:,3);
y3= datos(:,4);
y4= datos(:,5);
y5=datos(:,6);
y6=datos(:,7);
y7=datos(:,8);



% Graficar la primera función
plot(x, y1, 'r', 'LineWidth', 2);  % Línea roja
hold on;  % Mantener la gráfica actual

% Graficar la segunda función
plot(x, y2, 'b', 'LineWidth', 2);  % Línea azul punteada
hold on;

% Graficar la segunda función
plot(x, y3, 'g', 'LineWidth', 2);  % Línea azul punteada
hold on;

% Graficar la segunda función
plot(x, y4, 'y', 'LineWidth', 2);  % Línea azul punteada
hold on;

plot(x, y5, '-r', 'LineWidth', 2);  % Línea azul punteada
hold on;

plot(x, y6, '-c', 'LineWidth', 2);  % Línea azul punteada
hold on;

plot(x, y7, '-r', 'LineWidth', 2);  % Línea azul punteada
hold on;



% Personalizar la gráfica
title('');
xlabel('x');
ylabel('y1');
legend('f', 'fp3p','fp2pup','fp2pdn','fp5p','fp4pup','fp4pdn');  % Añadir leyenda
grid on;  % Activar la cuadrícula

% Opcional: Guardar la gráfica
print -dpng grafico.png;