datos = load('resultados.txt');
puntos = load('datos.txt');

x0 = puntos(1,1);
y0 = puntos(1,2);

x1 = puntos(2,1);
y1 = puntos(2,2);

x2 = puntos(3,1);
y2 = puntos(3,2);

x3 = puntos(4,1);
y3 = puntos(4,2);


x = datos(:,1);
y = datos(:,2);

% Graficar los datos
plot(x, y, 'b');
xlabel('Eje X');
ylabel('Eje Y');
title('Gráfico de datos desde archivo');
grid on;
hold on;

% Graficar los puntos adicionales
scatter([x0, x1, x2,x3], [y0, y1, y2, y3], 'filled', 'r');

% Opcional: Guardar la gráfica
print -dpng grafico_cubica.png;