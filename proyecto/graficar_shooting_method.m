datos1=load('shotingmethod.dat');

% Extraer columnas
abscisa1=datos1(:,1);
ordenada11=datos1(:,2);
ordenada21=datos1(:,3);
ordenada31=ordenada11+ 



plot(abscisa1,ordenada11,'g')
hold on;
plot(abscisa1,ordenada21,'m')
xlabel('Eje X');
ylabel('Eje Y');
title('Voltaje y Tiempo ');
grid on;

legend('solucion aproximada de carga','solucion exacta de carga');

% Opcional: Guardar la gráfica
print -dpng grafica.png;