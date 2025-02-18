datos=load('carga_runge_kutta.dat');
datos1=load('descarga_runge_kutta.dat');

% Extraer columnas
abscisa1=datos1(:,1);
ordenada11=datos1(:,2);
ordenada21=datos1(:,3);


abscisa=datos(:,1);
ordenada1=datos(:,2);
ordenada2=datos(:,3);


% Graficar
plot(abscisa, ordenada1, 'bs');
hold on;
plot(abscisa,ordenada2,'r')
hold on;
plot(abscisa1,ordenada11,'gs')
hold on;
plot(abscisa1,ordenada21,'m')
xlabel('Eje X');
ylabel('Eje Y');
title('Voltaje y Tiempo ');
grid on;

legend('solucion aproximada de carga','solucion exacta de carga','solucion aproximada de descarga','solucion exacta de descarga');

% Opcional: Guardar la gráfica
print -dpng grafico_runge_kutta.png;