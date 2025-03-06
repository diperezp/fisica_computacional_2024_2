datos3=load('shotingmethodnolinealD1.dat');

% Extraer columnas
abscisa1=datos3(:,1);
ordenada31=datos3(:,2);

plot(abscisa1,ordenada31,'b')
hold on;
xlabel('Eje X');
ylabel('Eje Y');
title('Voltaje y Tiempo ');
grid on;

legend('PVFL');

% Opcional: Guardar la gráfica
print -dpng grafica.png;