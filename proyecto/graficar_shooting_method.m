datos1=load('shotingmethodD1.dat');
datos2=load('shotingmethodD2.dat');
datos3=load('solucion.dat');

% Extraer columnas
abscisa1=datos1(:,1);
ordenada11=datos1(:,2);
ordenada21=datos2(:,2) ;
ordenada31=datos3(:,2);



plot(abscisa1,ordenada11,'g')
hold on;
plot(abscisa1,ordenada21,'m')
hold on;
plot(abscisa1,ordenada31,'b')
xlabel('Eje X');
ylabel('Eje Y');
title('Voltaje y Tiempo ');
grid on;

legend('PVI1','PV2','PVFL');

% Opcional: Guardar la gráfica
print -dpng grafica.png;