datos=load('tabla.txt');


abscisa=datos(:,1);
ordenada1=datos(:,2);
ordenada2=datos(:,3);


% Graficar
plot(abscisa, ordenada1, 'b');
hold on;
plot(abscisa,ordenada2,'r')
xlabel('Eje X');
ylabel('Eje Y');
title('Tiempo y angulo ');
grid on;

% Opcional: Guardar la gráfica
print -dpng grafico.png;