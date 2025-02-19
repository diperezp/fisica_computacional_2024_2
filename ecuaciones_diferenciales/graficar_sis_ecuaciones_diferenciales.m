% Leer los datos del fichero
data = load('sis_ecuaciones_diferenciales.dat');

x=data(:,1);
y1=data(:,2);
y2=data(:,3);
y3=data(:,4);

plot(x,y1);
hold on;
plot(x,y3);
