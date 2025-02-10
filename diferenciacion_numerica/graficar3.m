datos=load('datos.txt');

x = datos(:, 1);
y1 = datos(:, 2);
y2= datos(:,3);
y3= datos(:,4);
y4= datos(:,5);
y5=datos(:,6);
y6=datos(:,7);
y7=datos(:,8);
y8=datos(:,9);
y9=datos(:,10);
y10=datos(:,11);
y11=datos(:,12);

% Crear una figura con 2 filas y 6 columnas
subplot(2, 6, 1);
plot(x, y1);
title('y = f(x)');

subplot(2, 6, 2);
plot(x, y2);
title('fp3p');

subplot(2, 6, 3);
plot(x, y3);
title('fp2pup');

subplot(2, 6, 4);
plot(x, y4);
title('fp2pdn');

subplot(2, 6, 5);
plot(x, y5);
title('fp5p');

subplot(2, 6, 6);
plot(x, y6);
title('fp4pup');

subplot(2, 6, 7);
plot(x, y7);
title('fp4pdn');

subplot(2, 6, 8);
plot(x, y8);
title('f2p4p');

subplot(2, 6, 9);
plot(x, y9);
title('f3p4p');

subplot(2, 6, 10);
plot(x, y10);
title('f2p5p');

subplot(2, 6, 11);
plot(x, y11);
title('f3p5p');

% Ajustar el espaciado entre subgráficas
subplot_adjust('left', 0.5, 'right', 0.9, 'bottom', 0.1, 'top', 0.9, 'wspace', 0.4, 'hspace', 0.4);