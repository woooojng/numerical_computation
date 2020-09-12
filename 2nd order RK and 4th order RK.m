x0 = -3;
x1 =  3;
Nx = 301;

y0 = -3;
y1 =  3;
Ny = 301;

xv    = linspace(x0,x1,Nx);
yv    = linspace(y0,y1,Ny);
[x,y] = meshgrid(xv,yv);
z = x + i*y;
g = 1 + z + 0.5*z.^2;

gmag = abs(g);
contour(x,y,gmag,[1 1],'k-');
axis([x0,x1,y0,y1]);
axis('square');
xlabel('Real \lambda\Delta t');
ylabel('Imag \lambda\Delta t');
grid on;