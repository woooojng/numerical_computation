
clear all; close all;

sflag = input('Use stiff integrator? (1=yes, [default=no]): ');

k   = 1.0; % this is really k/(rho*cp)

L = 1.0; % non-dimensional

Tinit = 400;

Tleft = 800;
Tright = 1000;

Nx = input(['Enter number of divisions in x-direction: [default=' ...
            '51]']);
if (isempty(Nx)),
  Nx = 51;
end

h  = L/Nx;
x  = linspace(0,L,Nx+1);

Tmax = 0.5;

A = spalloc(Nx-1,Nx-1,3*(Nx-1));
I = speye(Nx-1);

% Calculate stiffness matrix

for ii = 1:Nx-1,

  if (ii > 1),
    A(ii,ii-1) = k/h^2;
  end

  if (ii < Nx-1),
    A(ii,ii+1) = k/h^2;
  end

  A(ii,ii) = -2*k/h^2;

end

b = zeros(Nx-1,1);
b(1)    = k*Tleft/h^2;
b(Nx-1) = k*Tright/h^2;

v0 = Tinit*ones(1,Nx-1);

if (sflag == 1),

  options = odeset('Jacobian',A);
  [t,v] = ode15s(@dif1d_fun,[0 Tmax],v0,options,A,b);

else

  [t,v] = ode45(@dif1d_fun,[0 Tmax],v0,[],A,b);

end

Tmid = v(:,floor(Nx/2));
plot(t,Tmid);
xlabel('t');
ylabel('T at midpoint');
 



function [f] = dif1d_fun(t, v, A, b)

f = A*v + b;