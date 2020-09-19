clear all;

periodic_flag = 1;

% Set-up grid
xL = -4;
xR =  4;
Nx = 21; % number of points
x = linspace(xL,xR,Nx);

% cell size in control volumes (assumed equal)
dx = x(2) - x(1);

% velocity
u = 1;

% timestep
CFL = 1;
dt = CFL*dx/abs(u);

% bc state at left (assumes u>0)
UL = exp(-xL^2);

% stiffness matrix (A).
%
A = zeros(Nx-1,Nx-1);

% A except for first and last row
for i = 2:Nx-2,
  A(i,i-1) =  u/(2*dx);
  A(i,i+1) = -u/(2*dx);
end

if (periodic_flag == 1), % Periodic bcs

  A(1   ,2   ) = -u/(2*dx);
  A(1   ,Nx-1) =  u/(2*dx);
  A(Nx-1,1   ) = -u/(2*dx);
  A(Nx-1,Nx-2) =  u/(2*dx);

else % non-periodic bc's
