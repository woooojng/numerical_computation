
clear all;

% grid
xL = -4;
xR =  4;
Nx = 40; % number of control volumes
x = linspace(xL,xR,Nx+1);

% midpoint values of x in a control volume
xmid = 0.5*(x(1:Nx) + x(2:Nx+1));

% cell size in control volumes (assumed equal)
dx = x(2) - x(1);

% velocity
u = 1;

% final time
tfinal = 100;

% timestep
CFL = 0.5;
dt = CFL*dx/abs(u);

% initial condition U0 = exp(-x^2)

U = exp(-xmid.^2);
t = 0;

% Loop until t > tfinal
while (t < tfinal),

  Ubc = [U(Nx), U, U(1)]; % This enforces the periodic bc

  % the flux at each interface
  F =   0.5*    u *( Ubc(2:Nx+2) + Ubc(1:Nx+1)) ...
      - 0.5*abs(u)*( Ubc(2:Nx+2) - Ubc(1:Nx+1));

  % residual in each cell
  R = F(2:Nx+1) - F(1:Nx);

  % Forward Euler step
  U = U - (dt/dx)*R;

  % Increment time
  t = t + dt;

  % Plot current solution
  stairs(x,[U, U(Nx)]);
  axis([xL, xR, -0.5, 1.5]);
  grid on;
  drawnow;

end

U = exp(-xmid.^2);
hold on;
stairs(x,[U, U(Nx)], 'r-');