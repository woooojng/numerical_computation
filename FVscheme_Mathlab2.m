N = 10;
x = linspace(0, 1, N+1) ;
dx = x(2:end) - x(1:end-1) ;
u_int = -1/(2*pi)*cos(2*pi*x) ;
u = (u_int(2:end) - u_int(1:end-1)) ./ dx;
plot([x(1:end-1); x(2:end)], [u; u])
u0 = u



dt = 0.01;
for t = 0:dt:2
[t, u] = ode45(@ddtFiniteVolume, [0, dt], u) ;
u = u(end, :);
plot([x(1:end-1); x(2:end)], [u; u])
ylim([-1, 1])
drawnow;
pause(dt)
end

%After compiling, comapare the graph with the last part of the Lec7-6