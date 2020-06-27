function [dudt] = ddtFiniteVolume3 (t, u)
%DDTFINITEVOLUME Godunov's Method - FVM for nonlinear system
%Detailed explanation goes here
	N = length(u) ;
	x = linspace(0,1,N+1) ;
	dx = x(2:end) - x(1:end-1) ;
	f = u. ^2 /2 ; %change thiss if you solve a different equation
	dfdu = u;
	% f_int = (f(1:end-1) + f(2:end)) / 2; %you only need to change this if you use a different equation
	speed = (f(1:end-1) - f(2:end)) ./ (u(1:end-1) u(2:end));
	special_case = abs(u(1:end-1) - u(2:end)) < 1E-8;
	speed(special_case) = dfdu(special_case);
	fL = f(1:end-1);
	fR = f(2:end);
	f_int = fL .* (speed > 0) + fR .* (speed < 0);
	f_int = [0; f_int; 0];
	dudt = (f_int(1:end-1) - f_int(2:end)) ./ dx';
	size(dudt)
end

%u = u0 + 0.1;
%for t = 0:dt:2
%[t, u] = ode45(@ddtFiniteVolume3, [0, dt], u);
%u = u(end, :);
%plot([x(1:end-1); x(2:end)], [u; u], 'k');
%ylim([-1, 1.1]);
