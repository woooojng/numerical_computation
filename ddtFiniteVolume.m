function [dudt] = ddtFiniteVolume (t, u)
%DDTFINITEVOLUME Summary of this function goes here
% Detailed explanation goes here
	N = length (u) ;
	x = linspace(0, 1, N+1);
	dx = x(2:end) - x(1:end -1);
	f = u.^2 /2;
	uL = u(1:end-1);
	uR = u(2:end);
	maxF = max(f(1:end-1), f(2:end));
	minF = min(f(1:end-1), f(2:end));
	minF(logical((uR >0) .* (0 >uL))) = 0;
	f_int = maxF .* (uL > uR) + minF .* (uL <= uR) ;
	f_int = [0; f_int; 0];
	dudt = (f_int(1:end-1) - f_int(2:end)) ./ dx';
end

% command
% N =100
% x = linspace(0, 1, N+1);
% dx = x(2:end) - x(1:end-1);
% u = ones(N, 1);
% u(1:N/2) = -1;
% u0 = u;
% dt = 0.01;
% for t = 0:dt:2
% [t, u] = ode45(@ddtFiniteVolume, [0, dt], u);
% u = u(end, :);
% plot([x(1:end-1) ; x(2:end)], [u; u], 'k');
% drawnow;
% pause(dt)
% end