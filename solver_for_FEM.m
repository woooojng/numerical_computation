% FEM solver for d2T/dx2 + f = 0 where f = 50 exp(x)
%
% BCs: T(-1) = 100 and T(1) = 100.
%
% Note: In the vector T, the finite element degrees of freedom are
%       stored.

nElem = 10;
x = linspace(-1,1,nElem+1);

K = zeros(nElem+1, nElem+1);
F = zeros(nElem+1, 1);

for elem = 1:nElem,

  n1 = elem;
  n2 = elem+1;

  x1 = x(n1);
  x2 = x(n2);

  dx = x2 - x1;

  K(n1, n1) = K(n1, n1) - (1/dx);

  K(n1, n2) = K(n1, n2) + (1/dx);

  K(n2, n1) = K(n2, n1) + (1/dx);

  K(n2, n2) = K(n2, n2) - (1/dx);

  F(n1) = F(n1) - (50*(exp(x2)-x2*exp(x1) + x1*exp(x1) - exp(x1))/dx);

  F(n2) = F(n2) - (50*(x2*exp(x2)-exp(x2)-x1*exp(x2)+exp(x1))/dx);

end


n1 = 1;
K(n1,:)    = zeros(size(1,nElem+1));
K(n1, n1) = 1.0;
F(n1)      = 100.0;


n1 = nElem+1;
K(n1,:)    = zeros(size(1,nElem+1));
K(n1, n1) = 1.0;
F(n1)      = 100.0;


T = K\F;


figure(1);
plot(x,T,'*-');
xlabel('x');
ylabel('T');


Npt = 20*nElem+1;
xe = linspace(-1,1,Npt);
Te = -50*exp(xe) + 50*xe*sinh(1) + 100 + 50*cosh(1);
hold on; plot(xe,Te); hold off;



Terr(1) = T(1) - Te(1);
h = x(2)-x(1);
for i = 2:Npt-1,
  xxi = xe(i);
  Tei = Te(i);
  j = floor((xxi-xe(1))/h) + 1;
  x0 = x(j);
  x1 = x(j+1);
  T0 = T(j);
  T1 = T(j+1);
  xi = 2*(xxi - x0)/(x1-x0)-1;  % This gives xi between +/-1
  Ti = 0.5*(1-xi)*T0 + 0.5*(1+xi)*T1;
  Terr(i) = Ti - Tei;
end
Terr(Npt) = T(nElem+1) - Te(Npt);

figure(2);
plot(xe,Terr);
xlabel('x');
ylabel('Error');