K = [-8/3, 0; 0, -8/5];
>> b = [-200*exp(-1); -100*exp(1) + 700*exp(-1)];
>> a = K\b

a =

  2×2 logical 배열

   1   1
   1   1

>> Ta_mwr = 100 + a(1)*phi1 + a(2) *phi2
>> E_mwr = Ta_mwr - T
>> R_mwr = -2*a(1) - 6*a(2)*x + 50 *exp(x) ; 
>> figure(4);
plot (x, Ta_mwr, '--'); hold on;
plot(x, T); hold off;
grid on;
xlabel('x');ylabel('Temperature');
legend('MWR', 'Exact');

figure(5);
plot(x, E_mwr);
grid on;
xlabel('x');
ylabel('Errorin Temperature');