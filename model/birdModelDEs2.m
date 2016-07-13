function birdModelDEs2 = birdModelDEs2(t,y)

global n;

U = y(1:n);
L = y((n+1):(2*n));
R = y((2*n+1):(3*n));


% Set the values of the parameters of the model

alpha = zeros(n,1);  % The rate at which L's pass information on to U's in each patch (per LU pair).
for i = 1:n
   alpha(i) = 0.01; 
end

beta = zeros(n,1);  % The rate at which R's pass information on to U's in each patch (per RU pair).
for i = 1:n
   beta(i) = 0.01; 
end

epsilon1 = zeros(n,1);  % The reduction in rate at which information is passed to birds that are already trained compared to susceptible birds
for i = 1:n
   if L(i)+R(i) > 0
    epsilon1(i) = R(i)/(L(i)+R(i));
   else
    epsilon1(i) = 0;
   end
end

epsilon2 = zeros(n,1);  % The reduction in rate at which information is passed to birds that are already trained compared to susceptible birds
for i = 1:n
   if L(i)+R(i) > 0
    epsilon2(i) = L(i)/(L(i)+R(i));
   else
    epsilon1(i) = 0;
   end
end


gamma1 = zeros(n,n);
for i = 1:n
    for j = 1:n
        gamma1(i,j) = (U(j)+L(j)+R(j))/(U(i)+U(j)+L(i)+L(j)+R(i)+R(j));
    end
end

gamma2 = zeros(n,n);
gamma2(1,1) = 0;       gamma2(1,2) = 0.1;    gamma2(1,3) = 0.01;    gamma2(1,4) = 0;        gamma2(1,5) = 0;
gamma2(2,1) = 0.1;    gamma2(2,2) = 0;       gamma2(2,3) = 0.1;     gamma2(2,4) = 0.01;    gamma2(2,5) = 0;
gamma2(3,1) = 0.01;   gamma2(3,2) = 0.1;    gamma2(3,3) = 0;        gamma2(3,4) = 0.1;     gamma2(3,5) = 0.01;
gamma2(4,1) = 0;       gamma2(4,2) = 0.01;   gamma2(4,3) = 0.1;     gamma2(4,4) = 0;        gamma2(4,5) = 0.1;
gamma2(5,1) = 0;       gamma2(5,2) = 0;       gamma2(5,3) = 0.01;    gamma2(5,4) = 0.1;     gamma2(5,5) = 0;
gamma2(6,1) = 0.1;    gamma2(6,2) = 0.1;    gamma2(6,3) = 0.01;    gamma2(6,4) = 0;        gamma2(6,5) = 0;
gamma2(7,1) = 0.01;   gamma2(7,2) = 0.1;    gamma2(7,3) = 0.1;     gamma2(7,4) = 0.01;    gamma2(7,5) = 0;
gamma2(8,1) = 0;       gamma2(8,2) = 0.01;   gamma2(8,3) = 0.1;     gamma2(8,4) = 0.1;     gamma2(8,5) = 0.01;
gamma2(9,1) = 0;       gamma2(9,2) = 0;       gamma2(9,3) = 0.01;    gamma2(9,4) = 0.1;     gamma2(9,5) = 0.1;
gamma2(10,1) = 0;      gamma2(10,2) = 0;      gamma2(10,3) = 0;       gamma2(10,4) = 0.01;   gamma2(10,5) = 0.1;
gamma2(11,1) = 0.01;  gamma2(11,2) = 0.01;  gamma2(11,3) = 0;       gamma2(11,4) = 0;       gamma2(11,5) = 0;
gamma2(12,1) = 0;      gamma2(12,2) = 0.01;  gamma2(12,3) = 0.01;   gamma2(12,4) = 0;       gamma2(12,5) = 0;
gamma2(13,1) = 0;      gamma2(13,2) = 0.01;  gamma2(13,3) = 0.01;   gamma2(13,4) = 0.01;   gamma2(13,5) = 0;
gamma2(14,1) = 0;      gamma2(14,2) = 0;      gamma2(14,3) = 0.01;   gamma2(14,4) = 0.01;   gamma2(14,5) = 0.01;
gamma2(15,1) = 0;      gamma2(15,2) = 0;      gamma2(15,3) = 0;       gamma2(15,4) = 0.01;   gamma2(15,5) = 0.01;
gamma2(16,1) = 0;      gamma2(16,2) = 0;      gamma2(16,3) = 0;       gamma2(16,4) = 0;       gamma2(16,5) = 0.01;

gamma2(1,6) = 0.1;    gamma2(1,7) = 0.01;   gamma2(1,8) = 0;        gamma2(1,9) = 0;        gamma2(1,10) = 0;
gamma2(2,6) = 0.1;    gamma2(2,7) = 0.1;    gamma2(2,8) = 0.01;    gamma2(2,9) = 0;        gamma2(2,10) = 0;
gamma2(3,6) = 0.01;   gamma2(3,7) = 0.1;    gamma2(3,8) = 0.1;     gamma2(3,9) = 0.01;    gamma2(3,10) = 0;
gamma2(4,6) = 0;       gamma2(4,7) = 0.01;   gamma2(4,8) = 0.1;     gamma2(4,9) = 0.1;     gamma2(4,10) = 0.01;
gamma2(5,6) = 0;       gamma2(5,7) = 0;       gamma2(5,8) = 0.01;    gamma2(5,9) = 0.1;     gamma2(5,10) = 0.1;
gamma2(6,6) = 0;       gamma2(6,7) = 0.1;    gamma2(6,8) = 0.01;    gamma2(6,9) = 0;        gamma2(6,10) = 0;
gamma2(7,6) = 0.1;    gamma2(7,7) = 0;       gamma2(7,8) = 0.1;     gamma2(7,9) = 0;        gamma2(7,10) = 0;
gamma2(8,6) = 0.01;   gamma2(8,7) = 0.1;    gamma2(8,8) = 0;        gamma2(8,9) = 0.1;     gamma2(8,10) = 0.01;
gamma2(9,6) = 0;       gamma2(9,7) = 0;       gamma2(9,8) = 0.1;     gamma2(9,9) = 0;        gamma2(9,10) = 0.1;
gamma2(10,6) = 0;      gamma2(10,7) = 0;      gamma2(10,8) = 0.01;   gamma2(10,9) = 0.1;    gamma2(10,10) = 0;
gamma2(11,6) = 0.1;   gamma2(11,7) = 0.01;  gamma2(11,8) = 0;       gamma2(11,9) = 0;       gamma2(11,10) = 0;
gamma2(12,6) = 0.1;   gamma2(12,7) = 0.1;   gamma2(12,8) = 0.01;   gamma2(12,9) = 0;       gamma2(12,10) = 0;
gamma2(13,6) = 0.01;  gamma2(13,7) = 0.1;   gamma2(13,8) = 0.1;    gamma2(13,9) = 0.01;   gamma2(13,10) = 0;
gamma2(14,6) = 0;      gamma2(14,7) = 0.01;  gamma2(14,8) = 0.1;    gamma2(14,9) = 0.1;    gamma2(14,10) = 0.01;
gamma2(15,6) = 0;      gamma2(15,7) = 0;      gamma2(15,8) = 0.01;   gamma2(15,9) = 0.1;    gamma2(15,10) = 0.1;
gamma2(16,6) = 0;      gamma2(16,7) = 0;      gamma2(16,8) = 0;       gamma2(16,9) = 0.01;   gamma2(16,10) = 0.1;

gamma2(1,11) = 0.01;  gamma2(1,12) = 0;      gamma2(1,13) = 0;       gamma2(1,14) = 0;       gamma2(1,15) = 0;      gamma2(1,16) = 0;
gamma2(2,11) = 0.01;  gamma2(2,12) = 0.01;  gamma2(2,13) = 0;       gamma2(2,14) = 0;       gamma2(2,15) = 0;      gamma2(2,16) = 0;
gamma2(3,11) = 0;      gamma2(3,12) = 0.01;  gamma2(3,13) = 0.01;   gamma2(3,14) = 0.01;   gamma2(3,15) = 0;      gamma2(3,16) = 0;
gamma2(4,11) = 0;      gamma2(4,12) = 0;      gamma2(4,13) = 0.01;   gamma2(4,14) = 0.01;   gamma2(4,15) = 0.01;  gamma2(4,16) = 0;
gamma2(5,11) = 0;      gamma2(5,12) = 0;      gamma2(5,13) = 0;       gamma2(5,14) = 0.01;   gamma2(5,15) = 0.01;  gamma2(5,16) = 0.01;
gamma2(6,11) = 0.1;   gamma2(6,12) = 0.1;   gamma2(6,13) = 0.01;   gamma2(6,14) = 0;       gamma2(6,15) = 0;      gamma2(6,16) = 0;
gamma2(7,11) = 0.01;  gamma2(7,12) = 0.1;   gamma2(7,13) = 0.1;    gamma2(7,14) = 0.01;   gamma2(7,15) = 0;      gamma2(7,16) = 0;
gamma2(8,11) = 0;      gamma2(8,12) = 0.01;  gamma2(8,13) = 0.1;    gamma2(8,14) = 0.1;    gamma2(8,15) = 0.01;  gamma2(8,16) = 0;
gamma2(9,11) = 0;      gamma2(9,12) = 0;      gamma2(9,13) = 0.01;   gamma2(9,14) = 0.1;    gamma2(9,15) = 0.1;   gamma2(9,16) = 0.01;
gamma2(10,11) = 0;     gamma2(10,12) = 0;     gamma2(10,13) = 0;      gamma2(10,14) = 0.01;  gamma2(10,15) = 0.1;  gamma2(10,16) = 0.1;
gamma2(11,11) = 0;     gamma2(11,12) = 0.1;  gamma2(11,13) = 0.01;  gamma2(11,14) = 0;      gamma2(11,15) = 0;     gamma2(11,16) = 0;
gamma2(12,11) = 0.1;  gamma2(12,12) = 0;     gamma2(12,13) = 0.1;   gamma2(12,14) = 0.01;  gamma2(12,15) = 0;     gamma2(12,16) = 0;
gamma2(13,11) = 0.01; gamma2(13,12) = 0.1;  gamma2(13,13) = 0;      gamma2(13,14) = 0.1;   gamma2(13,15) = 0.01; gamma2(13,16) = 0;
gamma2(14,11) = 0;     gamma2(14,12) = 0.01; gamma2(14,13) = 0.1;   gamma2(14,14) = 0;      gamma2(14,15) = 0.1;  gamma2(14,16) = 0.01;
gamma2(15,11) = 0;     gamma2(15,12) = 0;     gamma2(15,13) = 0.01;  gamma2(15,14) = 0.1;   gamma2(15,15) = 0;     gamma2(15,16) = 0.1;
gamma2(16,11) = 0;     gamma2(16,12) = 0;     gamma2(16,13) = 0;      gamma2(16,14) = 0.01;  gamma2(16,15) = 0.1;  gamma2(16,16) = 0;

gamma = gamma2 .* gamma1; % An n x n matrix.  The (i,j) entry represents the rate with which birds migrate from patch i to patch j.  These values will depend on the connectivity between patches


% Here we write out the differential equations

for i = 1:n
    birdModels2(i) = -alpha(i)*L(i)*U(i) - beta(i)*R(i)*U(i) - sum(gamma(i,:))*U(i) + sum(gamma(:,i).*U);  % The differential equations for U.  The term sum(gamma(i,:))*U(i) adds up all the rates at which susceptible birds leave patch i to go to another patch, and sum(gamma(:,i).*U) is the rate at which susceptible birds in other patches leave to go to patch i
end
for i = 1:n
    birdModels2(n + i) = alpha(i)*L(i)*U(i) - epsilon1(i)*beta(i)*R(i)*L(i) + epsilon2(i)*alpha(i)*L(i)*R(i) - sum(gamma(i,:))*L(i) + sum(gamma(:,i).*L);  % The differential equations for L
end
for i = 1:n
    birdModels2(2*n + i) = beta(i)*R(i)*U(i) - epsilon2(i)*alpha(i)*L(i)*R(i) + epsilon1(i)*beta(i)*R(i)*L(i) - sum(gamma(i,:))*R(i) + sum(gamma(:,i).*R);  % The differential equations for R
end



birdModelDEs2 = transpose(birdModels2);

end



