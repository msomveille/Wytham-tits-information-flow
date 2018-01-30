function birdModelDEs = birdModelDEs(t,y)

global n;

U = y(1:n);
L = y((n+1):(2*n));
R = y((2*n+1):(3*n));


% Set the values of the parameters of the model

alpha = zeros(n,1);  % The rate at which L's pass information on to U's in each patch (per LU pair).
for i = 1:n
   alpha(i) = 0.005; 
end

beta = zeros(n,1);  % The rate at which R's pass information on to U's in each patch (per RU pair).
for i = 1:n
   beta(i) = 0.005; 
end

epsilon = zeros(n,1);  % The reduction in rate at which information is passed to birds that are already trained compared to susceptible birds
for i = 1:n
   epsilon(i) = 0.2; 
end

gamma = zeros(n,n);  % An n x n matrix.  The (i,j) entry represents the rate with which birds migrate from patch i to patch j.  These values will depend on the connectivity between patches
gamma(1,1) = 0;     gamma(1,2) = 0.1;   gamma(1,3) = 0;
gamma(2,1) = 0.1;   gamma(2,2) = 0;     gamma(2,3) = 0.1;
gamma(3,1) = 0;  gamma(3,2) = 0.1;   gamma(3,3) = 0;




% Here we write out the differential equations

for i = 1:n
    birdModels(i) = -alpha(i)*L(i)*U(i) - beta(i)*R(i)*U(i) - sum(gamma(i,:))*U(i) + sum(gamma(:,i).*U);  % The differential equations for U.  The term sum(gamma(i,:))*U(i) adds up all the rates at which susceptible birds leave patch i to go to another patch, and sum(gamma(:,i).*U) is the rate at which susceptible birds in other patches leave to go to patch i
end
for i = 1:n
    birdModels(n + i) = alpha(i)*L(i)*U(i) - epsilon(i)*beta(i)*R(i)*L(i) + epsilon(i)*alpha(i)*L(i)*R(i) - sum(gamma(i,:))*L(i) + sum(gamma(:,i).*L);  % The differential equations for L
end
for i = 1:n
    birdModels(2*n + i) = beta(i)*R(i)*U(i) - epsilon(i)*alpha(i)*L(i)*R(i) + epsilon(i)*beta(i)*R(i)*L(i) - sum(gamma(i,:))*R(i) + sum(gamma(:,i).*R);  % The differential equations for R
end



birdModelDEs = transpose(birdModels);

end





