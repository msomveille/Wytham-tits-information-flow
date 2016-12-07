function birdModelDEs4 = birdModelDEs4(t,y)

% This line just tests whether GitHub works

global n;

U = y(1:n);
L = y((n+1):(2*n));
R = y((2*n+1):(3*n));


% Set the values of the parameters of the model

alpha = zeros(n,1);  % Transmission rate from solvers to susceptibles
for i = 1:n
   alpha(i) = 0.006; 
end

beta = zeros(n,1);  % Asocial learning rate
for i = 1:n
   beta(i) = 0.00001; 
end

gamma = importdata('move_per_capita.csv'); % An n x n matrix.  The (i,j) entry represents the daily rate with which birds move from patch i to patch j (based from the probability that a bird seen in patch i will also be seen in patch j in the same day).  These values depend on the connectivity between patches


% Here we write out the differential equations

for i = 1:n
    birdModels2(i) = -alpha(i)*L(i)*U(i) - alpha(i)*R(i)*U(i) - beta(i)*U(i) - sum(gamma(i,:))*U(i) + sum(gamma(:,i).*U);  % The differential equations for U.  The term sum(gamma(i,:))*U(i) adds up all the rates at which susceptible birds leave patch i to go to another patch, and sum(gamma(:,i).*U) is the rate at which susceptible birds in other patches leave to go to patch i
end
for i = 1:n
    birdModels2(n + i) = alpha(i)*L(i)*U(i) - alpha(i)*L(i)*R(i) + alpha(i)*L(i)*R(i) + (beta(i)/2)*U(i) - sum(gamma(i,:))*L(i) + sum(gamma(:,i).*L);  % The differential equations for L
end
for i = 1:n
    birdModels2(2*n + i) = alpha(i)*R(i)*U(i) + alpha(i)*L(i)*R(i) - alpha(i)*L(i)*R(i) + (beta(i)/2)*U(i) - sum(gamma(i,:))*R(i) + sum(gamma(:,i).*R);  % The differential equations for R
end



birdModelDEs4 = transpose(birdModels2);

end