function birdModelDEs5 = birdModelDEs5(t,y)

% This line just tests whether GitHub works

global n;

U = y(1:n);
L = y((n+1):(2*n));
R = y((2*n+1):(3*n));


% Set the values of the parameters of the model

alpha = zeros(n,1);  % Transmission rate from solvers to susceptibles
for i = 1:n
   alpha(i) = 0.0055; 
end

alpha2 = zeros(n,1);  % Rate at which solvers switch strategy
for i = 1:n
   alpha2(i) = 0.0055; 
end

beta = zeros(n,1);  % Asocial learning rate
for i = 1:n
   beta(i) = 0.000; 
end


lambda = zeros(n,1); % Conformity parameter
for i = 1:n
   lambda(i) = 1.1; 
end




%lambda = zeros(n,1);  % Transmission rate from one solution to the other
%for i = 1:n
%   lambda(i) = 0.005; 
%end

epsilonL = zeros(n,1);  % Conformity function for L
for i = 1:n
   if L(i) > 0 | R(i) > 0
    epsilonL(i) = 1 / (1+exp(-log( (L(i)/(L(i)+R(i))) / (1-(L(i)/(L(i)+R(i)))) )*lambda(i)));
   else
    epsilonL(i) = 0;
   end
end

epsilonR = zeros(n,1);  % Conformity function for R
for i = 1:n
   if R(i) > 0 | L(i) > 0
    epsilonR(i) = 1 / (1+exp(-log( (R(i)/(L(i)+R(i))) / (1-(R(i)/(L(i)+R(i)))) )*lambda(i)));
   else
    epsilonR(i) = 0;
   end
end



gamma = importdata('move_per_capita.csv'); % An n x n matrix.  The (i,j) entry represents the daily rate with which birds move from patch i to patch j (based from the probability that a bird seen in patch i will also be seen in patch j in the same day).  These values depend on the connectivity between patches

%gamma1 = zeros(n,n);
%for i = 1:n
%    for j = 1:n
%        gamma1(i,j) = (U(j)+L(j)+R(j))/(U(i)+U(j)+L(i)+L(j)+R(i)+R(j));
%    end
%end

% gamma = gamma2 .* gamma1; 


% Here we write out the differential equations

for i = 1:n
    birdModels2(i) = -alpha(i)*(L(i)+R(i))*U(i) - beta(i)*U(i) - sum(gamma(i,:))*U(i) + sum(gamma(:,i).*U);  % The differential equations for U.  The term sum(gamma(i,:))*U(i) adds up all the rates at which susceptible birds leave patch i to go to another patch, and sum(gamma(:,i).*U) is the rate at which susceptible birds in other patches leave to go to patch i
end
for i = 1:n
    birdModels2(n + i) = alpha(i)*(L(i)+R(i))*U(i)*epsilonL(i) + (beta(i)/2)*U(i) -alpha2(i)*(L(i)+R(i))*L(i)*epsilonR(i) +alpha2(i)*(L(i)+R(i))*R(i)*epsilonL(i) - sum(gamma(i,:))*L(i) + sum(gamma(:,i).*L);  % The differential equations for L
end
for i = 1:n
    birdModels2(2*n + i) = alpha(i)*(L(i)+R(i))*U(i)*epsilonR(i) + (beta(i)/2)*U(i) +alpha2(i)*(L(i)+R(i))*L(i)*epsilonR(i) -alpha2(i)*(L(i)+R(i))*R(i)*epsilonL(i) - sum(gamma(i,:))*R(i) + sum(gamma(:,i).*R);  % The differential equations for R
end


%+ lambda(i)*epsilon2(i)*L(i)*R(i) - lambda(i)*epsilon1(i)*L(i)*R(i) 

birdModelDEs5 = transpose(birdModels2);

end