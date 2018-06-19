function birdModelDEs_wytham = birdModelDEs_wytham(t,y)

% This line just tests whether GitHub works

global n;

U = y(1:n);
L = y((n+1):(2*n));
R = y((2*n+1):(3*n));


% Set the values of the parameters of the model

load parameters;


epsilonL = zeros(n,1);  % Conformity function for L
for i = 1:n
   if L(i) > 0 | R(i) > 0
    epsilonL(i) = 1 / (1+exp(-log( (L(i)/(L(i)+R(i))) / (1-(L(i)/(L(i)+R(i)))) )*bet));
   else
    epsilonL(i) = 0;
   end
end

epsilonR = zeros(n,1);  % Conformity function for R
for i = 1:n
   if R(i) > 0 | L(i) > 0
    epsilonR(i) = 1 / (1+exp(-log( (R(i)/(L(i)+R(i))) / (1-(R(i)/(L(i)+R(i)))) )*bet));
   else
    epsilonR(i) = 0;
   end
end



%gamma = importdata('move_per_capita.csv'); % An n x n matrix.  The (i,j) entry represents the daily rate with which birds move from patch i to patch j (based from the probability that a bird seen in patch i will also be seen in patch j in the same day).  These values depend on the connectivity between patches
%gamma = [[0,(lambda*dist*52)];[(lambda*dist*52),0]];

%gamma1 = zeros(n,n);
%for i = 1:n
%    for j = 1:n
%        gamma1(i,j) = (U(j)+L(j)+R(j))/(U(i)+U(j)+L(i)+L(j)+R(i)+R(j));
%    end
%end

% gamma = gamma2 .* gamma1; 


% Here we write out the differential equations

for i = 1:n
    birdModels2(i) = -alph*(L(i)+R(i))*U(i) - sum(gamma(i,:))*U(i) + sum(gamma(:,i).*U);  % The differential equations for U.  The term sum(gamma(i,:))*U(i) adds up all the rates at which susceptible birds leave patch i to go to another patch, and sum(gamma(:,i).*U) is the rate at which susceptible birds in other patches leave to go to patch i
end
for i = 1:n
    birdModels2(n + i) = alph*(L(i)+R(i))*U(i)*epsilonL(i) -alph*(L(i)+R(i))*L(i)*epsilonR(i) +alph*(L(i)+R(i))*R(i)*epsilonL(i) - sum(gamma(i,:))*L(i) + sum(gamma(:,i).*L);  % The differential equations for L
end
for i = 1:n
    birdModels2(2*n + i) = alph*(L(i)+R(i))*U(i)*epsilonR(i) +alph*(L(i)+R(i))*L(i)*epsilonR(i) -alph*(L(i)+R(i))*R(i)*epsilonL(i) - sum(gamma(i,:))*R(i) + sum(gamma(:,i).*R);  % The differential equations for R
end


%+ lambda(i)*epsilon2(i)*L(i)*R(i) - lambda(i)*epsilon1(i)*L(i)*R(i) 

birdModelDEs_wytham = transpose(birdModels2);

end