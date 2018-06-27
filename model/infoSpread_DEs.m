% Differential equations for the model of information spread

function infoSpread_DEs = infoSpread_DEs(t,y)

global n;

U = y(1:n);             % Vector describing the number of naive individuals in each patch
S1 = y((n+1):(2*n));    % Vector describing the number of solvers using behaviour s1 in each patch
S2 = y((2*n+1):(3*n));  % Vector describing the number of solvers using behaviour s2 in each patch


% Get the values of the parameters of the model

load parameters;


% Conformist learning function L for both solution s1 (LS1) and solution s2 (LS2)

LS1 = zeros(n,1);
for i = 1:n
   if S1(i) > 0 | S2(i) > 0
    LS1(i) = 1 / (1+exp(-log( (S1(i)/(S1(i)+S2(i))) / (1-(S1(i)/(S1(i)+S2(i)))) )*lambda));
   else
    LS1(i) = 0;
   end
end

LS2 = zeros(n,1);
for i = 1:n
   if S2(i) > 0 | S1(i) > 0
    LS2(i) =  1 / (1+exp(-log( (S2(i)/(S1(i)+S2(i))) / (1-(S2(i)/(S1(i)+S2(i)))) )*lambda));
   else
    LS2(i) = 0;
   end
end


% Here we write out the differential equations

for i = 1:n
    DEsModel(i) = -alph*(S1(i)+S2(i))*U(i) - sum(movementMatrix(i,:))*U(i) + sum(movementMatrix(:,i).*U);  % The differential equations for naives. The term sum(movementMatrix(i,:))*U(i) adds up all the rates at which susceptible invididuals leave patch i to go to another patch, and sum(movementMatrix(:,i).*U) is the rate at which susceptible individuals in other patches leave to go to patch i
end
for i = 1:n
    DEsModel(n + i) = alph*(S1(i)+S2(i))*U(i)*LS1(i) -alph*(S1(i)+S2(i))*S1(i)*LS2(i) +alph*(S1(i)+S2(i))*S2(i)*LS1(i) - sum(movementMatrix(i,:))*S1(i) + sum(movementMatrix(:,i).*S1);  % The differential equations for solvers using solution s1
end
for i = 1:n
    DEsModel(2*n + i) = alph*(S1(i)+S2(i))*U(i)*LS2(i) +alph*(S1(i)+S2(i))*S1(i)*LS2(i) -alph*(S1(i)+S2(i))*S2(i)*LS1(i) - sum(movementMatrix(i,:))*S2(i) + sum(movementMatrix(:,i).*S2);  % The differential equations for for solvers using solution s2
end

infoSpread_DEs = transpose(DEsModel);

end