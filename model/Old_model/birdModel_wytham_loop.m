clc
clear all
close all

global n;
n = 65;  % This is the number of patches

to = 0;  % This is the initial time
tf = 150;  % This is the final time

times = [to:1:tf];  % This is a vector of timepoints that we want to record results at

u0 = zeros(n,1);  % This creates a vector with every entry equal to zero.  I will fill this vector in as the initial values of U, i.e. the number of susceptible birds in each patch
l0 = zeros(n,1);  % I will fill this vector in as the initial values of L.  I will fill this vector in as the initial values of L, i.e. the number of birds with LEFT preference in each patch
r0 = zeros(n,1);  % I will fill this vector in as the initial values of R.  I will fill this vector in as the initial values of R, i.e. the number of birds with RIGHT preference in each patch


% Set the initial state of the system
initialState = importdata('Number_of_individuals_per_patch.csv');

for i = 1:n
    u0(i) = initialState(i);
end

IC = randi([1 60], 1, 5)

l0(IC(1)) = 2;
l0(IC(2)) = 2;
l0(IC(3)) = 2;
r0(IC(4)) = 2;
r0(IC(5)) = 2;

%l0(11) = 2;
%l0(50) = 2;
%l0(59) = 2;
%r0(3) = 2;
%r0(7) = 2;

yo = [u0; l0; r0]

% Set parameter values
lambdavals = 0:0.005:0.1;
betavals = 1:0.2:5;
%dist = importdata('feedersDistances.csv');
dist = importdata('forestDistances.csv');
for l = 1:size(lambdavals(:))
    for b = 1:size(betavals(:))
    
        lambda = lambdavals(l);
        alph = 0.005;
        bet = betavals(b);
        gamma = zeros(n,n); % Movement matrix
        for i = 1:n
            gamma(i,:) = lambda*transpose(u0+l0+r0)./(1+dist(i,:));
        end
        gamma(:,61:65) = 0;
        gamma(logical(eye(size(gamma)))) = 0;
        save parameters lambda alph bet gamma;

        %  Run the differential equation model

        [t y] = ode23s('birdModelDEs_2patches',times, transpose(yo));
        
        y = real(y);
        resVar((21*(l-1)) + b) = var(y(150,66:125)./(y(150,66:125) + y(150,131:190)));
        resNbL((21*(l-1)) + b) = sum(y(150,66:125))/(sum(y(150,66:125)) + sum(y(150,131:190)));
        lb((21*(l-1)) + b) = lambda;
        al((21*(l-1)) + b) = alph;
        be((21*(l-1)) + b) = bet;
    end
end

csvwrite('resultsModel_wytham_sumstats_alpha0.005_forest.csv', {al(:),lb(:),be(:),resVar(:),resNbL(:)})