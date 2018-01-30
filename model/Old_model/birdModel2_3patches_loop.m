clc
clear all
close all

global n;
n = 3;  % This is the number of patches

to = 0;  % This is the initial time
tf = 150;  % This is the final time

times = [to:1:tf];  % This is a vector of timepoints that we want to record results at

u0 = zeros(n,1);  % This creates a vector with every entry equal to zero.  I will fill this vector in as the initial values of U, i.e. the number of susceptible birds in each patch
l0 = zeros(n,1);  % I will fill this vector in as the initial values of L.  I will fill this vector in as the initial values of L, i.e. the number of birds with LEFT preference in each patch
r0 = zeros(n,1);  % I will fill this vector in as the initial values of R.  I will fill this vector in as the initial values of R, i.e. the number of birds with RIGHT preference in each patch


% Set the initial state of the system
initialState = [100,100,50];

for i = 1:n
    u0(i) = initialState(i);
end

l0(1) = 2;
r0(2) = 2;

yo = [u0; l0; r0]

% Set parameter values
lambdavals = 0.0005:0.0005:0.01;
betavals = 1:0.1:5;
for l = 1:size(lambdavals(:))
    for b = 1:size(betavals(:))
    
        lambda = lambdavals(l);
        alph = 0.01;
        bet = betavals(b);
        dd = 1.5;
        dist = [[0,dd,1]; [dd,0,dd]; [1,dd,0]];
        gamma = zeros(n,n); % Movement matrix
        for i = 1:n
            gamma(i,:) = lambda*transpose(u0+l0+r0)./(1+dist(i,:));
        end
        gamma(logical(eye(size(gamma)))) = 0;
        save parameters lambda alph bet gamma;

        %  Run the differential equation model

        [t y] = ode23s('birdModelDEs_2patches',times, transpose(yo));
        
        y = real(y);
        resVar((41*(l-1)) + b) = var([y(150,4)/(y(150,4)+y(150,7)), y(150,5)/(y(150,5)+y(150,8))]);
        resNbL((41*(l-1)) + b) = (y(150,4)+y(150,5)) / (y(150,4)+y(150,5)+y(150,7)+y(150,8));
        lb((41*(l-1)) + b) = lambda;
        al((41*(l-1)) + b) = alph;
        be((41*(l-1)) + b) = bet;
    end
end

csvwrite('resultsModel_3patches_sumstats_alpha0.01_dist1.5_IC100100.csv', {al(:),lb(:),be(:),resVar(:),resNbL(:)})
