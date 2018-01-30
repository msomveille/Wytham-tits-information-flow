clc
clear all
close all

global n;
n = 65;  % This is the number of patches

to = 0;  % This is the initial time
tf = 150;  % This is the final time

times = [to:1:tf];  % This is a vector of timepoints that we want to record results at


% Set the initial state of the system
initialState = importdata('Number_of_individuals_per_patch.csv');


% Set parameter values
%dist = importdata('feedersDistances.csv');
dist = importdata('forestDistances.csv');
for k = 1:100
    u0 = zeros(n,1);  % This creates a vector with every entry equal to zero.  I will fill this vector in as the initial values of U, i.e. the number of susceptible birds in each patch
    l0 = zeros(n,1);  % I will fill this vector in as the initial values of L.  I will fill this vector in as the initial values of L, i.e. the number of birds with LEFT preference in each patch
    r0 = zeros(n,1);  % I will fill this vector in as the initial values of R.  I will fill this vector in as the initial values of R, i.e. the number of birds with RIGHT preference in each patch

    for i = 1:n
        u0(i) = initialState(i);
    end

    %l0(11) = 2;
    %l0(50) = 2;
    %l0(59) = 2;
    %r0(3) = 2;
    %r0(7) = 2;

    IC = datasample(1:60, 2,'Replace',false);
    l0(IC(1)) = 2;
    %l0(IC(2)) = 2;
    %l0(IC(3)) = 2;
    r0(IC(2)) = 2;
    %r0(IC(5)) = 2;
    
    yo = [u0; l0; r0]

    lambda = 0.02;
    alph = 0.005;
    bet = 4;
    gamma = zeros(n,n); % Movement matrix
    for i = 1:n
        gamma(i,:) = lambda*transpose(u0+l0+r0)./(1+dist(i,:));
    end
    gamma(:,61:65) = 0;
    gamma(logical(eye(size(gamma)))) = 0;
    save parameters lambda alph bet gamma;

    %  Run the differential equation model

    [t y] = ode23s('birdModelDEs_wytham',times, transpose(yo));
        
    y = real(y);
    resVar(k) = var(y(150,66:125)./(y(150,66:125) + y(150,131:190)));
    resNbL(k) = sum(y(150,66:125))/(sum(y(150,66:125)) + sum(y(150,131:190)));
    poolL(k) = yo(IC(1));
    poolR(k) = yo(IC(2));
    centralityL(k) = median(dist(IC(1),:));
    centralityR(k) = median(dist(IC(2),:));
    
end

csvwrite('resultsModel_wytham_sumstats_random_conformity4.csv', {resVar(:),resNbL(:),poolL(:),poolR(:),centralityL(:),centralityR(:)})