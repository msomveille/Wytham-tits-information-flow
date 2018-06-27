% Code to run the model of information spread across Wytham Woods multiple
% times randomizing the initial conditions

clc
clear all
close all

global n;
n = 65;  % This is the number of patches

to = 0;     % This is the initial time
tf = 150;   % This is the final time

times = [to:1:tf];  % This is a vector of timepoints that we want to record results at

% Set the initial state of the system (data from Farine & Sheldon 2016
% bioRxiv)
initialState = importdata('Number_of_individuals_per_patch.csv');

dist = importdata('forestDistances.csv');


% Run the model (100 times)

for k = 1:100
    u_0 = zeros(n,1);  % This creates a vector with every entry equal to zero.  We will then fill this vector in as the initial values of U, i.e. the number of naive individuals in each patch
    s1_0 = zeros(n,1);  % We will then fill this vector in as the initial values of S1, i.e. the number of solvers using solution s1 in each patch
    s2_0 = zeros(n,1);  % We will then fill this vector in as the initial values of S2, i.e. the number of solvers using solution s2 in each patch

    for i = 1:n
        u_0(i) = initialState(i);
    end
    
    % Release innovators (i.e. trained solvers) at random locations across
    % Wytham Woods
    IC = datasample(1:60, 2,'Replace',false);
    s1_0(IC(1)) = 2;
    s2_0(IC(2)) = 2;
    
    yo = [u_0; s1_0; s2_0]
    
    
    % Set parameter values

    lambda = 0.02;
    alph = 0.005;
    bet = 4;
    movementMatrix = zeros(n,n); % Movement matrix
    for i = 1:n
        movementMatrix(i,:) = lambda*transpose(u_0+s1_0+s2_0)./(1+dist(i,:));
    end
    movementMatrix(:,61:65) = 0; % Remove the patches that are outside of the forest
    movementMatrix(logical(eye(size(movementMatrix)))) = 0;

    save parameters mov alph lambda movementMatrix; % Save the parameter values and movement matrix so that it can be used for the differential equations

    
    %  Run the differential equation model

    [t y] = ode23s('infoSpread_DEs',times, transpose(yo));
        
    y = real(y);
    resVar(k) = var(y(150,66:125)./(y(150,66:125) + y(150,131:190)));
    resNbL(k) = sum(y(150,66:125))/(sum(y(150,66:125)) + sum(y(150,131:190)));
    poolL(k) = yo(IC(1));
    poolR(k) = yo(IC(2));
    centralityL(k) = median(dist(IC(1),:));
    centralityR(k) = median(dist(IC(2),:));
    
end

csvwrite('resultsModel_wytham_random_conformity4.csv', {resVar(:),resNbL(:),poolL(:),poolR(:),centralityL(:),centralityR(:)})