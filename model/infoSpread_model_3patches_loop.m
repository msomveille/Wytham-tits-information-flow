% Code to run the 3-patch version of the model of information spread
% multiple times to sample the paramter space

clc
clear all
close all

global n;
n = 3;  % This is the number of patches

to = 0;    % This is the initial time
tf = 150;  % This is the final time

times = [to:1:tf];  % This is a vector of timepoints that we want to record results at

u_0 = zeros(n,1);  % This creates a vector with every entry equal to zero.  We will then fill this vector in as the initial values of U, i.e. the number of naive individuals in each patch
s1_0 = zeros(n,1);  % We will then fill this vector in as the initial values of S1, i.e. the number of solvers using solution s1 in each patch
s2_0 = zeros(n,1);  % We will then fill this vector in as the initial values of S2, i.e. the number of solvers using solution s2 in each patch


% Set the initial state of the system (e.g. 45 naive individuals in patch 1, 55 naives in patch 2, and 50 naives in patch 3)
initialState = [45,55,50];

for i = 1:n
    u_0(i) = initialState(i);
end

s1_0(1) = 2;  % Two individuals trained to solve the puzzle using solution s1 are released in patch 1
s2_0(2) = 2;  % Two individuals trained to solve the puzzle using solution s2 are released in patch 2

yo = [u_0; s1_0; s2_0]


% Set range of parameter values

movementvals = 0.0005:0.0005:0.01;
lambdavals = 1:0.1:5;


% Run the model

for l = 1:size(lambdavals(:))
    for m = 1:size(movementvals(:))
    
        mov = movementvals(m);    % Magnitude of the movement rate
        alph = 0.01;              % Learning rate
        lambda = lambdavals(l);   % Conformity strength
        dd=1.5;
        dist = [[0,dd,1]; [dd,0,dd]; [1,dd,0]];
        movementMatrix = zeros(n,n); % Movement matrix
        for i = 1:n
            movementMatrix(i,:) = mov*transpose(u_0+s1_0+s2_0)./(1+dist(i,:));
        end
        movementMatrix(logical(eye(size(movementMatrix)))) = 0;

        save parameters mov alph lambda movementMatrix; % Save the parameter values and movement matrix so that it can be used for the differential equations

        
        %  Run the differential equations

        [t y] = ode23s('infoSpread_DEs',times, transpose(yo));
        
        y = real(y);
        resVar((41*(l-1)) + b) = var([y(150,3)/(y(150,3)+y(150,5)), y(150,4)/(y(150,4)+y(150,6))]);
        resNbL((41*(l-1)) + b) = (y(150,3)+y(150,4)) / (y(150,3)+y(150,4)+y(150,5)+y(150,6));
        lb((41*(l-1)) + b) = lambda;
        al((41*(l-1)) + b) = alph;
        be((41*(l-1)) + b) = bet;
    end
end

csvwrite('resultsModel_3patches_alpha0.01_IC45.csv', {al(:),lb(:),be(:),resVar(:),resNbL(:)})
