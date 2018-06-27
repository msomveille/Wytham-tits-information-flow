% Code to run the baseline model of information spread

clc
clear all
close all

global n;
n = 2;  % This is the number of patches

to = 0;    % This is the initial time
tf = 150;  % This is the final time

times = [to:1:tf];  % This is a vector of timepoints that we want to record results at

u_0 = zeros(n,1);  % This creates a vector with every entry equal to zero.  We will then fill this vector in as the initial values of U, i.e. the number of naive individuals in each patch
s1_0 = zeros(n,1);  % We will then fill this vector in as the initial values of S1, i.e. the number of solvers using solution s1 in each patch
s2_0 = zeros(n,1);  % We will then fill this vector in as the initial values of S2, i.e. the number of solvers using solution s2 in each patch


% Set the initial state of the system (e.g. 49 naive individuals in patch
% and 51 naives in patch 2)
initialState = [49,51];

for i = 1:n
    u_0(i) = initialState(i);
end

s1_0(1) = 2;  % Two individuals trained to solve the puzzle using solution s1 are released in patch 1
s2_0(2) = 2;  % Two individuals trained to solve the puzzle using solution s2 are released in patch 2

yo = [u_0; s1_0; s2_0]


% Set parameter values

mov = 0.003;    % Magnitude of the movement rate
alph = 0.01;    % Learning rate
lambda = 3.5;   % Conformity strength
dist = 1;
movementMatrix = [[0,(mov*dist*(yo(2)+yo(4)+yo(6)))];[(mov*dist*(yo(1)+yo(3)+yo(5))),0]];

save parameters mov alph lambda movementMatrix; % Save the parameter values and movement matrix so that it can be used for the differential equations


% Run the differential equation model

[t y] = ode23s('infoSpread_DEs',times, transpose(yo));
        
y = real(y);

Pvar = var([y(150,3)/(y(150,3)+y(150,5)), y(150,4)/(y(150,4)+y(150,6))])
Ptot = (y(150,3)+y(150,4)) / (y(150,3)+y(150,4)+y(150,5)+y(150,6))


% Write the model outputs to a csv file

csvwrite('resultsModel_2patches.csv', y)
