% Code to run the model of information spread across Wytham Woods

clc
clear all
close all

global n;
n = 65;  % This is the number of patches

to = 0;     % This is the initial time
tf = 150;   % This is the final time

times = [to:1:tf];  % This is a vector of timepoints that we want to record results at

u_0 = zeros(n,1);  % This creates a vector with every entry equal to zero.  We will then fill this vector in as the initial values of U, i.e. the number of naive individuals in each patch
s1_0 = zeros(n,1);  % We will then fill this vector in as the initial values of S1, i.e. the number of solvers using solution s1 in each patch
s2_0 = zeros(n,1);  % We will then fill this vector in as the initial values of S2, i.e. the number of solvers using solution s2 in each patch


% Set the initial state of the system (data from Farine & Sheldon 2016
% bioRxiv)
initialState = importdata('Number_of_individuals_per_patch.csv');

for i = 1:n
    u_0(i) = initialState(i);
end

% Release innovators (i.e. trained solvers) at the same location than the real-world cultural diffusion experiment described in Aplin et al (2015) Nature
s1_0(11) = 2;
s1_0(50) = 2;
s1_0(59) = 2;
s2_0(3) = 2;
s2_0(7) = 2;

yo = [u_0; s1_0; s2_0]



% Set parameter values

mov = 0.03;     % Magnitude of the movement rate
alph = 0.005;   % Learning rate
lambda = 1;     % Conformity strength
dist = importdata('forestDistances.csv');
movementMatrix = zeros(n,n); % Movement matrix
for i = 1:n
    movementMatrix(i,:) = lambda*transpose(u_0+s1_0+s2_0)./(1+dist(i,:));
end
movementMatrix(:,61:65) = 0; % Remove the patches that are outside of the forest
movementMatrix(logical(eye(size(movementMatrix)))) = 0;

save parameters mov alph lambda movementMatrix; % Save the parameter values and movement matrix so that it can be used for the differential equations


% Run the differential equation model

[t y] = ode23s('infoSpread_DEs',times, transpose(yo));
        
y = real(y);

Pvar = var([y(150,3)/(y(150,3)+y(150,5)), y(150,4)/(y(150,4)+y(150,6))])
Ptot = (y(150,3)+y(150,4)) / (y(150,3)+y(150,4)+y(150,5)+y(150,6))


% Write the model outputs to a csv file

csvwrite('resultsModel_wytham.csv', y)
