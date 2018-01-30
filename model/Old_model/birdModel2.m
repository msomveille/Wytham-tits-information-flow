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

l0(11) = 2;
l0(50) = 2;
l0(59) = 2;
r0(3) = 2;
r0(7) = 2;

yo = [u0; l0; r0]


% Set parameter values

lambda = 0.002;
alph = 0.01;
bet = 1.3;
save parameters lambda alph bet;


%  Run the differential equation model

[t y] = ode23s('birdModelDEs5',times, transpose(yo));

y = real(y);

csvwrite('resultsModel_wytham.csv', y)

