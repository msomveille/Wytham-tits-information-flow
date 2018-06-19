clc
clear all
close all

global n;
n = 2;  % This is the number of patches

to = 0;  % This is the initial time
tf = 150;  % This is the final time

times = [to:1:tf];  % This is a vector of timepoints that we want to record results at

u0 = zeros(n,1);  % This creates a vector with every entry equal to zero.  I will fill this vector in as the initial values of U, i.e. the number of susceptible birds in each patch
l0 = zeros(n,1);  % I will fill this vector in as the initial values of L.  I will fill this vector in as the initial values of L, i.e. the number of birds with LEFT preference in each patch
r0 = zeros(n,1);  % I will fill this vector in as the initial values of R.  I will fill this vector in as the initial values of R, i.e. the number of birds with RIGHT preference in each patch


% Set the initial state of the system
initialState = [49,51];

for i = 1:n
    u0(i) = initialState(i);
end

l0(1) = 2;
r0(2) = 2;

yo = [u0; l0; r0]

% Set parameter values

mov = 0.003;
alph = 0.01;
lambda = 3.5;
dist = 1;
gamma = [[0,(mov*dist*(yo(2)+yo(4)+yo(6)))];[(mov*dist*(yo(1)+yo(3)+yo(5))),0]];
save parameters mov alph lambda gamma;


%  Run the differential equation model

[t y] = ode23s('birdModelDEs_2patches',times, transpose(yo));
        
y = real(y);
var([y(150,3)/(y(150,3)+y(150,5)), y(150,4)/(y(150,4)+y(150,6))])
(y(150,3)+y(150,4)) / (y(150,3)+y(150,4)+y(150,5)+y(150,6))

csvwrite('resultsModel_2patches3.csv', y)