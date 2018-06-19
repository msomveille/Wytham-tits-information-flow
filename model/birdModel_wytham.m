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

%l0(IC(1)) = 2;
%l0(IC(2)) = 2;
%l0(IC(3)) = 2;
%r0(IC(4)) = 2;
%r0(IC(5)) = 2;

l0(11) = 2;
l0(50) = 2;
l0(59) = 2;
r0(3) = 2;
r0(7) = 2;

yo = [u0; l0; r0]


% Set parameter values

lambda = 0.03; % Movement rate -- low=0.005 , good=0.02 , high=0.1
alph = 0.005; % Learning rate  -- low=0.003 , good=0.0055 , high=0.01
bet = 1; % Conformity rate  -- low=1 , high=5
%dist = importdata('feedersDistances.csv');
dist = importdata('forestDistances.csv');
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

var(y(150,66:125)./(y(150,66:125) + y(150,131:190)))
sum(y(150,66:125))/(sum(y(150,66:125)) + sum(y(150,131:190)))

yo(IC(1)) + yo(IC(2)) + yo(IC(3))
yo(IC(4)) + yo(IC(5))

csvwrite('resultsModel_wytham1.csv', y)

