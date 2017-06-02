clc
clear all
close all

global n;
n = 65;  % This is the number of patches

to = 0;  % This is the initial time
tf = 20;  % This is the final time

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


%  Run the differential equation model

[t y] = ode23s('birdModelDEs3',times, transpose(yo));


% Plot the results
%for i = 1:25
%figure()
%h1 = plot(t, y(:, i), 'g')
%hold on
%h2 = plot(t, y(:, n + i), 'r')
%hold on
%h3 = plot(t, y(:, 2*n + i), 'b')
%hold on

%legend([h1 h2 h3], 'Number susceptible', 'Number left preference', 'Number right preference')

%title(strcat('Patch number ', num2str(i)))
%end

csvwrite('resultsModel4.csv', y)

