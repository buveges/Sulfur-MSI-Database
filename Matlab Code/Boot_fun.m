function [age,d34s,d33s,D33S,D33Scalc,sig34s,sig33s,sig33S,sig33SC,Ox] =...
    Boot_fun(df,t,O,Kwpy)
%Boot_fun Bootstrap sampling function for MIF database associated with
%Uveges et al., 2022 (DOI: )
%   This function takes a large database (df) that has been
%   sampled-resampled to remove any sampling bias of the Sulfur MIF record
%   (ie. every 50 Myr time bin now has the same numbe of samples in it)
%   and applies various age and oxygen based weighting to the data base.
%   More specifically, it applies a stronger likelihood of a given sample
%   being picked if the samples age is close to that of the input time. As
%   well, there is a built in oxygen dependence for sulfide phases, with
%   low oxygen content resulting in a sulfide being less likely to be
%   'picked' and included in the weatthering average. The function then
%   randomly picks m samples from the weighted database and calculates the
%   mean of those samples (in d34,d33 and D33 S), and repeats that process
%   n times. Finally, the function out puts the mean of the n bootsrapped
%   means. If uncommented, can also output stdev of bootstrapped means.

% Converting input age to age format used in data base (Ga to Ma)
tx = (t*1000); 
% Converting Oxygen levels into a scalar weight. K is reaction constant of
% pyrite weathering
    Ox = (O^.5)/((Kwpy^0.5)+(O^0.5)); % Half power version
    %Ox = (O)/(Kwpy+O); % Single power version, what was used originally.

% Other constants
n = 10; %Number of bootstrap iterations
m = 10; %Number of samples to randomly pull each bootstrap iteration

  
% Selecting only samples older than input age, and assigning age weighting
    % Age weighting is derived from Peters and Husson, 2017.
 samples = df((df.group_age>tx),:);
 samples.weight = exp(1).^(-0.001*(samples.group_age-(tx-1)));
    % Testing raw database
 %samples.weight = samples.group_age./samples.group_age;
 
% Assigning Ox weights based on oxygen dependence
    % Sulfides are scaled by Ox parameter, sulfates are not.
 for i = 1:length(samples.weight)
 if any(strcmp(samples.Phase(i), ["Sulfide","Total S",...
         "Sulfide + Organic S", "Sulfide + Organic S, Sulfide",...
         "Sulfide, Sulfide + Organic S"]))
     samples.Oxweight(i) = samples.weight(i).*Ox;
 else
    samples.Oxweight(i) = samples.weight(i);
 end
 end

 % Setting up table of zeros with column names
bootmeans = array2table(zeros(n,3));
bootmeans.Properties.VariableNames = {'d34s','d33s','D33S'};
 
% Random sampling (m samples pulled) of database with replacement. 
    % Repeat n times
for i = 1:length(bootmeans.d34s)
 index = randsample(1:length(samples.d33s),m,true,samples.Oxweight);
 samples2 = samples(index,:);
 
 % Calculating means of each random sampling run
 bootmeans.d34s(i) = nanmean(samples2.d34s);
 bootmeans.d33s(i) = nanmean(samples2.d33s);
 bootmeans.D33S(i) = nanmean(samples2.D33S);
end
  
% Adding a D33S column that is calculated from d34s and d33s
bootmeans.D33Scalc = (bootmeans.d33s - 1000*...
    (((1+(bootmeans.d34s/1000)).^0.515)-1));  

% Calculating means and stdevs of all the bootstrapped means
d34s = mean(bootmeans.d34s);
sig34s = std(bootmeans.d34s);
d33s = mean(bootmeans.d33s);
sig33s = std(bootmeans.d33s);
D33S = mean(bootmeans.D33S);
sig33S = std(bootmeans.D33S);
D33Scalc = mean(bootmeans.D33Scalc);
sig33SC = std(bootmeans.D33Scalc);
age= t;
end
 



