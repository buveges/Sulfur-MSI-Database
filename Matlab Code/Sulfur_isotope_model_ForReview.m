%% Isotope model for review
% Isotope model companion to OSMV5 model utilized in Uveges et al., 2022.
% OSMV5 must be run first, as this model utilizes the output of that model.
% NOTE: this model takes a long time to run due to the bootstrapping
% function (Boot_fun.m) that is run on each time step.

%% Set up and constants
% Importing output from OMSV5 model
B= SOflux(9:end,:);
[m,n]=size(B);
check = reshape(mean(reshape(B,38,[])),[],n);
SOflux2= array2table(check);
% Importing output from BTUs OMSV5 version of SOno model
%  and converting to table with specifc variable names
SOflux2.Properties.VariableNames = {'t', 'O', 'S', 'Fvs', 'Fbpy',...
    'Fwpy', 'Fbgyp', 'Fwgyp','FS8py','FvS8','FvSulf','dt','ds'};
    % Combining Gyp and Pyr weathering fluxes into "weathered S" column
    SOflux2.Fws = SOflux2.Fwpy + SOflux2.Fwgyp; 
    % Combining Gyp and Pyr burial fluxes into "buried S" column
    SOflux2.Fbs = SOflux2.Fbpy + SOflux2.Fbgyp; 
    SOflux2.dsdt = SOflux2.ds./SOflux2.dt;
    SOflux2.NetFs = SOflux2.Fws+SOflux2.Fvs-SOflux2.Fbs;
    %SOflux2 = SOflux2(108:end,:);
    
    % Setting up dummy array for boot_fun output
    out = array2table(zeros(length(SOflux2.S),9));
    out.Properties.VariableNames={'age','d34s','d33s','D33S',...
        'D33Scalc','sig34s','sig33s','sig33S','sig33SC'};
    
   %df =  readtable('MIF_boot_sampl.csv'); % Uncomment this line to run
   %only time adusted database
   df =  readtable('Copy_of_MIF_boot_sample_cratons.csv');
   %df =  readtable('SSA_database.csv'); % Uncomment this line to run
   %unadjusted SSA database

    % constraining database to >1.5ga
   df= df((df.group_age >= 1500),{'group_age','d34s','d33s',...
        'D33S','Phase'}); 
    % Setting database values to single precision to increase speed.
    df.group_age = single(df.group_age); 
    df.d34s = single(df.d34s);
    df.d33s = single(df.d33s);
    df.D33S = single(df.D33S);
   

% Setting constants/initial conditions
DSulf = 0; % Initial SwSulfate ?33
dSulf = DSulf;
DvSulf = -5; % D33 of Incoming Volcanic S as SO4 after photolysis 
                %~min value in database for boot out3,
                %-8 for bout_outCraton
DvS8 = 15; % D33 of Incoming Volcanic S as S8 after photolysis 
                %~max value in databasefor boot out3,
                %24 for bout_outCraton

    %%% fS8 and kS8 have been incoporated into OMSV5 model

        % Setting up dummy table for output of model. D33S refers to ?33 of
            % SwSulf
    pool = array2table(zeros(length(SOflux2.S),11));
    pool.Properties.VariableNames = {'age','D33S','Dbpy','Fbpy','Fbgyp','Fbs','FvS8','FvSulf','RPy','S','SiR'};

%% Actual model

for l = 1:length(SOflux2.S)
       
        
        %S = SOflux2.S(l); % Seawater sulfate  
        O = SOflux2.O(l);   % Oxygen
        t= SOflux2.t(l); % time in Ga
        dt = SOflux2.dt(l); % timestep in years
        Fws = SOflux2.Fws(l)*dt;% Flux of weathered sulfur into system 
                                    % (/year) multiplied by dt
        Fbs = SOflux2.Fbs(l)*dt;% Flux of buried sulfur out of system 
                                    % (/year) multiplied by dt
        Fvs = SOflux2.Fvs(l)*dt;% Flux of volcanic sulfur into system 
                                    % (/year) multiplied by dt
        Fbpy = SOflux2.Fbpy(l)*dt;% Flux of sulfur out of system as pyrite
                                    % (/year) multiplied by dt
        Fbgyp = SOflux2.Fbgyp(l)*dt;% Flux of sulfur out of system as 
                                        % gypsum (/year) multiplied by dt
        %Dbs = DSulf;
            
        FvS8 = SOflux2.FvS8(l)*dt; % Flux of S8 into system as fraction of the flux 
                            % of Volcanic Sulfur
         % portion of S8 direct to pyrite                    
        FvSulf = SOflux2.FvSulf(l)*dt;% Flux of SO4 into system as fraction of the 
                                %flux of Volcanic Sulfur
        FS8py = SOflux2.FS8py(l)*dt;   % Flux of S8 directly to pyrite
        Si = SOflux2.S(l); % Seawater sulfate reservoir initial
        S = Si + FvSulf + FvS8 + Fws - Fbpy - Fbgyp;
 
        Dbpy1 = DSulf; % D33S of buried pyrite from SWSR
        Dbpy = (DSulf*(Fbpy) + DvS8*FS8py)/(Fbpy+FS8py);% D33S of 
                            % total buried pyrite. Fxn of SwSulf pool
                            % and amount of S8 directly added (controlled
                            % by kS8
        Dbgyp = DSulf; % D33S of buried gypsum. Dependent on SwSulf
       
        %%%   Bootstrap function for MIF weathering   
         [out.age(l),out.d34s(l),out.d33s(l),out.D33S(l),...
         out.D33Scalc(l),out.sig34s(l),out.sig33s(l),...
         out.sig33S(l),out.sig33SC(l)] = Boot_fun(df,t,O,Kwpy);
        Dws = out.D33S(l); % D33 of weathered S (Wgyp and Wpyr)
                        % if boot_fun (above) is commented out, it will use
                        % previous out put of bootfun that was saved (much
                        % faster)
           dws = out.d34s(l);    
        SOflux2.t(l) % Just a time output to keep track of pace.
        
        % D33s of SwSulf pool.
% Dbpy vs Dbpy1. Dbpy1 is what is coming out of the Sulfate pool. Dbpy is
% that plus direct S8 addition.
DSulf = (DSulf*Si + DvSulf*FvSulf + DvS8*FvS8 + Dws*Fws - (Dbpy1*Fbpy) - (Dbgyp*Fbgyp)) / (S);   
        % outputting results into separate table (pool)
        pool.age(l) = SOflux2.t(l);
        pool.D33S(l) = DSulf;
        pool.Dbpy(l) = Dbpy;
        pool.Fbpy(l) = Fbpy;
        pool.Fbgyp(l) = Fbgyp;
        pool.Fbs(l) = Fbs;
        pool.FvS8(l) = FvS8;
        pool.FvSulf(l) = FvSulf;
        pool.RPy(l) = FS8py/Fbpy;
        pool.S(l) = S;
        pool.SiR(l) = S/Si;
        pool.sig33SC = out.sig33SC;


end
 %% Fitting
% Smoothing the Weathered sulfur pool output to a loess fitted line
       out.D33Sfit = smooth(out.age,out.D33Scalc,0.3,'loess');
       out.D33Sfitup = smooth(out.age,out.D33Scalc+out.sig33SC,0.3,'loess');
       out.D33Sfitlow = smooth(out.age,out.D33Scalc-out.sig33SC,0.3,'loess');
% Smoothing the SWSR poop output to a loess fitted line
pool.D33Sfit = smooth(pool.age,pool.D33S,0.3,'loess');
       pool.D33Sfitup = smooth(pool.age,pool.D33S+pool.sig33SC,0.3,'loess');
       pool.D33Sfitlow = smooth(pool.age,pool.D33S-pool.sig33SC,0.3,'loess');

       %% Figures      
x= out.age;
y1=out.D33Sfitup;
y2=out.D33Sfitlow;
       figure(17)
     subplot(2,1,1)
      plot(out.age, out.D33Scalc, ...
          out.age,out.D33Sfit,'r',...
            out.age,out.D33Sfitup,'r--',...
            out.age,out.D33Sfitlow,'r--','LineWidth',3);
      legend('Weathered S RawBoot','Weathered S Fit','Weathered S Fit upper',...
             'Weathered S Fit lower');
      title('Weathered Sulphur Pool')
      ylabel('∆33S ‰')
      xlabel('time (Ga)') 
     %plot(pool.age,pool.D33S,pool.age,pool.Dbpy,'b--');
     subplot(2,1,2)
     plot(pool.age,pool.D33Sfit,'g',...
         pool.age,pool.D33Sfitup,'g--',...
         pool.age,pool.D33Sfitlow,'g--','LineWidth',3);
         yline(0.3,'--');
         yline(-0.3,'--');
         legend('Sw Sulfate Fit','Sw Sulfate Fit upper',...
             'Sw Sulfate Fit lower')
        title('Seawater Sulphate Pool')
        ylabel('∆33S ‰')
        xlabel('time (Ga)') 
