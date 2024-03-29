%% Archean Oxygen Methane Sulfur model
% Model produced by Prof. Shuhei Ono (MIT) reproducing Claire (2006) model
% then adding sulfur chemistry. Model was originally built by Prof. Ono
% for Luo et al., 2016 (DOI: 10.1126/sciadv.16001), and subsequently 
% updated for Uveges et al., 2022 (DOI: )
%% Model setup
% constants
    Fprod = 400; % Primary productivity in Tmol year-1
    Fborg = 12; %10; % organic carbon burial in Tmol year-1
% initial conditions
    M = 0.001*1.8e+8; % initial methane in atmosphere (Tmol)
    O = 9e-8*1.8e+8; % initial oxygen in atmosphere (Tmol)
    S = 1.4e+4; % initial sulfate in ocean in Tmol
    GYP = 0; % initial gypsum Tmol
    t = 3.2; % initial time this is 4 Ga
    dt = 0.001; % initial time step in years

    Fbpy = 1.25; %   initial pyrite burial
    Fbgyp = 5.99e-8*S; % initial gypsum burial
    Fwpy = 0; % initial Oxidative pyrite weathering fraction
    fS8 = 0.25;
    kS8 = 0.15;
    % Setting initial empty data output table
data(1,1:5) = [t O M S GYP];
%% Model
    nn=1;
for n=1:15000  %30000
    
    m = M/1.8e+8; o = O/1.8e+8; % this is methane and oxygen in bars 
    s = S/1.4e+9; % sulfate in mol/kg
    
    katm = 10^keff(log10(o),log10(m)); % rate constant for CH4 and O2 sink

    Fresp = O/(3.7e+5+O)*(Fprod-Fborg-2*Fbpy); % Aerobic respiration
    Fferm = Fprod - Fresp - 2*Fbpy - Fborg; % Flux of organics fermented to methane
    
    %if t>=2.33; Fvr = 14.3; Fvs = 1; end % default is 18.3
    %if t<2.33; Fvr = 10; Fvs = 1; end % default is 10
    
 %   if t<2.5 & t>2.3; Fvr = 8.3/0.2*(t-2.3)+12; end
    
 %   if t<2.00; Fvr = 18.3; Fvs = 1; end
    
   
    Fvr = 4.8*exp(0.45*t); % Flux of reducing gas from volcanoes
    % 4.8*exp(0.4406*t); % 14.52*exp(-0.246*(4.5-t))+2; 
    Fvs = 0.0825*Fvr; % Volcanic sulfur flux
    FvS8 = Fvs*fS8;
    FvSulf = Fvs*(1-fS8);
    if O > 360 %This is a rough calc for # 
                                %of Tmol = 10^-5 atm (check/do better)
            kS8 = 0; % Indicating all S8 is mixed into SwSulf, 
                           % net ?33 of Volcanic S now 0
    end
    Fvs = Fvs - FvS8*kS8;
    FS8py = FvS8*kS8;
    FvS8 = FvS8-FS8py;
 %  Fvr = (0.0226*t^5.13+6.81)*(1-0.025); % volcanic gas O2 sink 
    Fwo = 8.29e-4*O^0.5; % O2 sink due to weathering
 %  Fvs = 0.048*Fvr; % volcanic sulfur flux
    Fwgyp = 2.16e-8*GYP; % Weathering flux of gypsum
 %   Fwpy = 1.25*O/(3.7e+4+O);
    Fesc = 3.7e-5*M; % Rate of hydrogen escape
    
% lump together explicit terms    
    Fexo = Fprod-Fresp-Fvr-Fwo-Fesc-1/2*Fvs-2*Fwpy;
    Fexm = 1/2*Fferm-Fesc;
    Fexs = Fvs + Fwgyp;
        
%    dMdt(n) = Fexm - kOM;
%    dOdt(n) = Fexo - 2kOM;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
% solve sulfur by implicit scheme
%  Fbpy = kbpy*S/(Kbpy+S)*O^theta
    theta = -0.035;  
    Kbpy = 2.8e+5; % Km at 200 uM SO4
    kbpy = 3.53; 

    f1 = S/(Kbpy+S); % Sulfate / pyrite burial constant+Sulfate
    df1 = Kbpy*(Kbpy+S)^-2;  % derivative of the above
    g1 = O^theta;  % Oxygen up to a constant
    dg1 = theta*O^(theta-1); % Derivative of the above
    
% Fwpy = kwpy*O/(Kwpy+O)   
% Kwpy is in Tmol of O2, kwpy
% Flux of pyrite weathering
    kwpy = 1.25;
    Kwpy = 3.7e+4;        
    %g2 = O*(Kwpy+O)^-1; % Oxygen / Oxygen+ pyrite weathering constant
    %dg2 = Kwpy*(Kwpy+O)^-2; % Derivative of above (quotient rule)

    g2 = (O^0.5)*((Kwpy^0.5)+(O^0.5))^-1; % Oxygen / Oxygen+ pyrite weathering constant half power version
    dg2 = Kwpy/(2*(O^0.5)*((Kwpy+(O^0.5))^2)); % Derivative of above (quotient rule)half power version

% Gypsum burial    
    kbgyp = 6.4e-8; % Gypsum burial constant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

    %dGYPdt = -kbgyp*S+Fwgyp;  % Change in total amount of gypsum per timestep
    dGYPdt = kbgyp*S-Fwgyp; % Changed to this from above
    if dGYPdt>0.03; dGYPdt=0.03; end % Check that limits how fast gypsum can change

    a1 = 1/dt + 2*katm*M; % 1/timestep + atm constant*Methane
    b1 = 2*katm*O;
    a2 = katm*M;
    a3 = kbpy*f1*dg1-kwpy*dg2;
    b2 = 1/dt + katm*O;
    c3 = 1/dt+kbpy*df1*g1+kbgyp;  
    
    A = [a1 b1 0; a2 b2 0; a3 0 c3];
    g = [Fexo-2*katm*O*M; Fexm-katm*O*M; Fexs-kbpy*f1*g1+kwpy*g2-kbgyp*S] ; 
    z = A\g; % z(1) = dO, z(2) = dM; linear algebra voodoo magic (inverse of 3x3 is a pain...)
    Z = [O; M; S];
    
    maxtor = 0.1; % maximum torelance
    
    % z is an increment, if z is larger than max tolerance, end the loop.
    if all(abs(z./Z)<=maxtor)==1 & isreal(z)==1; % if both dM/M and dO/O are < 0.1,          
        O = O+z(1);
        M = M+z(2); 
        S = S+z(3); 
        ds = z(3);
        GYP = GYP+dGYPdt*dt;   % Added this back in  
    
        data(nn,1:5) = [t O M S GYP];
        Fbgyp = kbgyp*S; 
        Fbpy = kbpy*S/(Kbpy+S)*O^theta;
        Fwpy = kwpy*O/(Kwpy+O);
        fluxS(nn,:) = [t Fvs Fbpy Fwpy Fbgyp Fwgyp]; %sulfur fluxes array?
        fluxO(nn,:) = [t Fbpy Fesc Fvr Fborg Fwo];
        SOflux(nn,:) = [t O S Fvs Fbpy Fwpy Fbgyp Fwgyp FS8py FvS8 FvSulf dt ds];
     
    %   Bootstrap function for MIF weathering   
%  [out.age(nn),out.d34s(nn),out.d33s(nn),out.D33S(nn),out.D33Scalc(nn),...
%      out.sig34s(nn),out.sig33s(nn),out.sig33S(nn),out.sig33SC(nn),...
%      out.Ox(nn)] = Boot_fun(df,t,O,Kwpy);

            t = t-dt*1e-9;
        nn= nn+1;
        dtn(nn) = dt;
    end
   
        DTn(n) = dt;
        zZ1(n) = z(1)./Z(1);
        katmn(n) = katm;
    
    if isreal(z)==0; dt = dt/4; end    
    if any(abs(z./Z)>maxtor)==1; dt = dt/2; end
    if all(abs(z./Z)<maxtor*0.5)==1; dt = 1.5*dt; end   
    if dt >1e+5; dt = 1e+5; end
  
    %if t<2.331 & t>2.299
    %    dt = 1000;
    %end
    
    
    
    [n t*1000]
    if t<2.2; n=10000;end
end

    agemax = 2.8; agemin = 1.8; 
    agemax2 = 2.36; agemin2 = 2.28;
%% Figures
figure(1)
subplot(4,2,1) % data(:,1) is time in Gigayears
    H(1)= plot(data(:,1),log10(data(:,2)/1.8e+8)); hold on % data(:,2) is 
                %O2 in Tmoles. 1.8e+8 is molecules of all gas in atm. Result is partial pressure of O2.
    H(2) = plot(data(:,1),log10(data(:,3)/1.8e+8)); hold off
    legend(H, 'O2 in bar', 'CH4 in bar', 'Location', 'southwest'); 
    ylabel('log(p) bars')
    xlabel('time (Ga)') 
    ax = axis; ax(4)=0;
    axis([agemin agemax ax(3) ax(4)]);
    
subplot(4,4,3)
    H(1)= plot(data(:,1),log10(data(:,2)/1.8e+8)); hold on
    H(2) = plot(data(:,1),log10(data(:,3)/1.8e+8)); hold off
    legend(H, 'O2 in bar', 'CH4 in bar', 'Location', 'southwest'); 
    ylabel('log(p) bars')
    xlabel('time (Ga)') 
    ax = axis; ax(4)=0;
    axis([agemin2 agemax2 ax(3) ax(4)]);
    
subplot(4,2,3)
    H= plot(data(:,1),data(:,4)/1.4e+9*1e+3); %hold on
    %H(2) = plot(data(:,1),data(:,5)); hold off
    legend(H, 'SO4', 'Location', 'southwest'); %, 'Gypsum in Tmol'); 
    ylabel('mmol/kg')
    xlabel('time (Ga)') 
    ax = axis;
    axis([agemin agemax ax(3) ax(4)]);

subplot(4,4,7)
    H= plot(data(:,1),data(:,4)/1.4e+9*1e+3); %*1e+3); %hold on
    %H(2) = plot(data(:,1),data(:,5)); hold off
    legend(H, 'SO4', 'Location', 'southwest'); %, 'Gypsum in Tmol'); 
    ylabel('mmol/kg')
    xlabel('time (Ga)') 
    ax = axis;
    axis([agemin2 agemax2 ax(3) ax(4)]);
    
   %   fluxS(nn,:) = [t Fvs Fbpy Fwpy Fbgyp Fwgyp];
   %   fluxO(nn,:) = [t Fbpy Fesc Fvr Fborg Fwo];

figure(2)
subplot(2,2,1)   
    h(1)=plot(fluxO(:,1),fluxO(:,2)); hold on
    h(2)=plot(fluxO(:,1),fluxO(:,3)); 
    h(3)=plot(fluxO(:,1),fluxO(:,4)); 
    h(4)=plot(fluxO(:,1),fluxO(:,5)); 
    h(5)=plot(fluxO(:,1),fluxO(:,6));hold off
   legend(h, 'Fbpy','Fesc','Fvr','Fborg','Fwo', 'Location','southwest') %, 'Fresp', 'Fferm') 
   ylabel('log(Flux) (Tmol/year)')
   xlabel('time (Ga)') 
    ax = axis;
    axis([agemin agemax ax(3) ax(4)]);
   
subplot(2,2,2)   
    h(1)=plot(fluxO(:,1),log10(fluxO(:,2))); hold on
    h(2)=plot(fluxO(:,1),log10(fluxO(:,3))); 
    h(3)=plot(fluxO(:,1),log10(fluxO(:,4))); 
    h(4)=plot(fluxO(:,1),log10(fluxO(:,5))); 
    h(5)=plot(fluxO(:,1),log10(fluxO(:,6)));hold off
   legend(h, 'Fbpy','Fesc','Fvr','Fborg','Fwo', 'Location','southwest') %, 'Fresp', 'Fferm') 
   ylabel('log(Flux) (Tmol/year)')
   xlabel('time (Ga)') 
    ax = axis;
    axis([agemin2 agemax2 ax(3) ax(4)]);
    
subplot(2,2,3)   
    h(1)=plot(fluxS(:,1),log10(fluxS(:,2))); hold on
    h(2)=plot(fluxS(:,1),log10(fluxS(:,3))); 
    h(3)=plot(fluxS(:,1),log10(fluxS(:,4))); 
    h(4)=plot(fluxS(:,1),log10(fluxS(:,5))); 
    h(5)=plot(fluxS(:,1),log10(fluxS(:,6)));hold off
   legend(h, 'Fvs','Fbpy','Fwpy','Fbgyp','Fwgyp', 'Location','southwest') %, 'Fresp', 'Fferm') 
   ylabel('log(Flux) (Tmol/year)')
   xlabel('time (Ga)') 
    ax = axis;
    axis([agemin agemax -5 1]);

subplot(2,2,4)   
    h(1)=plot(fluxS(:,1),log10(fluxS(:,2))); hold on
    h(2)=plot(fluxS(:,1),log10(fluxS(:,3))); 
    h(3)=plot(fluxS(:,1),log10(fluxS(:,4))); 
    h(4)=plot(fluxS(:,1),log10(fluxS(:,5))); 
    h(5)=plot(fluxS(:,1),log10(fluxS(:,6)));hold off
   legend(h, 'Fvs','Fbpy','Fwpy','Fbgyp','Fwgyp', 'Location','southwest') %, 'Fresp', 'Fferm') 
   ylabel('log(Flux) (Tmol/year)')
   xlabel('time (Ga)') 
    ax = axis;
    axis([agemin2 agemax2 -5 1]);
        
figure(3)
    subplot(2,1,1); plot(DTn)
    subplot(2,1,2); plot(1: length(zZ1), abs(zZ1))
    
% figure(4)
%     plot(out.age,out.D33S)
%     
%     figure(5) 
%         plot(SOfluxM.t,SOfluxM.O)
%     