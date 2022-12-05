function f = keff(x,y )
% This function returns keffective for second order rate constant utilized
% Originally produced by Prof. Shuhei Ono for Luo et al., 2016 
% (DOI: 10.1126/sciadv.16001), and subsequently updated for 
% Uveges et al., 2022 (DOI: )

% keff*[CH4][O2]
% units are keff in Tmol CH4^-1 year ^-1, in log10 unit
% x is [O2], y is [CH4] in log10 bar 

       p00 =      -9.692; %  (-14.3, -5.088)
       p10 =      -1.578; %  (-2.162, -0.9933)
       p01 =       0.891; %  (-4.214, 5.996)
       p20 =      0.1101; %  (0.009856, 0.2104)
       p11 =     -0.4401;%  (-0.7377, -0.1426)
       p02 =      0.4981;%  (-1.548, 2.544)
       p30 =     0.04172;%  (0.03226, 0.05119)
       p21 =    -0.07495;%  (-0.09432, -0.05558)
       p12 =    -0.03381 ;% (-0.09766, 0.03004)
       p03 =     0.08544 ;% (-0.2655, 0.4364)
       p40 =    0.002473 ;% (0.002127, 0.002819)
       p31 =   -0.004799 ;% (-0.005497, -0.004102)
       p22 =     0.00117 ;% (-0.0005347, 0.002875)
       p13 =   -0.003449 ;% (-0.008422, 0.001523)
       p04 =    0.005306 ;% (-0.01651, 0.02712)

     f = p00 + p10*x + p01*y + p20*x.^2 + p11*x.*y + p02*y.^2 + p30*x.^3 + p21*x.^2.*y ...
                    + p12*x.*y.^2 + p03*y.^3 + p40*x.^4 + p31*x.^3.*y + p22*x.^2.*y.^2 ...
                    + p13*x.*y.^3 + p04*y.^4;

end

