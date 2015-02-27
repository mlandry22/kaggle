% The purpose of this script is as follows:
%   1) Used for EEG Kaggle Competition
%   2) Data is then processed into "feature" sets using the B-J
%   time-frequency kernel.
%   3) Once the "feature" sets have been extracted, the "features" are
%   processed using a time-series model or decomposed into principal components.
%
% File Version History:
%   01/27/2015      Original version written by Robert Chong
%   02/11/2015      Starting to work with the team
%   02/20/2015		adapting slightly to take in R's as.matrix .mat files

% House keeping
clear all; clc; close all


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Set paths that contain m-files
%addpath(genpath('/Users/rchong/Documents/MATLAB/'))         % Path adds the time-frequency toolboxes
%addpath(genpath('/Users/rchong/Downloads/BCI_Challenge/'))  % Path adds the current script and data
addpath(genpath('/home/mark/Documents/bci'))
%addpath(genpath('/home/mark/Documents/bci/tfdn'))
%addpath(genpath('/home/mark/Documents/bci/NaN'))
%addpath(genpath('/home/mark/Documents/bci/statistics'))

%% loop through each person and session; to speed this up, I broke this list into four pieces and
%%   had separate sessions working on each piece; if you do that you need to adjust the personList and k length
personList = ["S01","S02","S03","S04","S05","S06","S07","S08","S09","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20","S21","S22","S23","S24","S25","S26"];
sessionList= ["01","02","03","04","05"];

for k = 1:26
    for j = 1:5
        fileBase = strcat('train/Data_',personList((1+(k-1)*3):k*3),'_Sess',sessionList((1+(j-1)*2):j*2));
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %   Load dataset & Assign Variable names
        load(strcat(fileBase,'.mat'));
        %tsFile = csvread('/Users/rchong/Downloads/BCI_Challenge/train/Data_S02_Sess01.csv')

        % Find all the index where FeedBackEvent equals 1
        idx = find(x(1:length(x),59));

        for i = 1:length(idx)
            idxPos = idx(i);
            data = x(idxPos:(idxPos+260),30);
            %30 is the column for Cz; eventually loop through the columns of choice, if time

            fSample = 200;
            windowSize = 1000;
            alpha = 0.27;

            %tfr_AF3 = rgk(data,alpha);
            tfr_AF3 = bintfd(data,windowSize,'Hamming','Analytic',0,'PosOnly','NoMex');
            tfrRGK = tfr_AF3;

            %   Time
            TAtimeMargRGK = t_margin(tfrRGK); % tfdn-toolbox -- not divided [m x n]
            TAtimeMargRGK = TAtimeMargRGK';
    
            % Marginals
            %   Frequency
            TAfreqMargRGK = f_margin(tfrRGK); % tfdn-toolbox -- not divided [m x n]
    
            % Run PLS Modeling here
            y = zscore(x(idxPos:idxPos+260,58));
            X = zscore(tfrRGK');
            [XL,yl,XS,YS,beta,PCTVAR] = plsregress(X,y,140);

            % save the top 200 PLS components; continually append in the loop for later writing to CSV
            if(i==1)
                outVec = rot90(beta(1:200));
            else
                outVec = vertcat(outVec,rot90(beta(1:200)));
            endif
        end
	% write out the matrix of all PLS components for use later (in R, or anywhere)
	csvwrite(strcat(fileBase,'_pls.csv'),outVec);
        strcat('Done with: ',fileBase)
    end
end
