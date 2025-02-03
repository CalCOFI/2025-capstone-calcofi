% ESPER_calc_all.m

% Use ESPER_Mixed function to compute TA, TC, pH, etc. from combined 
% bottle data using all available predictors

% read in combined bottle data
merged_bottle = readtable("../../data/merged_bottle_data.csv");

% select desired output variables
DesiredVars = 1:7;

% extract coordinates from combined bottle data
OutputCoords = merged_bottle{:,["Longitude", "Latitude", "Depth"]};

% select predictors from combined bottle data
PredictorMeasurements = merged_bottle{:,["Salnty","T_degC","PO4uM","NO3uM","SiO3uM","Oxy__mol_Kg"]};

% indicate predictor types
PredictorTypes = 1:6;

% extract dates from combined bottle data
EstDates = decyear(merged_bottle{:,["Year_UTC","Month_UTC","Day_UTC"]});

% compute estimates and uncertainties using ESPER_Mixed
[Estimates, Uncertainties] = ESPER_Mixed( ...
    DesiredVars, ...
    OutputCoords, ...
    PredictorMeasurements, ...
    PredictorTypes, ...
    'EstDates',EstDates);

% convert ESPER_Mixed outputs to table formats
EstimateTable = splitvars(struct2table(Estimates));
UncertaintyTable = splitvars(struct2table(Uncertainties));

% write output to csv files
writetable(EstimateTable,"../../data/ESPER_output/ESPER_estimates_all.csv")
writetable(UncertaintyTable,"../../data/ESPER_output/ESPER_uncertainties_all.csv")