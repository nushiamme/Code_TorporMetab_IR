**Paper authors: **

Code by: Anusha Shankar, github/nushiamme; contact: nushiamme\<at\>gmail\<dot\>com for questions about code/datasets


#### Zenodo archive


#### Abstract


#### Code organisation

The code is organized by data type into 4 scripts, for
1. Temperature and thermoregulatory cost calculations,
2. Floral resources (floral abundance)
3. The energy budget model, and
4. A model of daily energy expenditure as a function of resources, temperature, and bird mass

Figures not listed here were conceptual figures made in powerpoint or in prism.

-   **Temp\_EnergyBudget.R** - Needs input files *"BBLH_temperatures_compiled.csv"*, *"Melted_Te_thermo.csv"*, *"Melted_Ta_thermo.csv"*; contains code for producing and compiling thermoregulatory costs for thermoregulation component of the energy budget model.
    -   *Figure 1d*: Distribution of temperatures at Harshaw and Sonoita, facetted by day and night.
    
-   **Energy\_budget\_resources.R** - Needs input file *"FloralCensusData2013.csv"*; code to plot resource availability across sites and seasons
    -   *Figure 1e*: 

-   **Energy\_budget\_BBLH\_Aug2018.R** - Needs input files *"EnergyBudget_model_values.csv"*, *"DLW_summary"*, *"Costas1986_VO2_DRPowers.csv"*, *"Validation_Enrichment_dose_A.csv"*, *"Validation_enrichment_eqb_B.csv"*, and *"Validation_CO2produc_dose_C.csv"*; Contains code to analyse energy budget models and doubly labelled water (DLW) data 
    -   *Figure 2*: Energy budget models compared against DLW measurements of daily energy expenditure. The left panel compares Harshaw DLW and model measurements, with individuals recaptured multiple times as colored points. The right panel compares Sonoita DLW and model values.
    -   *Figure 3*: Stacked bar graph showing the different modeled components of the daily energy budget 
    -   *Supplementary Figure S1*: Validation of the modified DLW method
    -   *Supplementary Figure S3*: Scholander-Irving curve for Costa's hummingbirds
    
-   **Energy\_budget\_BBLH\_models.R** - Needs input files *"FloralCensusData2013.csv"*, *"Melted_Ta_thermo.csv"*, *"DLW_summary.csv"*; Contains code to analyse daily energy expenditure in the context of environmental factors
    -   *Table S1*: 


#### Packages you will need for all three scripts:

    + reshape2 or reshape
    + ggplot2
    + dplyr
    + grid
    
#### Additional packages you will need for the temperature-thermoregulation script:

    + data.table
    + gridExtra
    + scales

#### Additional packages you will need for the resources script:
    
    + plyr
    + ggthemes
    
#### Additional packages you will need for the model script:
    
    + plyr
    + stringr
    + lme4
    
