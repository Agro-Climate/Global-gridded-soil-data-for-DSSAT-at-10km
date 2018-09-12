# Global-gridded-soil-data-for-DSSAT-at-10km

Global high-resolution (10km) soil profile database for DSSAT.
The dataset was developed by The HarvestChoice Project at IFPRI, in collaboration with IRI at Columbia University and Michigan State University

The dataset was developed using ISRIC’s SoilGrids, aggregated from its 1 km resolution to 10 km (5 arc-minute).
Six soil properties (bulk density, organic carbon, percentage of clay and silt, soil pH and cation exchange capacity) were directly used as DSSAT inputs, and pedo-transfer functions were used to derive soil hydraulic properties (saturated hydraulic conductivity, soil water content at field capacity, wilting point and saturation). 
Final outputs are provided in *.SOL file format for each country at 5-min resolution. 

The working paper and all data files, including the soil profiles (*.SOL) at 10 km resolution organized by country are available to download at IFPRI’s Dataverse at http://dx.doi.org/10.7910/DVN/1PEEY0.


Citation:



What can you find on this github repository:

Fortran code for taking soil properties from SoilGrids1km, applying pedotransfer function to derive soil hydraulic properties and generating soil output files at 10km
