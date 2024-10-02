# getisOrdGiStar.py
# This script allows batch processing of the Getis-Ord Gi* Hot spot analysis. I most recently used this tool
# to determine hot spots for locating areas of disability needs in the Dallas-Fort Worth metroplex.

import arcpy

arcpy.env.workspace = "C:/Users/brett/OneDrive/Documents/Code/R_Scripts/DFWselfcare/DFWselfcare.gdb"
arcpy.env.overwriteOutput = True

start_year = 2012
end_year = 2022

for i in range(start_year, end_year + 1):
    print(f'running year: {i}')
    arcpy.stats.HotSpots(
        Input_Feature_Class= f"selfcare{i}",
        Input_Field="slfcr_p",
        Output_Feature_Class=f"selfcare{i}_HotSpots",
        Conceptualization_of_Spatial_Relationships="INVERSE_DISTANCE",
        Distance_Method="EUCLIDEAN_DISTANCE",
        Standardization="ROW",
        Distance_Band_or_Threshold_Distance=5,
        Self_Potential_Field=None,
        Weights_Matrix_File=None,
        Apply_False_Discovery_Rate__FDR__Correction="NO_FDR",
        number_of_neighbors=None
    )