# Author: Carlos Navarro
# UNIGIS 2022
# Description: Select each country from GADM amd0 and adm1 shpaefile and export them

# Import system modules
import arcpy
from arcpy import env


# 00_gadm_split_by_countries.py 

# Set workspace
# env.workspace = "D:\cenavarro\msc_gis_thesis\00_admin_data"

# Set local variables
in_features = "D:\\cenavarro\\msc_gis_thesis\\00_admin_data\\gadm36_0_camexca.shp"
ctr_list = "BHS", "BLZ", "CRI", "CUB", "DOM", "GTM", "HND", "HTI", "JAM", "MEX", "NIC", "PAN", "PRI", "SLV"  

for ctr in ctr_list:

    out_feature_class = "D:\\cenavarro\\msc_gis_thesis\\00_admin_data\\by_country\\" + ctr + "_adm0.shp"
    where_clause = "\"GID_0\"= + '"+ ctr + "'"
    # '"GID_0" = \'BHS\''
    
    print where_clause
    print out_feature_class
    
    # Execute Select
    arcpy.Select_analysis(in_features, out_feature_class, where_clause)

   
in_features = "D:\\cenavarro\\msc_gis_thesis\\00_admin_data\\gadm36_1_camexca.shp"

for ctr in ctr_list:

    out_feature_class = "D:\\cenavarro\\msc_gis_thesis\\00_admin_data\\by_country\\" + ctr + "_adm1.shp"
    where_clause = "\"GID_0\"= + '"+ ctr + "'"
    # '"GID_0" = \'BHS\''
    
    print where_clause
    print out_feature_class
    
    # Execute Select
    arcpy.Select_analysis(in_features, out_feature_class, where_clause)    
    