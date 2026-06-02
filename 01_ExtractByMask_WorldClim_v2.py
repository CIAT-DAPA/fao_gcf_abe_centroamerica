# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpose: Extract by mask grids in a workspace
# Updated for ArcGIS Pro / Python 3 / arcpy
# ---------------------------------------------------------

import arcpy
import os
import sys
from arcpy.sa import ExtractByMask

# -------------------------------------------------------------------
# CHECK ARGUMENTS
# -------------------------------------------------------------------

if len(sys.argv) < 5:
    print("\nToo few args")
    print("Syntax:")
    print("C:\Program Files\ArcGIS\Pro\bin\Python\envs\arcgispro-py3\python.exe ExtractByMask.py <dirbase> <dirout> <mask> <wildcard>")
    print(
        r'python 01_ExtractByMask_WorldClim_v2.py '
        r'S:\observed\gridded_products\worldclim\Global_2_5min_v2_1 '
        r'E:\fao_gcf_abe\01_baseline\dom\wcl_v21_2_5min '
        r'E:\fao_gcf_abe\00_admin_data\dom_msk_2_5m.tif '
        r'ALL'
    )
    sys.exit(1)

# -------------------------------------------------------------------
# ARGUMENTS
# -------------------------------------------------------------------

dirbase = sys.argv[1]
dirout = sys.argv[2]
mask = sys.argv[3]
wildcard = sys.argv[4]

# -------------------------------------------------------------------
# CHECK OUT SPATIAL ANALYST LICENSE
# -------------------------------------------------------------------

arcpy.CheckOutExtension("Spatial")

# -------------------------------------------------------------------
# HEADER
# -------------------------------------------------------------------

print("\n~~~~~~~~~~~~~~~~~~~~~~~~~")
print("     EXTRACT BY MASK")
print("~~~~~~~~~~~~~~~~~~~~~~~~~\n")

# -------------------------------------------------------------------
# CREATE OUTPUT DIRECTORY
# -------------------------------------------------------------------

if not os.path.exists(dirout):
    os.makedirs(dirout)

# -------------------------------------------------------------------


# -------------------------------------------------------------------
# ENVIRONMENT SETTINGS

# -------------------------------------------------------------------

arcpy.env.workspace = dirbase
arcpy.env.scratchWorkspace = dirout
arcpy.env.overwriteOutput = True
arcpy.env.pyramid = "NONE"
arcpy.env.rasterStatistics = "NONE"
# LIST RASTERS

# -------------------------------------------------------------------

print(f"\t..listing grids into {dirbase}")

#if wildcard.upper() == "ALL":
#    rasters = sorted(arcpy.ListRasters("*", "TIF"))
#else:
#    rasters = sorted(arcpy.ListRasters(f"{wildcard}*", "TIF"))

if wildcard.upper() == "ALL":
    rasters = sorted(arcpy.ListRasters("wc2.1*", "TIF"))
else:
    rasters = sorted(arcpy.ListRasters(f"{wildcard}*", "TIF"))

# -------------------------------------------------------------------
# PROCESS RASTERS
# -------------------------------------------------------------------

for raster in rasters:

    filename = os.path.basename(raster)

    parts = filename.split("_")

    # Example:
    # wc2.1_2.5m_tavg_01.tif

    var = parts[2]

    if var == "tavg":
        varmod = "tmean"
    else:
        varmod = var

    mon = int(parts[3].split(".")[0])

    outname = f"{varmod}_{mon}.tif"
    outraster = os.path.join(dirout, outname)

    # ---------------------------------------------------------------
    # EXTRACT BY MASK
    # ---------------------------------------------------------------

    extracted = ExtractByMask(raster, mask)
    extracted.save(outraster)

    print(f"\t{filename} extracted")

# -------------------------------------------------------------------
# FINISH
# -------------------------------------------------------------------

print("\n\tProcess done!!")

# -------------------------------------------------------------------
# CHECK IN LICENSE
# -------------------------------------------------------------------

arcpy.CheckInExtension("Spatial")