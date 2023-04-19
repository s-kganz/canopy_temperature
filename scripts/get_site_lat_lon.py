import pandas as pd
import os
from glob import glob

from util import latlon_to_modis

site_dir = "data_in/ameriflux_canopy_eb/"

sites = glob(os.path.join(site_dir, "*.xlsx"))
print("Found {} sites".format(len(sites)))

def get_site_lat_lon(path):
    target_vars = ["LOCATION_LAT", "LOCATION_LONG"]
    out = {}

    f = pd.read_excel(path)
    # Get the site ID before the other targets
    out["SITE_ID"] = f["SITE_ID"].values[0]
    for tv in target_vars:
        out[tv] = f[f["VARIABLE"].isin([tv])]["DATAVALUE"].values[0]
    
    return out

site_locs = pd.DataFrame(map(get_site_lat_lon, sites))

# Convert coords to numbers
site_locs["LOCATION_LAT"] = pd.to_numeric(site_locs["LOCATION_LAT"])
site_locs["LOCATION_LONG"] = pd.to_numeric(site_locs["LOCATION_LONG"])

site_points_latlon = site_locs[["LOCATION_LAT", "LOCATION_LONG"]].itertuples(index=False)

site_points_modis = list(map(lambda x: latlon_to_modis(x[0], x[1]), site_points_latlon))

site_locs["LOCATION_MODIS_X"] = [s[0] for s in site_points_modis]
site_locs["LOCATION_MODIS_Y"] = [s[1] for s in site_points_modis]

print(site_locs.head())

site_locs.to_csv("data_out/ameriflux_canopy_temp_site_locs.csv", index=False)
        