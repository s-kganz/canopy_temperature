from pyproj import Transformer

modis_proj = '+proj=sinu +R=6371007.181 +nadgrids=@null +wktext'
latlon_to_modis_tform = Transformer.from_crs("EPSG:4326", modis_proj, always_xy=True)
modis_to_latlon_tform = Transformer.from_crs(modis_proj, "EPSG:4326", always_xy=True)

def latlon_to_modis(lat, lon):
    '''
    Convert from latitude, longitude to MODIS X/Y. Returns coordinates
    in the form (mod_x, mod_y).
    '''
    return latlon_to_modis_tform.transform(lon, lat)

def modis_to_latlon(mod_x, mod_y):
    '''
    Convert from MODIS X/Y to latitude, longitude. Returns coordinates
    in the form (lon, lat).
    '''
    return modis_to_latlon_tform.transform(mod_x, mod_y)