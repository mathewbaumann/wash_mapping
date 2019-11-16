import numpy as np
import pandas as pd
import feather as ft
import os
import imp

j = "J:/" if os.name == 'nt' else '/snfs1/'

#run interactively here:
#python -i "/snfs2/HOME/gmanny/backups/Documents/Repos/wash_mapping/00_extract/id_incomplete_definition_nids.py"

def list_files(path, full_names=False):
    f = []
    for (dirpath, dirnames, filenames) in os.walk(path):
        f.extend(filenames)
        break
    if full_names:
        full = []
        for fi in f:
            full.append(path + "/" + fi)
        f = full
    return(f)

def grep_files(files, pattern):
    list_of_files_w_pattern = []
    for file in files:
        if pattern in file:
            list_of_files_w_pattern.append(file)
    return(list_of_files_w_pattern)

#get water_source string defintions
defs = list_files(j + "WORK/11_geospatial/wash/definitions", full_names=True)
w_def = grep_files(defs, pattern="w_source_defined")
w_def = w_def[len(w_def)-1]
w = pd.read_csv(w_def)

#get latest point and polygon feathers
feathers = list_files(j + "LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash", full_names=True)
feathers = grep_files(feathers, pattern=".feather")
pts = grep_files(feathers, pattern="points")
pts = sorted(pts)[len(pts)-1]
polys = grep_files(feathers, pattern="poly")
polys = sorted(polys)[len(polys)-1]
print(pts)
print(polys)

pt = ft.read_dataframe(pts).encode('windows-1252') #doesn't work unless the feathers are saved in R with utf-8 encoding
poly = ft.read_dataframe(polys).encode('windows-1252') #doesn't work unless the feathers are saved in R with utf-8 encoding

all = pd.concat([pt, poly], axis=0, ignore_index=True)

list(pt.columns.values)
