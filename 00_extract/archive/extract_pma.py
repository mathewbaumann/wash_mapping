import pandas as pd
import numpy as np
import re
import os
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

pd.options.display.float_format = '{:,.0f}'.format

wash_cb = pd.read_csv("C:/Users/gmanny/Downloads/pma_ubcov.csv")
nid_path_matchup = wash_cb[["nid", "file_path"]].set_index("nid").to_dict().items()
nid_path_matchup = {257044: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/BFA/2014_R1/BFA_PMA2020_2014_R1_HH_WN_V1_Y2017M03D07.DTA', 257045: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/BFA/2015_R2/BFA_PMA2020_2015_R2_HH_WN_V1_Y2017M03D07.DTA', 285993: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/BFA/2016_R3/BFA_PMA2020_2016_R3_HH_WN_V1_Y2017M03D07.DTA', 307751: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/BFA/2016_2017_R4/BFA_PMA2020_2016_2017_R4_HH_WN_V1_Y2017M07D11.DTA', 257822: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/COD/2013_R1/COD_KINSHASA_PMA2020_2013_R1_HH_WN_Y2017M03D07.DTA', 257823: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/COD/2014_R2/COD_KINSHASA_PMA2020_2014_R2_HH_WN_V1_Y2017M03D07.DTA', 257826: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/COD/2015_R3/COD_KINSHASA_PMA2020_2015_R3_HH_WN_V1_2017M03D07.DTA', 286019: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/COD/2015_R4/COD_KINSHASA_PMA2020_2015_R4_HH_WN_V1_Y2017M03D07.DTA', 286020: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/COD/2015_R4/COD_KONGO_CENTRAL_PMA2020_2015_R4_HH_WN_V1_Y2017M03D07.DTA', 286054: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/COD/2016_R5/COD_KINSHASA_PMA2020_2016_R5_HH_WN_V1_Y2017M07D11.DTA', 286055: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/COD/2016_R5/COD_KONGO_CENTRAL_PMA2020_2016_R5_HH_WN_V1_Y2017M07D11.DTA', 153503: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/ETH/2014_R1/ETH_PMA2020_2014_R1_HHQFQ_Y2016M05D12.DTA', 256175: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/ETH/2014_R2/ETH_PMA2020_2014_R2_HHQFQ_Y2016M05D12.DTA', 256176: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/ETH/2015_R3/ETH_PMA2020_2015_R3_HHQFQ_Y2016M05D12.DTA', 285891: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/ETH/2016_R4/ETH_PMA2020_2016_R4_HH_WN_Y2017M03D07.DTA', 197904: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/GHA/2013_R1/GHA_PMA2020_2013_R1_HHQFQ_Y2016M05D16.DTA', 256241: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/GHA/2014_R2/GHA_PMA2020_2014_R3_GHA_PMA2020_2014_R2_HHQFQ_Y2016M05D16.DTA', 256243: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/GHA/2014_R3/GHA_PMA2020_2014_R3_HHQFQ_Y2016M05D16.DTA', 256244: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/GHA/2015_R4/GHA_PMA2020_2015_R4_HHQFQ_Y2016M05D17.DTA', 197910: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/KEN/2014_R1/KEN_PMA2020_2014_R1_HHQFQ_Y2016M05D13.DTA', 256338: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/KEN/2014_R2/KEN_PMA2020_2014_R2_HHQFQ_Y2016M05D18.DTA', 256365: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/KEN/2015_R3/KEN_PMA2020_2015_R3_HHQFQ_Y2016M05D18.DTA', 256366: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/KEN/2015_R4/KEN_PMA2020_2015_R4_HH_WN_V1_Y2017M03D07.DTA', 256177: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/NER/NIAMEY_2015_R1/NER_NIAMEY_PMA2020_2015_R1_HHQFQ_Y2016M05D13.DTA', 286052: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/NER/2016_R2/NER_PMA2020_2016_R2_HH_WN_V1_Y2017M06D08.DTA', 256263: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/NGA/KADUNA_2014_R1/NGA_KADUNA_PMA2020_2014_R1_HHQFQ_Y2016M05D17.DTA', 256262: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/NGA/LAGOS_2014_R1/NGA_LAGOS_PMA2020_2014_R1_HHQFQ_Y2016M05D17.DTA', 256268: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/NGA/2015_R2/NGA_KADUNA_PMA2020_2015_R2_HH_WN_V1_Y2017M03D07.DTA', 256267: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/NGA/2015_R2/NGA_LAGOS_PMA2020_2015_R2_HH_WN_V1_Y2017M03D07.DTA', 286022: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/NGA/2016_R3/NGA_PMA2020_2016_R3_HH_WN_V1_Y2017M03D07.DTA', 256199: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/UGA/2014_R1/UGA_PMA2020_2014_R1_HHQFQ_Y2016M05D13.DTA', 256200: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/UGA/2015_R2/UGA_PMA2020_2015_R2_HHQFQ_Y2016M05D17.DTA', 256201: 'J:/LIMITED_USE/PROJECT_FOLDERS/GBD/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/UGA/2015_R3/UGA_PMA2020_2015_R3_HHQFQ_Y2016M05D16.DTA', 285893: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/UGA/2016_R4/UGA_PMA2020_2016_R4_HH_WN_V1_Y2017M03D07.DTA', 257614: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/IDN/2015_R1/IDN_PMA2020_2015_R1_HH_WN_V1_Y2017M03D07.DTA', 285980: 'J:/LIMITED_USE/PROJECT_FOLDERS/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020/IND/2016_R1/IND_RAJASTHAN_PMA2020_2016_R1_HH_WN_Y2017M03D07.DTA'}

extractions = "J:/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/wash_2/"

def list_files(path):
    f = []
    for (dirpath, dirnames, filenames) in os.walk(extractions):
        f.extend(filenames)
        break
    return(f)

def grep_files(files, pattern):
    list_of_files_w_pattern = []
    for file in files:
        if pattern in file:
            list_of_files_w_pattern.append(file)
    return(list_of_files_w_pattern)

def get_time_to_water(i, pma, colname = "water_sources_main_drinking"):
    #1. get main drinking water source
    water_source = pma.loc[i, "water_sources_main_drinking"]
    if pd.isnull(water_source) or "bottled" in water_source or "Bottled" in water_source or "-99" in water_source or "-88" in water_source or water_source == -99 or water_source == -88 or water_source == -88.0 or water_source == -99.0:
        return(None)
        pass
    else:
        #recode compound water_sources
        if water_source == '5. Dug Well: Protected Well':
            water_source = " protected dug well"
        if water_source == '6. Dug Well: Unprotected Well':
            water_source = "unprotected dug well"            
        if water_source == "3. Piped Water: Public tap/standpipe":
            water_source = "piped public"
        if water_source == '2. Piped Water: to yard/plot':
            water_source = "piped yard"
        if water_source == '12. Surface water (River/Dam/Lake/Pond/Stream/Canal/Irrigation Channel)':
            water_source = "surface water"
        if water_source == '1. Piped Water: into dwelling/indoor':
            water_source = "piped indoor"
        if water_source == '10. Tanker Truck':
            water_source = "tanker"
        if 'unprotected_dug_well' in water_source or water_source == "unprotected dug well":
            water_source = "unprotected well"
        if "protected_dug_well" in water_source or water_source == " protected dug well":
            water_source = "protected well"
        if "piped_public" in water_source or "Public tap" in water_source:
            water_source = "piped public"
        if "Cart" in water_source:
            water_source = "cart"
        if "tube well" in water_source or "borehole" in water_source and "tubewell" in labels_as_string:
            water_source = "tubewell"
        if "tubewell" in water_source and "tube well" in labels_as_string:
            water_source = "tube well"
        #if "piped_indoor" in water_source:
            #water_source = "piped water from dwelling"
        if "piped_yard" in water_source:
            water_source = 'piped yard'
        if ":" in water_source:
            water_source = water_source.split(":")[1]
        print(water_source)
        #2. call clean_water_string on that 
        water_source = clean_water_string(water_source)
        #3. use that string to grep the labels
        #4 match that label to a column name
        colname = grep_dict_value(labs, water_source)
        #5. get the time value that corresponds to that column
        time = pma.loc[i, colname]
        #print(time)
        #6. return value
        return(time)

def clean_water_string(string):
    """Use regular expressions to clean PMA water strings"""
    new = re.sub("_", " ", string)
    new = re.sub("[0-9]+. ", "", new)
    new = new.lower()
    return(new)

def grep_dict_value(dicto, searchFor):
    for k in dicto:
        if searchFor in dicto[k]:
            return(k)
            break

def test_uniqueness(list_of_keys, df):
    maybe_unique_key = df[list_of_keys[0]].astype("str")
    for key in list_of_keys:
        if key == list_of_keys[0]:
            pass
        else:
            maybe_unique_key = maybe_unique_key.str.cat(df[key].astype("str"), sep="_")
    maybe_unique_key = maybe_unique_key.tolist()
    if len(maybe_unique_key) != len(set(maybe_unique_key)):
        return(False)
    else:
        return(True)
            
files = list_files(extractions)
pma_extracts = grep_files(files, "PMA2020")

#pma_extracts = ["JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020_197910_HHM_KEN_2014_2014.dta", 
#"JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020_197904_HHM_GHA_2013_2013.dta", 
#"JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020_153503_HHM_ETH_2014_2014.dta", 
#"JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020_286022_HHM_NGA_2016_2016.dta", 
#"JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020_256366_HHM_KEN_2015_2015.dta"]

def gsub_dictionary_value(dictionary, replace, replace_with):
    for key in dictionary.keys():
        if replace in dictionary[key]:
            dictionary[key] = dictionary[key].replace(replace, replace_with)
    return(dictionary)

os.chdir(extractions)

fix_me = "NA"
for file in pma_extracts:
    if "197910" in file:
        fix_me = file
idx = pma_extracts.index(fix_me)
print(len(pma_extracts))

for file in pma_extracts[0:len(pma_extracts)]:
    #print(file)
    #if file != pma_extracts[idx]:
    #    print(file + " didn't match up correctly with index of loop")
    #    break
    #use filename to get info about survey
    broken = file.split("_")
    #iso3 = broken[len(broken)-3]
    #year = broken[len(broken)-2]
    nid = broken[len(broken)-5]
    if nid == "256177":
        #NER survey that reports time to water in standard format
        continue
    #use that info to query survey from ubcov codebook
    path = nid_path_matchup[int(nid)]
    
    #open that filepath
    
    #get labels for columns
    original = pd.read_stata(path, iterator=True)
    labels = original.variable_labels()
    #subset labels to water collection time keys
    labs = {key:value for key, value in labels.items() if "water_collect" in key}
    labs = gsub_dictionary_value(labs, "dug ", "")
    labs = gsub_dictionary_value(labs, "piped public water", "piped water from public")
    labs = gsub_dictionary_value(labs, "piped water to public", "piped public")
    labs = gsub_dictionary_value(labs, "piped water from public", "piped public")
    labs = gsub_dictionary_value(labs, "piped water from yard", "piped yard")
    labs = gsub_dictionary_value(labs, "piped water from dwelling/indoor", "piped indoor")
    labs = gsub_dictionary_value(labs, "piped water in yard/plot", "piped yard")
    labs = gsub_dictionary_value(labs, "piped water in dwelling", "piped water from dwelling")
    labs = gsub_dictionary_value(labs, "piped yard water", "piped yard")
    labs = gsub_dictionary_value(labs, "piped in yard", "piped yard")
    labs = gsub_dictionary_value(labs, "piped water from dwelling", "piped indoor")
    labels_as_string = " ".join(list(labs.values()))
    #open the dataset
    original = pd.read_stata(path, iterator=False)
    original.columns = map(str.lower, original.columns)
    #get time to water columns
    w_time = original.filter(regex=("water_collection_rt*"))
    pma_extracts
    #get water sources
    #water = original["water_sources_main_drinking"]
    #sources = water.unique().dropna()
    
    #query water source for every entry and use it to grab the time to that water source
    for i in range(original.shape[0]):
        time = get_time_to_water(i, pma=original)
        #print(time)
        original.loc[i, "time_to_water"] = time
    
    #subset to key and new column
    print(file)
    if "ea_id" not in original.columns and "cluster_id" in original.columns:
        original = original.rename(columns={"cluster_id":"ea_id"})
    if "ea_id" not in original.columns and "ea" in original.columns:
        original = original.rename(columns={"ea":"ea_id"})
    if "structure" not in original.columns:
        original_w_time = original["time_to_water"]
        coded = pd.read_stata(path, convert_categoricals=False)
        coded.columns = map(str.lower, coded.columns)
        coded = coded[["ea_id", "metainstanceid", "fqmetainstanceid", "relationship", "marital_status_cdr1", "age", "gender"]]
        original_w_time = pd.concat([coded, original_w_time], axis=1)
        original_w_time["metainstanceid"] = original_w_time["metainstanceid"].astype("str")
        original_w_time["hh_id"] = original_w_time["fqmetainstanceid"]
        original_w_time["memberid"] = original_w_time["fqmetainstanceid"].str.cat(original_w_time["relationship"].astype(str), sep="_")
        original_w_time["memberid"] = original_w_time["memberid"].str.cat(original_w_time["marital_status_cdr1"].astype(str), sep="_")
        original_w_time["memberid"] = original_w_time["memberid"].str.cat(original_w_time["age"].astype(str), sep="_")
        original_w_time["memberid"] = original_w_time["memberid"].str.cat(original_w_time["gender"].astype(str), sep="_")
        original_w_time["hh_id"] = original_w_time["hh_id"].str.replace(pat=".0", repl="")
        original_w_time["memberid"] = original_w_time["memberid"].str.replace(pat=".0", repl="")
    else:
        original_w_time = original[["ea_id", "structure", "household", "memberid", "time_to_water"]]
        original_w_time["structure"] = original_w_time["structure"].astype("str")
        original_w_time["household"] = original_w_time["household"].astype("str")
        original_w_time["hh_id"] = original_w_time["structure"].str.cat(original_w_time["household"], sep="_")
        original_w_time["hh_id"] = original_w_time["hh_id"].str.replace(pat=".0", repl="")
    original_w_time = original_w_time[["ea_id", "hh_id", "memberid", "time_to_water"]]
    original_w_time = original_w_time.rename(columns={"ea_id":"psu", "memberid":"line_id", "time_to_water":"mins_ws"})
    ubcov = pd.read_stata(file)
    m = pd.merge(ubcov, original_w_time, how="left", on=["psu", "hh_id", "line_id"])
    m.to_stata("J:/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/wash_2/fixed_pma/" + file, write_index=False)
