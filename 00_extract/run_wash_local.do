// ***********************************************************************************************************
//  Author: Patrick Liu (pyliu@uw.edu)																		
//  Date: 7/13/2015
//  Project: ubCov
//  Purpose: Run Script
																					
// ***********************************************************************************************************/


//////////////////////////////////
// Setup
//////////////////////////////////

if c(os) == "Unix" {
	local j "/home/j"
	set odbcmgr unixodbc
}
else if c(os) == "Windows" {
	local j "J:"
}

clear all
set more off
set obs 1

// Settings
local central_root "`j'/WORK/01_covariates/common/ubcov_central"
local topics wash

// Load Jun's functions
local jun_root "`j'/temp/jkim118/ubcov_translation"
cd "`jun_root'"
do "`jun_root'/ubcov_translation/get_translated_labels.do"
do "`jun_root'/load_stata_14/load_stata_14.do"

// Load functions
cd "`central_root'"
do "`central_root'/modules/extract/core/load.do"


cd "`central_root'"
// Load the base code for ubCov
local paths  `central_root'/modules/extract/core/ `central_root'/modules/extract/core/addons/
foreach path in `paths' {
	local files : dir "`path'" files "*.do"
	foreach file in `files' {
		if "`file'" != "run.do" do "`path'/`file'"
	}
}

// Make sure you're in central
cd `central_root'

// Initialize the system
/* 
	Brings in the databases, after which you can run
	extraction or sourcing functions like: new_topic_rows

	You can view each of the loaded databases by running: get, *db* (eg. get, codebook)
*/

ubcov_path
init, topics(`topics')

// Run extraction
/* Launches extract

	Arguments:
		- ubcov_id: The id of the codebook row
	Optional:
		- keep: Keeps 
		- bypass: Skips the extraction check
		- run_all: Loops through all ubcov_ids in the codebook.
*/

local outpath = "`j'/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/wash_2"

local array 5809
foreach number in `array'{
	local i `number'
	run_extract `i', bypass_map bypass //keep
	tostring year_start, gen(year_n)
	tostring year_end, gen(end_year_n)
	tostring nid, gen(nid_n)
	local filename = survey_name + "_" + nid_n + "_" + survey_module + "_" + ihme_loc_id + "_" + year_n + "_" + end_year_n
	local filename = subinstr("`filename'", "/", "_",.)
	// recodes for postprocessing
	do "`j'/WORK/11_geospatial/07_data extraction/WASH/wash_recodes.do"
	drop year_n end_year_n nid_n
	cd 	`outpath'
	saveold "`filename'", replace
}
