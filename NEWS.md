# datapackr 7.6.0
## New features
* Sunset COP21 and COP22
* Added original versions of the COP25 templates

## Bug fixes
* reenabled droping invalid disaggs from the year 2 sheet
* Updated Valid Org Units for 2024 to account for regionalization.
* Removed COP Season parameter

## Minor improvements and fixes
* Updated strip_wb_nas

# datapackr 7.5.0
## New features
* Add utility functions for interacting with PDAP Jobs 

## Bug fixes
* 

## Minor improvements and fixes
* Updated dependencies
* Updated lintr to be more restrictive and updated code to fit new restrictions
* Removed TODO's from the entire code base

# datapackr 7.4.4
## New features
*

## Bug fixes
*

## Minor improvements and fixes
* Updated exported functions and back end param handling.
* Updated documentation

# datapackr 7.4.3

## New features
* 

## Bug fixes
* Updated `checkanalytics` and their associated tests. Relating to PMTCT & TB_STAT columns being based on COP Year.  

## Minor improvements and fixes
* Added tests related to `writePSNUxIM`
* Updated `getCurrentCOPYear` to 2024, and started development on sunsetting this function.
* Removed non-ASCII character from file unPackingChecks
* Fix issue with package imports flagged by devtools::check

# datapackr 7.4.1

## Bug fixes
* Updated valid Org units with changes from 2024

# datapackr 7.4.0

## New features
* Created `createPAWExport` to prepare data to be exported to PAW
* Modified `createDATIMExport` to prepare data to be exported only to DATIM
* Modified `packForDATIM` to feed both of the aforementioned export functions

## Bug fixes
* 

## Minor improvements and fixes
* Added tests around both `createPAWExport`,`createDATIMExport`, and `packForDATIM`
* Please note a few hot fixes related to visual formatting went into main between 7.3.3 and 7.4.0.

# datapackr 7.3.3

## New features
* 

## Bug fixes
* Updated `packForDATIM` to include only certain pop indicators for COP24 in `createDATIMExport`.
* Patched `update_de_coc_co_map` to allow for certain data elements to be included even though they were left off the data entry forms
* Updated `getCOPDataFromDATIM`, as it relates to imports, to work with COP24
* Updated `getMemoIndicators` to allow for COP24
* Updated `memoStructure` to allow for COP24

## Minor improvements and fixes
* 

# datapackr 7.3.2

## New features
* 

## Bug fixes
* Updated `packForDATIM` to included pop indicators for COP24 in `createDATIMExport`.

## Minor improvements and fixes
* 

# datapackr 7.3.1

## New features
* 

## Bug fixes
* Updated how `checkMechanisms` handles dedupe mechs.

## Minor improvements and fixes
* 

# datapackr 7.3.0

## New features
* Updated COP validation rules for 2024
* Updated `checkAnalytics` to analyze 2024 psnu data
* Updated `writePSNUxIM` to be able to write 2024 PSNUxIM tabs


## Bug fixes
* 

## Minor improvements and fixes
* Updated unit tests related to the New features above

# datapackr 7.2.1

## New features
* Updates to COP24 Template


## Bug fixes
* Completed updates related to OPU Data Pack's conversion PSNUxIM tools
* Updated country list for COP24 Target Setting Tool Generation script
* Updated regular expression related to Year2 KP data

## Minor improvements and fixes
* 

# datapackr 7.2.0

## New features
* Finalized model
* Able to produce production 2024 Target Setting Tools


## Bug fixes
* Updated `unPackYear2Sheet` regular expression to better capture data.

## Minor improvements and fixes
* Update to Year 2 tab for OVC to include 18-20 age band
* update default cell to A1 for all tabs
* Updated `packDataPack` to hide rows in Year 2 and AGYW sheets
* Updated documentation related to `getValidOrgUnits`
* Updated documentation related to `extractDuplicateRows`

# datapackr 7.1.1

## New features
* Added support for either PSNUxIM or OPU Data Packs for COP23.


## Bug fixes
* upgrade related to check params and Added exclusion of COP24 for regions

## Minor improvements and fixes
* Reorganized COP24 files
* Cached valid_OrgUnits as CSV
* Updated createDatimExport to handle COP24
* Saved DEsCOCs metadata as CSV accessible via API

# datapackr 7.1.0
* Updated logic related to the PSNUxIm.
* Updated various configuration files for year 2024

## New features
* 

## Bug fixes
*

## Minor improvements and fixes
* Updated home tab title to reflect new fiscal year syntax.
* Updated `createKycahinInfo` to reflect new fiscal year syntax
* Moved `getPSNUs` into its own file
* Updated metadata, reviewed formulas, and formatting. 
* Added tests related to unpackSNUxIM and dataset creation
* Updated year 23 data pack template and schema
* Various small updates to how configuration files are generated 

# datapackr 7.0.3

## New features
* 

## Bug fixes
* Updated `datapackVSDatim` to be more representative.
* Updated COP22 schema update script
* Updated cached map script for cogs for year 2024
* Fixed `getRDA` for unit tests
* Updated unit test related to create schema.

## Minor improvements and fixes
* Added year 2024 to `getHTSModality`
* Updated `unPackSchema` in how it handles multi uid patterns.
* Updated `update_de_coc_co_map` for year 2024
* Added an utility function related to DHIS2 UID's patterns
* Updated various configuration files


# datapackr 7.0.2

## New features
* Opens generation & processing of COP24 Data Packs.

## Bug fixes
* Corrects formatting of randomized Spectrum data saved in testing files from RData to rds.

## Minor improvements and fixes
* Exports writeSpectrumData to allow functionalized writing of Spectrum data to Spectrum tab of Data Packs.
* Updated some minor R package documentation.
* `compareData_DatapackVsDatim` updated to no longer include a dedupe data frame. This is more in line with the current operations of datim, and will assist with imports.


# datapackr 7.0.1

## New features
* COP24 PSNUxIM Tool Template created

## Bug fixes
* Updated dataset levels
* Fixed issue related to ordering by age in the memo tables

## Minor improvements and fixes
* COP24 Data Pack Template updated to incorporate formulas flagged for edit during COP23.
* COP24 Data Pack Template updated to increment FY references in Year 2 tab.
* Package dependencies updated

# datapackr 7.0.0

## New features
* Cop24 Data Pack Template created

## Bug fixes
* Updated valid Org Units to include UG SNU 2 PSNU's

## Minor improvements and fixes
* 

# datapackr 6.3.3

## New features
* 

## Bug fixes
*

## Minor improvements and fixes
* Added `season` as a parameter in most unpacking and packing functions to better
align with new workflows — PSNUxIM tools separate, and both PSNUxIM tool & Data
Pack valid for both COP & OPU. This is in preparation for the deprecation of the
concept of a separate "OPU Tool", as this is now functionally identical to the
PSNUxIM tool used during main COP.

# datapackr 6.3.2

## New features
* 

## Bug fixes
* Fixed issue with merging of message queues when unpacking multiple tools

## Minor improvements and fixes
* Dedupe Handling
* Tool to Datim comparison improvements
* Handling of mech file refresh
* Updated all dependencies to current versions

# datapackr 6.3.1

## New features
* 

## Bug fixes
* 

## Minor improvements and fixes
* Updated handling of zero values for non dedupe mechs.
* Functionalized the process for mapping mechanism data from Data Pack indicators to DATIM dataElements & categoryOptionCombos.

# datapackr 6.3.0

## New features
* 

## Bug fixes
* 

## Minor improvements and fixes
* Updated handling of KP_ESTIMATES dataElements to tag with the current COP_YEAR period, rather than the previous. This is different from how other IMPATT estimates are handled, but was a direct request from OGAC.

# datapackr 6.2.3

## New features
* Adds function to update PSNUxIM values from the main tabs

## Bug fixes
* Addresses some minor issues with duplication in the Data element/COC map

## Minor improvements and fixes
* Fixed an issue with certain tests failing in certain testing infrastructures
* Updated cached organisation unit map from DATIM


# datapackr 6.2.2

## Bug fixes
* Fixed an issue with countries in regions being flagged as having invalid prioritizations
* Fixed an issue with analytics checks when only using a PSNUxIM tool

# datapackr 6.2.1

## Breaking changes

## New features
* Adds support for parsing COP23 PSNUxIM sheets
* Adds wrapper function `unPackToolSet` to handle multiple tools
* Adds functionality for handling Year 2 exports
* Adds utility function `extractRawColumnData` which can retrieve raw data from a sheet
* Created a centralized method for creating DATIM exports

## Minor improvements and fixes
* Added unit tests for PSNU's for COP23 generation
* Updated COP 23 validation rules to match DATIM
* Updated analytics checks for COP23
* Analytics checks comparisons are made with rounded numbers to two significant digits
* Resolved issue with age band differences between tools and DATIM
* Fixed critical formulas
* Updated DE/COC map

# datapackr 6.2.0

## Breaking changes
* Removed support for COP21

## New features
* Added  initial support for parsing COP23 Datapacks
* Added functionality to export COP23 data to export formats
* Added function writeSpectrumData which can be used to populate the DataPack Spectrum tab during testing.
* Added testing helper functions to deal with peculiarities of the CI testing environment.
* Added various tests and testing files for COP23 tools.
* Adds initial parsing methods for COP23 Year2 tabs


## Minor improvements and fixes
* Upgraded CI testing environment to R 4.2.1
* Fixed bug in unPackSchemarelated to detecting invalid column and value types.
* Fixed create schema unit test.
* Removed superfluous warning related to missing PSNUs.
* Fixed issue in unPackingChecks related to the lack of the SNU1 column in COP23 tools.
* Fixed a testing issue related to choosing the correct template to use for testing.
* Updated several unit tests to favor COP23 over COP21.
* Disabled two unit tests for COP21.
* Altered test method from devtools::check to devtools::test, which skips CRAN package checks.

# datapackr 6.1.1

## Breaking changes

## New features
* Script for generating COP 23 test data

## Minor improvements and fixes
* Add additional unit tests to support COP 23
* Various formatting improvements as it relates to the exported file
* Updated valid Org Units and Dataset Levels. 


# datapackr 6.1.0

## Breaking changes
* Fixes issues with getMechanismView. The current implementation was too slow, and prone to timing out. The entire view will be downloaded (which is time consuming) once and then all filters happen on the full cached file.
* Fixes a bug with COP22 data element/category option combo maps. 50+ age bands were being dropped with full COP22 datapacks.

## New features
* Adds a check for defunct disaggregates in the PSNUxim tab.
* Adds an object dataset_levels which provides the organisation unit hierarchy of each country by COP year.
* Adds a test for blank_rows_datim_export which should inform if any rows of any export files to DATIM contain any blanks.
* Fixes dedupe handling in regards to OPU's

## Minor improvements and fixes
* Fixes an issue with add_dp_label for COP22 datapacks. The orgunit type was missing.
* Removes deprecated DATIM API functions, which have been moved to datimutils.
* Add various unit tests.


# datapackr 6.0.0

## Breaking changes
* Users are advised to use`getValidOrgunits(cop_year)` to obtain
a list of valid organisation units for a given COP year.

## New features
* Support generation of COP23 target setting tools
* Correctly supply a list of organisation units by COP year.

## Minor improvements and fixes
* Numerous code fixes and test improvements.


# datapackr 5.5.1

## Breaking changes

## New features

## Minor improvements and fixes
* Added TX_PVLS(D) to memo


# datapackr 5.5.0

## Breaking changes

## New features
* Use datimvalidation's expression lexer to validate incoming indicator expression

## Minor improvements and fixes
* Refactored how `unPackSNUxIM` function operates.
* Retire internal API utility functions and replace with datimutils functions
* Removed duplicative code in `packDataPack` that was already in `packTool`
* Added tests for `packDataPack`


# datapackr 5.4.0

## Breaking changes

## New features
* Adds support for COP22 OPU Data Pack processing
* Adds `mergeDatapack` function for use in merging multiple datapacks along
with related unit test.

## Minor improvements and fixes
* Fixes an issue with COP Memo indicator calculations
* New unit tests for OPU generation
* Fixes an issue with dedupes being dropped in the PSNUxIM tab


# datapackr 5.3.2

## Breaking changes
* Removes `getFY22Prioritizations` in favor of clearer, more stable, internal functions.
* Replaces `getPSNUs` with with more versatile (and appropriately named)
`getDataPackOrgUnits`. Many of the org units used in Data Packs are actually not
PSNUs (e.g., DREAMS SNUs, or Military SNUs), making many of the columns created
previously less than sensible. This function now also produces a tibble instead
of a data frame. In some cases, column names in this output have also slightly
changed.
* Similarly, replaces `add_dp_psnu` with `add_dp_label`.
* `adorn_import_file` can now correctly assign prioritizations to org units below
PSNU (e.g., for DREAMS SNUs in Rwanda & Eswatini).

## New features
* Adds the functions `cache_is_fresh` and `fresh_cache_part` for use in 
anticipated functions allowing the caching of metadata objects retrieved from
the DATIM API, allowing faster processing for repeat requests.
* Adds the function `checkToolEmptySheets` to allow checking and unpacking
processes to detect sheets with either no rows of data, or no text in key header
columns (e.g., PSNU, Age, Sex, KeyPop). In these cases, unpacking and checking
functions cannot accurately assess data, so these sheets should be skipped. This
function results in updates in `unPackingChecks`, `unPackSheets`, and
`unPackDataPackSheet`.

## Minor improvements and fixes


# datapackr 5.3.0

## Breaking changes
* Removes the following functions from the package:
   - `checkComments`
   - `checkDuplicateRows`
   - `checkExternalLinks`
   - `checkMissingMetadata`
   - `checkNumericValues`
   - `checkStructure`
   - `compareTemplateToSchema`
   - `defunctDisaggs`
   - `exportSubnatToDATIM`
   - `getDataValueSets`
   - `packForDATIM_MER`
   - `packForDATIM_OPU`
   - `packForDATIM_UndistributedMER`
* Refactors `packDataPack` and `packOPUDataPack` to work with new `packTool` wrapper function.
   - Common functionality is removed from individual functions and moved into `packTool` to remove duplication.
   - The `packDataPack` function now only takes in `d` and `d2_session` as arguments.
   - The `packOPUDataPack` function now takes in a `d` object, an `undistributed_mer_data` dataframe, and a `d2_session` object as arguments. When an `undistributed_mer_data` object is provided, it will use these target values for distributing rather than taking target values from DATIM.
   - Each function now checks if appropriate model filepath has been provided and then writes tabs into pre-generated file.

## New features
* Add the following functions, along with related tests:
   * `loadSheets`
   * `readSheet`
   * `checkFormulas`
   * `checkDisaggs` (instead of `defunctDisaggs`)
   * `checkInvalidPrioritizations`
   * `checkInvalidOrgUnits`
   * `checkNegativeValues`
   * `checkDecimalValues`
   * `checkMissingMetadata`
   * `checkNonNumeric` (instead of `checkNumericValues`)
   * `checkDupeRows` (instead of `checkDuplicateRows`)
   * `checkColumnStructure` (instead of `checkColStructure`)
   * `checkToolStructure` (instead of `checkStructure`)
   * `checkToolConnections` (instead of `checkExternalLinks`)
   * `checkToolComments` (instead of `checkComments`)
   * `checkSheets`
   * `extract_uid`
   * `extract_uid_all`
* Creates a new wrapper function `packTool` around `packDataPack` and `packOPUDataPack`
* Adds `checkOutputFolder` function to `check_params` set that validates that an output folder has been provided, that the directory exists, and can be written to.

## Minor improvements and fixes
* Refactors `unPackDataPackSheet` to reduce cyclomatic complexity and make it functional over multiple sheets at once.
* Updates `unPackDataPack` to use new check functions (see above).
* Updates `unPackTool` to use `loadDataPack` instead of `createKeychainInfo`.
* Simplifies and generalizes `unPackDataPackSheet`.
* Generalizes `separateDataSets`
* Turns off `checkMissingMetadata` in `unPackSNUxIM` for now.
* Updates `createKeychainInfo` to be more accurate in deducing metadata from submitted Data Packs.
* Updates `loadDataPack` to be production ready.
* Refactors `packForDATIM` to streamline all related functions into one and removes `packForDATIM_MER`, `packForDATIM_OPU`, and `packForDATIM_UndistributedMER`. Updates related functions accordingly.
* Removes deprecated code for `exportSubnatToDatim` function.
* Removes deprecated `compareTemplateToSchema` function after removing last instance of usage in `packDataPack`.
* Creates `data-raw/COP21OPU_PSNUxIM_generation_from_Data_Pack.R` which allows the generation of a COP21 OPU tool that includes the target values from a COP21 Data Pack tool and the % distributions from DATIM.
* Fixes tests for the following functions:
   * `listWorkbookContents`
   * `packForDATIM`
   * `separateDataSets`
   * `unPackSheets`
   * `prepareMemoData`
   * `createAnalytics`


# datapackr 5.2.3

## Breaking changes
- Deprecates `pullDATIMCodeList` and `pullFullCodeList` functions in favor of `getCodeList`
- Retires `api_sql_call` function (DP-603)

## New features
- Creates new function `getCodeList` which utilizes `datimutils` API utilities and replaces `pullDATIMCodeList` and `pullFullCodeList` (DP-599)
   - `getCodeList` uses `cop_year` as a parameter rather than the `FY` parameter previously used by `pullDATIMCodeList` and `pullFullCodeList`

## Minor improvements and fixes
- Adds unit tests for `get_Map_DataPack_DATIM_DEs_COCs` and `checkPSNUData` (DP-272)
- Refactors the following functions to utilize `datimutils` API utilities:
   - `getHTSModality` (DP-597)
   - `map_Cs_to_COs` (DP-605)
 - Refactors package to remove hard dependencies on `stringi`, `glue`, `data.table`, and `readr` packages (DP-670, DP-671, DP-687, DP-693)
 - Refactors package to standardize on `cop_year` over `fiscal_year` and `FY` for parameters (DP-691)
 - Refactors package to standardize usage of `pick_schema` across functions (DP-294)
 - Refactors `packSNUxIM` to reduce cyclomatic complexity (DP-662)

# datapackr 5.2.2

## Bug fixes
* Fixes `checkPSNUData` and `prepareMemoData` to stop crashes on Windows by avoiding paralellization if Windows OS detected
* Adds check to `messageQueue` to avoid potential mismatches of message level and message text

## Breaking changes
* Merges `getMechanismViewFromDATIM` into `getMechanismView`

## New features
* Tightens checks of `tool` and `cop_year` against one another. Adds `datapackrSupports`, `supportedCOPYears`, & `supportedTools`.

## Minor improvements and fixes
* Refactors the following functions to utilize `datimutils` instead of `datapackr` API functions:
   * `getMechanismView`
   * `map_COCs_to_COs`
* Refactors `getDataSetUids`, `writePSNUxIM`, and `packPSNUxIM` to reduce cyclomatic complexity of both functions
* Switches `getDataSetUids` to use `cop_year` instead of `FY` in parameters
* Simplifies `pullFullCodeList`
* Increments `getCurrentCOPYear`
* Adjusts `paste_oxford` to accommodate length = 2
* Updates `checkInvalidIndicatorCodes` to utilize `pick_schema` instead of hard coded schema
* Refactors package to remove dependencies on `R6` (DP-694), `plyr` (DP-672), and `rlist` (DP-684)
* Clears a number of warnings and notes during build checks:
   * Adds missing documentation
   * Adds missing package declarations
   * Fixes incorrect argument `full.name` to `full.names` in `list.files` call in `extractWorkbook` function
   * Updates `datapackr.R` to add missing global variables
* Adds test for the following functions:
   * `canReadFile`
   * `checkDuplicateRows`
   * `checkInvalidIndicatorCodes`
   * `checkMechanisms`
   * `defuntDisaggs`
   * `fetchPrioritizationTable`
   * `getHTSModality`
   * `getOPUDataFromDATIM`
* Adds more tests for memo generation, including `memoStructure` and `prepareMemoData`
* Fixes `pullFullCodeList` test for FY2022
* Fixes broken test for `getCurrentCOPYear` incrementing
* Disables `play-spectrum test`. Also increments `cop_year` within this test
* Adds more files to `.gitignore` and `.Rbuildignore`
* Refactors `prepareMemoData` and `prepareMemoDataByPSNU` to avoid parallelization on CI environments
* Update Circle CI configuration to specify large resource class and add insights snapshot to `README.md`

# datapackr 5.2.1

## Bug fixes
* Fix to `packOPUDataPack` by reverting changes to parameter names in `exportPackr` calls that were accidentally made during release v5.2.0.

# datapackr 5.2.0

## Breaking Changes
* Removes the following content as part of the deprecation of COP20 OPU handling:
   * `R/packSNUxIM_2020.R`
   * `R/packSNUxIM_OPU.R`
   * `R/unPackOPU_PSNUxIM.R`
   * `data/cop20_data_pack_schema.rda`
   * `data/cop20_map_DataPack_DATIM_DEs_COCs.rda`
   * `data/cop20OPU_data_pack_schema.rda`
   * `data/data_pack_schema.rda`
   * `data/updated_indicator_codes.rda`
   * `data/valid_category_options.rda`
   * `data-raw/cop20_validation_rules.json`
   * `data-raw/COP20OPU_Data_Pack_generation_script.R`
   * `data-raw/COP20OPU_Data_Pack_processing_script.R`
   * `data-raw/COP20OPU_Data_Pack_validation_script.R`
   * `data-raw/cop21_datapack_indicator_code_updates.csv`
   * `data-raw/GetDataForGlobalFund.R`
   * `data-raw/TEST_COP20_OPU_Data_Pack_Template.xlsx`
   * `data-raw/update_cop20_datapack_template.R`
   * `data-raw/update_cop20OPU_datapack_schema.R`
   * `data-raw/update_cached_valid_COs.R`
   * `data-raw/valid_COCs_COs.rda`
   * `inst/extdata/COP20_Data_Pack_Template_Final.xlsx`
   * `inst/extdata/COP20_OPU_Data_Pack_Template.xlsx`
* Removes the following legacy content from COP19:
   * `data/data_pack_schema.rda`
   * `data-raw/checkTX_CURR.R`
   * `data-raw/produceConfigFile.R`
* Removes COP20 processing in the following functions:
   * `adorn_import_file`
   * `check_cop_year`
   * `check_tool`
   * `checkMechanisms`
   * `createAnalytics`
   * `createKeychainInfo`
   * `createTestDataset`
   * `compareData_OpuDatapackVsDatim`
   * `getCOPDataFromDATIM`
   * `getDataSetUids`
   * `getHTSModality`
   * `getMapDataPack_DATIM_DEs_COCs`
   * `getMemoIndicators`
   * `getOPUDataFromDATIM`
   * `headerRow`
   * `memoStructure`
   * `packDataPack`
   * `packForDATIM_OPU`
   * `pick_schema`
   * `pick_template_path`
   * `skip_tabs`
   * `unPackOPUDataPack`
   * `unPackSNUxIM`
   * `unPackSchema_datapack`
   * `writePSNUxIM`
* Removes the following functions that are no longer used anywhere in the package:
   * `adornMechanisms`
   * `adornPSNUs`
   * `deriveTargets`
   * `getCountries`
   * `getIMPATTLevels`
   * `getNumeratorDenominator`
   * `getTechArea`
   * `getValidCategoryOptions`
   * `getValidCOs`
   * `packForPAW`
   * `pull_COPindicators`
* Renames `.testInvalidIndicatorCodes` to `checkInvalidIndicatorCodes`

## Bug Fixes
* Fixes a bug in `updateExistingPrioritization` that was preventing tools from being unpacked or memos from being generated in the apps

## Minor improvements and fixes
* Changes default schema parameter from `data_pack_schema` to `pick_schema()` for the following functions:
   * `packDataPackSheet`
   * `packSheets`
   * `prepareSheetData`
* Removes `lazyeval` and `datapackcommons` as required packages as neither are used in the package anywhere


# datapackr 5.1.7

## Bug fixes
* Removes `PrEP_CT >= PrEP_NEW` validation rule from checks. (DP-552)
* Removes `DIAGNOSED_SUBNAT (N, SUBNAT, HIVStatus) TARGET >= DIAGNOSED_SUBNAT (N, SUBNAT, Age/Sex/HIV) TARGET + DIAGNOSED_SUBNAT (N, SUBNAT, HIV/Sex) TARGET` validation rule from checks. (DP-554)
* Restores "affected_rows" column to the altered formula tab in the validation rules output file. (DP-572)
* Fixes bug that prevented Technical Assistance (TA) only implementing mechanisms (IMs) from populating in the PSNUxIM tab. (DP-575)
* Fixes bug in `checkMechanisms`.
* Ensures that `default/unallocated` data is included in COP Approval Memo tables.
* Reverts changes to function `unPackSNUxIM` in `R/unPackSNUxIM.R` back to 5.1.5.

## New features
* Adds new utility function `formatSetStrings` to assist in the `checkFormulas` function. Formats a vector of numbers into a string of ranges.

## Minor improvements and fixes
* Cleans up code and adds documentation to functions in the packPSNUxIM` tree. (DP-473)
* Strengthens validation of PSNUs: (DP-509)
    * Creates new function parsePSNUs
    * Errors out invalid PSNU strings are detected
    * Provides a warning only if NO PSNUs are detected
    * Adds unit tests
* Replaces deprecated code in `swapColumns` function located in `R/utilities.R`: (DP-547)
    * Replaces `mutate_` with base R fix to remove deprecation warning when writing PSNUXIM tab.
    * Added tests for `swapColumns` in `tests/testthat/test-utilities.R`
* Update FY23 MER DOD Targets COC UID to "o71WtN5JrUu" in the following locations: (DP-576 and DP-577)
    * `R/utilities.R`
    * `tests/testthat/test-GetDatasetUids.R`
    * `data-raw/update_cop22_de_coc_co_map.R`
    * `data/cop22_map_DataPack_DATIM_DEs_COCs.rda`
    * `data/cop22_map_adorn_import_file.rda`
* Adding whole package to linting checks. Lints `data-raw` and `tests` folders now in addition to `R` folder. (DP-578)
* Aligns on `datastreams` parameter to consolidate parameter names across functions (DP-579)
    * Changes `datastream` parameter for `pullFullCodeList` to `datastreams`
    * Changes `streams` parameter for `getCOPDataFromDATIM` to `datastreams`
    * Changes parameters in all relevant locations
    * Moves definition of `datastreams` parameter to `datapackr_params`
    * Removes duplicative parameter definitions from `getCOPDataFromDATIM` and inherits them from `datapackr_params` instead
* Adds missing `d2_session` parameter to `getCountries` function in `R/utilities.R` (DP-580)
* Aligns on `output_folder` as parameter for all functions that export files (DP-581)
    * Changes `output_path` parameter in `exportPackr` to `output_folder`
    * Replaces parameter names for `exportPackr` in all relevant locations
    * Modifies `output_folder` definition in `datapackr_params` to remove outdated reference to defaulting to working directory.
    * Removes duplicative parameter definitions in functions that were touched and replaces with inheritance from `datapackr_params`
* Remove unused parameters (DP-582)
    * Removes `type` argument from `deriveTargets`.
    * Removes `country_names` argument from `getOPUDataFromDATIM`.
    * Removes duplicative parameter definitions from `getOPUDataFromDATIM` and inherits definitions from `datapackr_params`.


# datapackr 5.1.6

## Bug fixes
- Fixes mismatch between text and metadata on certain validation messages that caused some warnings to be highlighted red as if they were errors and caused some errors not to be highlighted red. (DP-523)
- Fixes missing PrEP_CT data on approval memo target tables. (DP-561)
- Fixes missing AGYW_PREV data on approval memo target tables. (DP-562)
- Fixes reference to memo template in `generateMemoTemplate`.

## New Features
- Adds a system environment variable `MAX_CORES` which will limit the number of cores to be used in parallel processing. This environment variable is detected by the new function `getMaxCores`.
- Adds parameter `append` to `writePSNUxIM`. This parameter is `TRUE` by default and will append new rows to the existing PSNUxIM tab. If `FALSE`, a new PSNUxIM tab will be output only containing the new rows needed in the validated tool.
- Adds parameter `include_default` to `getMechanismView` and `adorn_import_file` that will include the default mechanism to the dataframe that is returned.
- Adds new function `checkHasPSNUxIM` to detect whether a COP Data Pack has a PSNUxIM tab.

## Minor improvements and fixes
- Updates alterned formula warning from `checkFormulas` to clarify that users may edit formulas in Green columns without permission, but need to receive clearance from DUIT prior to editing formulas in Grey columns. (DP-241)
- Makes `checkColStructure` more robust.
- Adds back `stringi` dependency due to new use of `stringi::stri_replace_all`. This will be replaced with a `stringr` function in a later version to reduce dependencies.


# datapackr 5.1.5

## New Features
* Adds COP Approval Memo target table generation to the package and enables COP22 Approvale Memo target table generation support (DP-503, DP-528, DP-534, DP-536)
  * Creates the following new functions to aid in the fetching, cleaning, and formatting of data used in the various target tables of COP approval memos:
    - `evaluateIndicators`
    - `fetchPrioritizationTable`
    - `generateComparisonTable`
    - `getMemoIndicators`
    - `memoStructure`
    - `prepareExistingDataAnalytics`
    - `prepareMemoData`
    - `prepareMemoDataByAgency`
    - `prepareMemoDataByPSNU`
    - `prepareMemoDataByPartner`
    - `prepareMemoDataByPrio`
    - `prepareMemoMetadata`
    - `updateExistingPrioritization`
  * Creates the following new functions to aid in the creation and formatting of COP Approval Memo target tables:
    - `getMemoTemplate` selects whether to supply a watermarked "Draft Memo" Word document or a blank document for final memo tables.
    - `renderAgencyTable`, `renderPartnerTable`, and `renderPrioTable` each generate formatted tables for their respective sections of the memo.
    - `generateApprovalMemo` compiles all target tables together in a single document.
    - `default_memo_font`, `default_memo_style_header`, and `default_memo_style_para` contain information on standard memo formatting.
    - `zeros_to_dashes` is a utility function used in formatting memos appropriately.
  * Adds `cop_validation_rules` as new dataset object within package
    - Adds `data-raw` file to create and update `cop_validation_rules` object
  * Adds `draft_memo_template.docx` under `inst/extdata` folder for use in draft memo generation
  * Adds tests for new functions:
    - Adds `test-get-memo-indicators` to test `getMemoIndicators`
    - Adds `test-indicators` to test `evaluateIndicators`
    - Adds `test-memo-structure` to test `memoStructure`
* Adds additional non-memo related functionality from the `datapackr-app` into the package (DP-504)
  * Creates `checkMechanisms` and `checkPSNUData` to replace `validateMechanisms` and `validatePSNUData` from the `datapackr-app`, respectively. These functions are used in validating and analyzing Data Pack targets.
  * Adds `sane_name`, `source_user`, and `operating_unit` as new variables under `d$info` in keychain
    - Creates `getSaneName` and `getOUFromCountryUIDs` as new functions
    - Modifies `createKeychainInfo` to create new variables under `d$info`
    - Modifies `packDataPack` to create new variables under `d$info`
    - Adds `d2_session` argument to `createKeychainInfo`
    - Updates relevant tests to incorporate new elements of keychain
* Enables the ability to get COP22 data using `getCOPDataFromDATIM` (DP-536)

## Minor improvements and fixes
* Adds `parallel`, `gdtools`, `flextable`, and `officer` as suggested packages


# datapackr 5.1.4

## Bug Fixes

* Ignore duplicated Not PEPFAR columns in PSNUxIM
* Fixes for handling 50+ age bands in PSNUxIM packing and unpacking


# datapackr 5.1.3

## Bug Fixes
* Downgrades `openxslx` to version 4.2.3 to prevent error with PSNUxIM packing.
* Changes the HTS modality group set to `HTS Modality (USE ONLY for FY22 Results/FY23 Targets)`.
* Change `compareData_OpuDatapackVsDatim` to NOT compare AGYW data which are not part of OPU data packs.

## Breaking changes
* Changes arguments for `unPackSchema_datapack` and `validateSchema` to use standard argument names. Replaces `filepath` with `template_path` and replaces their usage in all relevant locations.

## Minor improvements and fixes
* Reexports magrittr `%<>%` function.
* Adds missing function `interactive_warning`.
* Removes function `getPass` since this code is commented out and functionality has been replaced by `datimutils`.
* Cleans up mismatched brackets in `data.R` file and allows for creation of documentation for all datasets.
* Updates `.Rbuildignore` to include all appropriate files and folder.
* Clears error with R dependency by downgrading required version to R 4.1.0.
* Updates package dependencies:
    - Adds dependency and remote for `datimutils`.
    - Adds dependency for `methods`.
    - Suggests `waldo` package.
    - Removes `scales` and `stringi` as dependencies as they are not used.
    - Moves `assertthat` from `Imports` to `Suggests` since it is only used in `data-raw` files.
* Moves the following variables from `data-raw` files to environment variables. Users can add these to an `.Rprofile` to streamline their workflow:
    - `SECRETS_FOLDER`
    - `OUTPUT_FOLDER`
    - `MODEL_DATA_PATH`
    - `SNUXIM_MODEL_DATA_PATH`

# datapackr 5.1.2

## Bug Fixes
* Corrects the % of EID by 12 months from 95% (same as by 2 months) to 100%.
* Updates comments and conditional formatting in the Cascade Tab over HTS_INDEX yield columns to align with COP Guidance (<10% as warning, rather than 20%).

## New Features
* For COP22 Data Packs, enables appending data to the bottom of the PSNUxIM tab when data is detected in the main tabs of the Data Pack, but not reflected in the PSNUxIM tab.

# datapackr 5.1.1

## Bug fixes
* This maintenance release fixes a bug with `unPackCountryUIDs` that was introduced in v5.0.1 due to linting. This bug preventing the processing of COP and OPU tools in `datapackr-app` as well as broke the `createAnalytics` function for regional tools.

# datapackr 5.1.0

## Breaking changes
* The function `packForDATIM_UndistributedMER` now takes in arguments for MER data and COP year instead of an entire `d` object and returns a table of undistributed MER data rather than return a `d` object with data nested under `d$data$UndistributedMER`.

## New features
* Updates COP22 Data Pack template and processing code.
    - Removes "Summary" tab from COP22 tools and processing code.
    - Includes "Not PEPFAR" column to PSNUxIM tab and adds support for processing this data.
      Drops dataf from this column before creating the analytics table and MER exports.
* Adds item to `d` object for unallocated IMs nested under `d$info$unallocatedIMs`.

## Minor improvements and fixes
* Additional handling for default Category Option Combos.
* Adds dataset UIDs for COP22 to the `getDatasetUids` function.
* Updates `getMapDataPack_DATIM_DEs_COCs` to include handling for COP22 map.
* Adds test for `getDatasetUids`
* Adds explicit comparison checks when updating schemas and Data Pack-DATIM mapping files using the `waldo` package.

# datapackr 5.0.3

## New features
* Initial launch of COP22 Data Pack processing!

## Breaking changes
* Now requires R version 4.1.1 or higher.

## Minor improvements and fixes
* Updated and improved documentation of datasets in `datapackr`.
* Improves handling of default `categoryOptionCombo`.
* Improves documentation of `packDataPackSheet`, `packSheets`, and `prepareSheetData`.

## Deprecated features
* `loginToDATIM` is retired in favor of the same function in `datimutils`. All
  instances of this function being invoked have been replaced appropriately.
    - The functions `DHISLogin`, `GetCredentialsFromConsole`, `LoadConfigFile`,
      and `ValidateConfig` were not exported and are now deprecated as well.
      They were previously only used by `loginToDATIM`.
* `isLoggedIn` is retired as it was only used in `getMechList` and
  `loginToDATIM`.


# datapackr 5.0.2

## Bug fixes
* Resolves a bug with `packOPUDataPack` where `createDataPack` was not
implemented correctly in version 5.0.1.
* Patches a bug with `getOPUDataFromDATIM` where `getCOPDataFromDATIM` returns
a dataframe where the default Category Option Combo UID is listed as `default`
rather than the appropriate DATIM UID. This will be removed in favor of a more
permanent solution in future updates.

## New features
* Significantly improves handling of parameter checks and standardizes their
  validation and defaults. Documentation for these checks is also added.
* Adds functionality for producing COP22 Beta Packs and test data.

## Breaking changes
* Removes `getDataPackSchema` in favor of consolidated `pick_schema`.

## Deprecated features
* `getDataPackSchema` has been deprecated in favor of `pick_schema` and has been
replaced in the two locations where it was previously used.

## Minor improvements and fixes
* Improves and updates tests related to parameter checks and schemas.
* Introduces many new small utilities functions such as `%missing%` and `%||%`.
* Improves automation of Data Pack Template/schema validation.


# datapackr 5.0.1

## New features

* `loadDataPack` is a new function that returns a Data Pack object conserving
  styles and formatting of the original Data Pack .xlsx file, as well as other
  metadata necessary for processing and analysing data in the Data Pack.
* `.testInvalidIndicatorCodes` was previously an internal function that is now
  documented and exported by the package. This function tests for invalid
  indicator codes in a `d` Datapackr object.
* `datapack_cogs` is a new dataset containing Datapack Category option groups
  (id and name) along with their individual category options (id and name) as a
  nested data frame.


## Breaking changes

* `createWorkbook` has been renamed `createDataPack` to deconflict with the
  openxlsx function `createWorkbook`. This function now returns a `d`
  datapackr object rather than an openxlsx workbook object.
* The `d2_session` argument has been removed from the following functions:
    - `check_params`
    - `createDataPack` (previously `createWorkbook`)
    - `unPackSchema_datapack`


## Minor improvements and fixes

* `unPackSchema_datapack` was modified in the following ways:
    - Now uses the `datapack_cogs` data set rather than making a query to DATIM.
    - Inherits parameters from `datapackr_params`.
* `writeHomeTab` was modified in the following ways:
    - The `wb` and `datapack_name` arguments default to `NULL`.
    - Checks and assigns parameters using the `check_params` function.
    - Lists country names on the Home tab in addition to Country UIDs.
* Minor corrections were made to Excel functions written by `packSNUxIM` that
  had been erroneously changed during previous linting.
* Internal changes were made to variable names and functions used inside the
  `check_params` function.
* A new file has been added to `data-raw` to generate the `datapack_cogs`
  data set.
* Documentation is now provided for the `cop20OPU_data_pack_schema` data set.
