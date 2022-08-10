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
