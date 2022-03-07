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
