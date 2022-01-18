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
