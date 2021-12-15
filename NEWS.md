# datapackr 5.0.2

## New features

* Initial launch of COP22 Data Pack processing!

## Breaking changes


## Minor improvements and fixes



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
