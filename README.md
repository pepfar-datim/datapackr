# datapackr: A package for packing and unpacking Data Packs.

### Installation

1. Download R from http://cran.us.r-project.org/
2. Install R. Leave all default settings in the installation options.
3. Download RStudio from http://rstudio.org/download/desktop and install it. Leave all default settings in the installation options.
4. Open RStudio
5. Go to the “Packages” tab and click on “Install Packages”. The first time you do this you’ll be prompted to choose a CRAN mirror. R will download all necessary files from the server you select here. Choose the location closest to you.
6. Open a new R Script in RStudio.
7. Copy and paste the following into that new file:

```R
install.packages("devtools")
library(devtools)
install_github(repo = "https://github.com/pepfar-datim/datapackr.git", ref = "master")
library(datapackr)
```

8. Hit the `Run` button.
9. If this presents issues, contact the development team either through a [GitHub ticket](https://github.com/pepfar-datim/datapackr/issues/new), or via [DATIM Support](https://datim.zendesk.com) (DATIM users only).
10. If the package loads without issue, copy, paste, and run the following code in RStudio:

```R

d <- unPackData()

```

11. When prompted, select the location of the file you would like to check. This must be saved as an XLSX file.
12. Once automated reviews are completed, you can see any warnings that were generated in the RStudio Console.



Have a question? Find us on [GitHub](https://github.com/pepfar-datim/datapackr/issues/new) or [DATIM Support](https://datim.zendesk.com) (DATIM users only).
