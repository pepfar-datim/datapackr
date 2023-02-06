# datapackr: A package for packing and unpacking Data Packs.

**Repo Owner:** Scott Jackson [@jacksonsj](https://github.com/jacksonsj)

### Installation

1. Go to https://cran.rstudio.com/ to download and install R. Leave all default settings in the installation options.
2. Go to https://dailies.rstudio.com/version/2022.07.2+576.pro12/ to download and install R Studio. Leave all default settings in the installation options.
3. Open RStudio on your computer.
4. Go to the “Packages” tab and click on “Install Packages”. The first time you do this you’ll be prompted to choose a CRAN mirror. R will download all necessary files from the server you select here. Choose the location closest to you.
5. Type in "devtools". Ensure that "Install dependencies" is checked, and click "Install". If you get permission errors while installing packages, close RStudio and reopen it with administrator privileges.
6. Open a new R Script in RStudio.
7. Copy and paste the following into that new file:

```R
library(devtools)
install_github(repo = "https://github.com/pepfar-datim/datapackr.git", ref = "master")
library(datapackr)
```

8. Hit the `Run` button.
9. If you are prompted in the Console to select which packages to update, just hit Enter to bypass.
10. If this presents issues, contact the development team via [DATIM Support](https://datim.zendesk.com) (DATIM users only).
11. If the package loads without issue, restart your R session.
12. Copy, paste, and run the following code in RStudio — make sure to insert your actual DATIM username and password.:

```R
loginToDATIM(username = "MyUsername", password = "MyPassword123", base_url = "datim.org/")

d <- unPackTool()

```

13. When prompted, select the location of the file you would like to check. This must be saved as an XLSX file.
14. Once automated reviews are completed, you can see any warnings that were generated in the RStudio Console.



Have a question? Find us on [DATIM Support](https://datim.zendesk.com) (DATIM users only).

[![CircleCI](https://circleci.com/gh/pepfar-datim/datapackr.svg?style=shield)](https://app.circleci.com/insights/github/pepfar-datim/datapackr/)
