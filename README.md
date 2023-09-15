23-08070-000: Westport Golf Links Discipline Report
================
Regina Lionheart
2023-09-15

------------------------------------------------------------------------

## ![](readme_figures/Herrera_lockup_4c.png)

## Table of Contents

- [Project Description](#Project-Description)
- [Location in Herrera Database](#Location-in-Herrera-Database)
- [Requirements and Dependencies](#Requirements-and-Dependencies)
- [Installation and Usage](#Installation-and-Usage)
  - [Layout of directory and data](#Layout-of-directory-and-data)
  - [Detailed description of data and
    analysis](#Detailed-description-of-data-and-analysis)
  - [Metadata](#Metadata)
- [Pull Requests](#Pull-Requests)
- [Contributors and Contact
  Information](#Contributors-and-Contact-Information)

------------------------------------------------------------------------

# Project 23-08070-000 - Task 002

### Westport Golf Links LLC: Discipline Report

**SharePoint Site:**
[SharePoint](https://herrerainc.sharepoint.com/teams/23-08056-000)

------------------------------------------------------------------------

## Project Description

This repository supports the Discipline Report, prepared for Westport
Golf Links LLC by Herrera Environmental Consultants (located in the
Documents folder of the SharePoint site, linked above). The report
covers the coastal history of the Westport region, as well as predicted
erosion in the coming 25 years. The dataset referenced in this
repository is specifically for the two sets of transects used to inform
erosion predictions: The Department of Ecology data (described below)
and the transects placed on satellite imagery to track historical
erosion and predict future erosion (details located in the
Westport_Report.qmd file).

### Repo Description

This analysis aims to characterize approximately 25 years of shoreline
profile transect data, provided by NANOOS (Northwest Association of
Networked Ocean Observing Systems).

This repository focuses specifically on Westport Light State Park,
profiles 16 and 17 drawn from raw data consisting of approximately 4500
individual profiles across 54 coastal sites in Washington and Oregon.

The profiles are in x y z format (Easting Northing elevation).The naming
convention is prof_X_ssYY.out where prof is short for profile, X is the
profile number, ss is a season code (e.g. f = fall) and YY is the year,
in the format of the last two digits (e.g. “98” is 1998”, “00” is 2000,
“08” is 2008, etc).

## :droplet: Location in Herrera Database

The original, unmodified data used in this project is locally located in
*data_raw* That folder is backed up to
[SharePoint](https://herrerainc.sharepoint.com/teams/21-07771-002-InternalDocs/Shared%20Documents/Forms/AllItems.aspx?FolderCTID=0x0120008C8FFE1D422C224DA4CA697C7E7BF5DF&id=%2Fteams%2F21%2D07771%2D002%2DInternalDocs%2FShared%20Documents%2FInternal%20Docs%2FProject%2DFiles%2FTask%203%20Synthesis%20Shoreline%2FShorelineSynthesis%5Fdata%5Fraw&viewid=91d991bd%2De61d%2D4a39%2Dae3a%2Dac3ba8123673)

\*\* Please note that this raw data used in this project is a subset of
a larger data set from a previous project, and the complete dataset is
located in the linked SharePoint folder \*\*

If you do not have access to the data, please contact the emails listed
at the bottom of the repository.

## 📦 Requirements and Dependencies

Below is a list of packages and external softwares that this project
utilizes.

| Name                              | Description                                 |
|:----------------------------------|:--------------------------------------------|
| [`R`](https://www.r-project.org/) | Programming language used for this project. |

## :computer: Installation and Usage

In order to run this script and recreate the analysis, you will need to
have R and Rstudio installed on your computer. All the data produced by
this analysis can be found in the data_secondary/ folder, while all
figures can be found in the figures/ directory.

### :arrows_counterclockwise: Layout of directory and data

This repository is organized into a main Complete.Rmd markdown script,
which produces the results from scratch when run in its entirety. The
Complete script references folders of raw data (data_raw/), and produces
results data that has been modified or created by the analysis
(data_secondary/). All analysis scripts are contained in the scripts/
directory, which also contains the src/ sub directory. The src/ sub
directory contains package loading scripts and scripts that produce or
modify data used throughout the analysis.

The data_raw/ folder is **READ ONLY** and should never be modified or
deleted.

The Complete.Rmd script primarily handles the data from the Department
of Ecology, while the Westport_Report.qmd handles those transects that
were placed using satellite imagery.

### :heavy_check_mark: Detailed description of data and analysis

The raw data consists of Department of Ecology transects, filtered only
to include those that are relevant to Westport. These are delimited
files in x y z format.

The profiles are in .out raw format, and can be tidied and imported to
dataframes using the scripts found in Complete.Rmd.

Because these profiles are a subset of a larger set of profiles used for
another project, the README of that project has the detailed analysis
description. Please see the Shoreline Change Analysis
[GitHub](https://github.com/HerreraEnvironmental/21-07771-003_ShorelineChangeAnalysis)
for a complete rundown of the analysis steps. These steps are also
described in the comments on the Complete.Rmd script included in this
repository.

### :information_source: Metadata

The vertical datum is NAVD88. The horizontal is WA State Plane South.
All of the units are **originally** in meters but have been translated
to feet for this project.

------------------------------------------------------------------------

## 🔧 Pull Requests

Pull requests are welcome. For major changes, please open an issue
first.

All functioning code is located on the main branch. Dev branches are to
be named <specific_issue_description>\_dev.

## 💬 Contributors + Contact Information

- [Regina Lionheart](https://github.com/R-Lionheart)
- [Jeff Parsons](https://www.herrerainc.com/team-member/jeff-parsons/)
