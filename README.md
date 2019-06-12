# SoilHealthDB

A database for global soil health assessment

## General information

Here, we compiled data from a set of soil health measurements collected across 41 countries around the world, named SoilHealthDB, which includes 5,241 data entries from 281 published studies. The SoilHealthDB includes 42 soil health indicators and 45 background indicators that describe factors such as climate, elevation, and soil type. A primary goal of SoilHealthDB is to enable the research community to perform comprehensive analyses, e.g., meta-analyses, of soil health changes related to cropland conservation management. The database also provides a common framework for sharing soil health, and the scientific research community is encouraged to contribute their own measurements. 
R code for quality checking and data processing are available at **RScripts**.
All data (including meta data for clerification) were stored in 'SoilHealthDB_V1.xlsx'.

## `SoilHealthDB_V1.xlsx`, which includes 15 sheets:
* Sheet1: Include all compiled data inputs from 281 articles (see detail in the Reference sheet)
* ReadMe: This file
* Explanation: Description of each column in Sheet 1
* ExperimentID: Descrption of how Experiment ID of each pairwise comparison were assigned 
* UnitsConvertion: Meta information about unit convertion
* RsUnitsConvertion: Soil respiration unit convertion
* SoilClassficaton: Different soil classification system in the world
* SoilDepth: Diagram shows how soil sampling depth were grouped into 0-10cm, 0-20cm, 0-30cm, and >30cm
* CoverCropGroup: Describe how cover crop grouped 
* GrainCropGroup: Describe how grain crop grouped 
* TillageGroup: Describe how tillage management grouped
* ConservationType: Describe the type of conservation in the soil health database
* SD_SE: Calculate SD based on SE, CV, or 95%CI
* Reference: 281 articles compiled in this database, the StudyIDs corresponding to the same ID from sheet1
* Notes: Other notes about SoilHealthDB

## Acknowledgements

This work is supported by the Natural Resources Conservation Service, U.S. Department of Agriculture, under NRCS Conservation Innovation Grant 69-3A75-14-260. Funding for this work was provided in part by the Virginia Agricultural Experiment Station and the Hatch Program of the National Institute of Food and Agriculture, U.S. Department of Agriculture. Jinshi Jian was partially supported by the U.S. Department of Energy, Office of Science, Biological and Environmental Research as part of the Terrestrial Ecosystem Sciences Program. 

## Contributions

Jinshi Jian and Ryan D. Stewart conceived the design of the data framework. Jinshi Jian and Xuan Du extracted and integrated the data from papers to the SoilHealthDB. 

## How to contribute

We appreciate if you can contribute, please:
1) For those 281 papers (details in the **Reference** data sheet), you can adding (new inputs if we missing some from the papers) and correcting (if there are some mistake inputs)
2) For those not included in those 281 papers, you can dirrectly add new rows following the same data framework
3) For all above changes, please open an issue or a pull request in Github (preferred) or send us an email (see below contact information)

## Contact information

If you have any questions, please contact Jinshi Jian: jinshi@vt.edu or Ryan Stewart: ryan.stewart@vt.edu

## Usage and citation

Those items can be used for individual, academic, research, and commercial usage, but cannot be repackaged or sold without written permission. If you used this data, please cite this Github (https://github.com/jinshijian/SoilHealthDB); a published article: Stewart, Ryan D., et al. "What we talk about when we talk about soil health." Agricultural & Environmental Letters 3.1 (2018); and an upcoming article submitted to *Scientific Data* (details will be provided in the future).

