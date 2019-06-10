# SoilHealthDB

A database for global soil health assessment

## General information

Here, we compiled data from a set of soil health measurements collected across 41 countries around the world, named SoilHealthDB, which includes 5,241 data entries from 281 published studies. The SoilHealthDB includes 42 soil health indicators and 45 background indicators that describe factors such as climate, elevation, and soil type. A primary goal of SoilHealthDB is to enable the research community to perform comprehensive analyses, e.g., meta-analyses, of soil health changes related to cropland conservation management. The database also provides a common framework for sharing soil health, and the scientific research community is encouraged to contribute their own measurements. 
R code for quality checking and data processing are available at **RScripts**.
All data (including meta data for clerification) were stored in 'SoilHealthDB_V1.xlsx'.

### `DESCRIPTION.txt`, which includes 15 seets:
* Sheet1: Include all digitized data from 281 papers
* ReadMe: Descriptions about SoilHealthDB
* Explanation: Description about each column in Sheet 1
* ExperimentID: Descrption of how Experiment ID of each pairwise comparison were assigned 
* Units convertion: Meta information about unit convertion
* Rs units convertion: Soil respiration unit convertion
* SoilClassficaton: Different soil classification system in the world
* SoilDepth: Diagram shows how soil sampling depth were grouped into 0-10cm, 0-20cm, 0-30cm, and >30cm
* CoverCropGroup: Describe how cover crop grouped 
* GrainCropGroup: Describe how grain crop grouped 
* TillageGroup: Describe how tillage management grouped
* ConservationType: Describe the type of conservation in the soil health database
* SD_SE: Calculate SD based on SE, CV, or 95%CI
* Reference: Historical papers collected in this database, the StudyIDs corresponding to the same ID from sheet1
* Notes: Other notes about SoilHealthDB

## Acknowledgements

This work is supported by the Natural Resources Conservation Service, U.S. Department of Agriculture, under NRCS Conservation Innovation Grant 69-3A75-14-260. Funding for this work was provided in part by the Virginia Agricultural Experiment Station and the Hatch Program of the National Institute of Food and Agriculture, U.S. Department of Agriculture. Jinshi Jian was partially supported by the U.S. Department of Energy, Office of Science, Biological and Environmental Research as part of the Terrestrial Ecosystem Sciences Program. Those items can be used for individual, academic, research, and commercial usage, but cannot be repackaged or sold without written permission.

## Contributions

Jinshi Jian and Ryan D. Stewart conceived the design of the data framework. Jinshi Jian and Xuan Du extracted and integrated the data from papers to the SoilHealthDB. 

## Contact information

If you have any questions, please contact Jinshi Jian: jinshi@vt.edu or Ryan Stewart: ryan.stewart@vt.edu

