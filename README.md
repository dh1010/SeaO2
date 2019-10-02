# SeaO2
GUI for calculating seawater carbonate parameters, based on the r seacarb package. 

## Getting Started

This repository contains two python programs for lab control.

Autotitrator.py is an autotitrator program that is designed for carrying out alkalinity titrations on seawater samples. The associated R data analysis file Titr.Calc.rmd uses the gran method (https://water.usgs.gov/owq/FieldManual/Chapter6/section6.6/html/section6.6.4.htm) to determine alkalinity for each sample.

pH_Stat.py is a chemostat program that adjusts the rate of a syringe pump to maintain a constant pH within a reactor vessel. The associated R data analysis file Rate_Cale.rmd is designed to use the volume of titrants added to calculate the rate of precipitation within the vessel. This requires a surface area (initially set to 0.205 m2/g).

### Prerequisites

The app requires the following packages:
```
install.packages("tidyverse")
install.packages("seacarb")
install.packages("shiny")
install.packages('DT')
```
## Built With

* [RStudio](https://rstudio.com/products/rstudio/download/#download) - RStudio for Windows R version 3.6.1

## Authors

* **David Hodkin**

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* The Seacarb package was used for calculating water compositions 
