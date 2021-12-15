## Quantifying impacts of the COVID-19 pandemic on Australian life expectancy

### Introduction
------------

This GitHub repository contains the replication code and data to accompany the manuscript [*Quantifying impacts of the COVID-19 pandemic on Australian life expectancy*](), published in the *International Journal of Epidemiology*.

### Prerequisites
------------

To run the code locally you will need a working installation of [**R**](https://www.r-project.org/), including the necessary dependencies.

Databases from the Australian Bureau of Statistics (ABS) are used in the analysis and included in two input data sets:

* _POP2020b.csv_ includes the registered deaths and Estimated Resident Population (ERP) by age, sex and Australian state/territory of residence for years 2010 to 2020. The data is presented exactly as downloaded from the [ABS tabulator](https://explore.data.abs.gov.au/).
* _CoD17to20.csv_ includes the distribution of deaths by age and total causes of death, as well as the leading causes of death by age. Data is arranged with males on the top rows and females in the bottom and going from 2017 to 2020. The leading causes of death by age included corresponds to those causes which matched in all years for each age, and it is based on the ABS data ([https://www.abs.gov.au/statistics/health/causes-death/causes-death-australia](https://www.abs.gov.au/statistics/health/causes-death/causes-death-australia)). For this analysis 5 causes of death were selected: neoplasms, cardiovascular diseases or CVD, respiratory diseases, external causes, COVID-19 and a group of other causes referring to causes not included in the previous 5 groups. COVID-19 mortality for 2020 was available by age-group. However, for other causes of death some estimation was required which is explained in Appendix 3 accompanying the article and included as part of the R code. 

Life tables by single age and year for Australian females and males from the Human Mortality Database (HMD: [www.mortality.org](www.mortality.org)) are included among the data used _fltper_1x1.txt_ and _mltper_1x1.txt_. However, we recommend users downloading the most recent information directly from this data source. 

For the comparisons with Denmark and the United States, source data from Aburto et al. (2021) will need to be downloaded separately. You can download this file [here](https://github.com/OxfordDemSci/ex2020/blob/master/out/lt_input.rds) and place it in the _data_ folder.

### Structure
----------------

* _r_ contains R code.
* _data_ contains the data for Australia.
* _output_ contains outputs (i.e., figures) produced from the R code.

### Acknowledgements

The R code and resulting figures partly re-use [publicly available code and data](https://github.com/OxfordDemSci/ex2020) used by *Aburto et al. (2021)*, with some modifications.

Aburto, J. M., Schöley, J., Kashnitsky, I., Zhang, L., Rahal, C., Missov, T. I., Mills, M. C., Dowd, J. B., & Kashyap, R. (2021). Code to replicate 'Quantifying impacts of the COVID-19 pandemic through life expectancy losses' (Version 0.3.1) [Computer software]. https://doi.org/10.5281/zenodo.5171938