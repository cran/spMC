# spMC: Continuous-Lag Spatial Markov Chains

### Authors <img src="man/figures/logo.svg" align="right" alt="spMC logo" />
[Luca Sartore](mailto://drwolf85@gmail.com)[<img alt="ORCID iD" src="https://cran.r-project.org/web/orcid.svg" width="16px" height="16px" style="width:16px; height:16px; margin-left:4px; margin-right:4px; vertical-align:middle">](https://orcid.org/0000-0002-0446-1328) 

Maintainer: [Luca Sartore](mailto://drwolf85@gmail.com)

[![](https://www.r-pkg.org/badges/version/spMC)](https://CRAN.R-project.org/package=spMC)
[![GPLv2 license](https://img.shields.io/badge/License-GPLv2-yellow.svg)](https://perso.crans.org/besson/LICENSE.html)
[![DOI](https://zenodo.org/badge/doi/10.32614/RJ-2013-022.svg)](https://dx.doi.org/10.32614/rj-2013-022)
[![DOI](https://zenodo.org/badge/doi/10.1016/j.cageo.2016.06.001.svg)](https://dx.doi.org/10.1016/j.cageo.2016.06.001)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/spMC)](https://www.r-pkg.org/pkg/spMC)
[![Total Downloads from CRAN RStudio mirror](https://cranlogs.r-pkg.org/badges/grand-total/spMC?color=orange)](https://CRAN.R-project.org/package=spMC)


## Features of the package
The main goal of the **spMC** package is to provide a set of functions for
  1. the stratum lengths analysis along a chosen direction,
  2. fast estimation of continuous lag spatial Markov chains model parameters and probability computing (also for large data sets), 
  3. transition probability maps and transiograms drawing, 
  4. simulation methods for categorical random fields.

Several functions are available for the stratum lengths analysis, in particular they compute the stratum lengths for each stratum category, they compute the empirical distributions and many other tools for a graphical analysis.

Usually, the basic inputs for the most of the functions are a vector of categorical data and their location coordinates. They are used to estimate empirical transition probabilities (`transiogram`), to estimate model parameters (`tpfit` for one-dimensional Markov chains or `multi_tpfit` for multidimensional Markov chains). Once parameters are estimated, it's possible to compute theoretical transition probabilities by the use of the function `predict.tpfit` for one-dimensional Markov chains and `predict.multi_tpfit` for multidimensional ones.

The function `plot.transiogram` allows to plot one-dimensional transiograms, while `image.multi_tpfit` permit to draw transition probability maps. A powerful tool to explore graphically the anisotropy of such process is given by the functions `pemt` and `image.pemt`, which let the user to draw "quasi-empirical" transition probability maps.

Simulation methods are based on Indicator Kriging (`sim_ik`), Indicator Cokriging (`sim_ck`), Fixed or Random Path algorithms (`sim_path`) and Multinomial Categorical Simulation technique (`sim_mcs`).

For a complete list of exported functions, use `library(help = "spMC")` once the **spMC** package is installed.

## References

Allard, D., D'Or, D., Froidevaux, R. (2011) An efficient maximum entropy approach for categorical variable prediction. *European Journal of Soil Science*, **62**(3), 381-393.

Carle, S. F., Fogg, G. E. (1997) Modelling Spatial Variability with One and Multidimensional Continuous-Lag Markov Chains. *Mathematical Geology*, **29**(7), 891-918.

Dynkin, E. B. (1961) *Theory of Markov Processes*. Englewood Cliffs, N.J.: Prentice-Hall, Inc.

Higham, N. J. (2008) *Functions of Matrices: Theory and Computation*. Society for Industrial and Applied Mathematics.

Li, W. (2007) A Fixed-Path Markov Chain Algorithm for Conditional Simulation of Discrete Spatial Variables. *Mathematical Geology*, **39**(2), 159-176.

Li, W. (2007) Markov Chain Random Fields for Estimation of Categorical Variables. *Mathematical Geology*, **39**(June), 321-335.

Li, W. (2007) Transiograms for Characterizing Spatial Variability of Soil Classes. *Soil Science Society of America Journal*, **71**(3), 881-893.

Pickard, D. K. (1980) Unilateral Markov Fields. *Advances in Applied Probability*, **12**(3), 655-671.

Sartore, L. (2010) Geostatistical models for 3-D data. M.Phil. thesis, Ca' Foscari University of Venice.

Sartore, L. (2013). spMC: Modelling Spatial Random Fields with Continuous Lag Markov Chains. *The R Journal*, **5**(2), 16-28.

Sartore, L., Fabbri, P. and Gaetan, C. (2016). spMC: an R-package for 3D lithological reconstructions based on spatial Markov chains. *Computers & Geosciences*, **94**(September), 40-47.

Weise, T. (2009) *Global Optimization Algorithms - Theory and Application*. [[Archived copy]](https://archive.org/details/Thomas_Weise__Global_Optimization_Algorithms_Theory_and_Application/page/n57/mode/2up).

