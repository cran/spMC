# spMC News

## CHANGES IN spMC 0.3.15

* **BUG FIXES**

  * It was edited the file `NAMESPACE` for better loading S3 Methods
    on more recent versions of R.

  * Fixing the function declaration `SEXP isOmp();` to
    `SEXP isOmp(void);` within the file `src/spMC.c`.

  * Fixing unprotected variable `prob` in `src/spMC.c` within the
    functions `annealingSIM` and `geneticSIM`.

## CHANGES IN spMC 0.3.14

* **BUG FIXES**

  * It was removed the file `cleanup` from the root directory.

## CHANGES IN spMC 0.3.13

* **BUG FIXES**

  * Fixing the prototype of the `SEXP isOmp();` to
    `SEXP isOmp(void);` within the header file `src/spMC.h`.

  * The file `R/setCores.R` has been updated to allow the function
    `suppressPackageStartupMessages()` to suppress all startup
    messages.

  * The file `man/spMC-package.Rd` has an updated url to a digital
    archived book on the references.

  * Converted the file `NEWS` to `NEWS.md`.

## CHANGES IN spMC 0.3.12

* **BUG FIXES**

  * The URLs and images in the file `README.md` have been fixed.

  * The file `man/spMC-package.Rd` has been corrected at the `<img>`
    attribute `align` using `"float:right;"` instead.

## CHANGES IN spMC 0.3.11

* **NEW FEATURES**

  * the logical argument `entropy` is introduced in the function
    `sim()`, `sim_ck()`, `sim_ik()`, `sim_mcs()` and `sim_path()`.
    Entropy and standardized entropy are introduced to study the
    uncertainty of the predicted values.

  * the package logo `man/figures/logo.svg` was included.

  * it was added a `README.md` file.

  * the files `man/spMC-package.Rd`, `man/setCores.Rd`,
    `man/quench.Rd`, `man/sim.Rd`, `man/sim_ck.Rd`, and
    `man/sim_ik.Rd` have been updated.

  * it was removed a reference in `inst/CITATION`.

* **BUG FIXES**

  * fixed a minor bug in the function `embedTProbs` of the file
    `spMC.c` to avoid possible divisions by zeros.

## CHANGES IN spMC 0.3.10

* **BUG FIXES**

  * a more consistent use of `PROTECT` has been implemented for the
    function `bclm` in the file `spMC.c` (line 1552).

## CHANGES IN spMC 0.3.9

* **BUG FIXES**

  * the order of the macros `$(LAPACK_LIBS)` and `$(BLAS_LIBS)` was
    switched to be conformed with the new CRAN guidelines.

  * bibentry author fields in the `inst/CITATION` file are now
    following the standard `"Given_1 Family_1 and Given_2 Family_2"`.

## CHANGES IN spMC 0.3.8

* **BUG FIXES**

  * `sim_mcs()` returns more accurate probabilities, and reliable
    predictions.

  * `embed_MC()` returns the transition probabilities that are now
    consistent with the `tpfit()` estimation methodology. An
    additional attribute is added to deal with the counts.

## CHANGES IN spMC 0.3.7

* **BUG FIXES**

  * the argument `"contour"` in `image.pemt()` is now `TRUE` by
    default, and the contour plots are drawn according the
    rappresented probabilities.

## CHANGES IN spMC 0.3.6

* **BUG FIXES**

  * the example of the `tpfit_me()` function now provides consistent
    results.

## CHANGES IN spMC 0.3.5

* **BUG FIXES**

  * `mlen()` now returns more accurate results with `"avg"` method.

  * `"transCount"` consider all directions between couple of points.
  
  * the C code was fixed in order to be compatible with OpenMP 4.0.
  
  * the deprecated `which.lines()` function is now replaced by
    `which_lines()` in all the examples of the manual.

## CHANGES IN spMC 0.3.4

* **BUG FIXES**

  * an uninitialized variable was fixed in the C procedure
    `"transCount"` when dealing with OpenMP 3.1.

  * unused variabels were removed from the C procedure `"nsph2"`.
  
  * unused values were fixed in the C procedures `"nsph2"` and
    `"nsph"`.

## CHANGES IN spMC 0.3.3

* **NEW FEATURES**

  * `quench()` function is introduced to reproduce the optimizations
    algorithms, which are required for removing spatial artifacts
    from the simulations.

  * `transiogram()` function computes also the log-odds standard error
    which will be used to draw confidence intervals for transition
    probabilities.
  
  * `plot.transiogram()` can draw confidence intervals around
    empirical transition probabilities.

* **BUG FIXES**

  * `transiogram()` has increased accuracy on non-autotransition
    probabilities calculation.

  * `pemt()` has improved probabilities calculation for multiple
    directions.

* **DEPRECATED AND DEFUNCT**

  * the arguments `GA`, `optype` and `max.it` in functions `sim_ik()`
    and `sim_ck()` are defunct. Simulation optimization is performed
    by the `quench()` function.

## CHANGES IN spMC 0.3.2

* **NEW FEATURES**

  * `pers.multi_tpfit()` will produce perspective plots for
    theoretical multidimensional transiograms.

  * `perp.pemt()` will produce perspective plots for
    pseudo-empirical multidimensional transiograms.
  
  * `sim()`, `sim_ck()`, `sim_ik()`, `sim_mcs()` and `sim_path()` are
    now returning an object of class `"data.frame"` and `"spsim"`.
  
  * `tpfit()`, `tpfit_ml()`, `tpfit_ils()`, `tpfit_me()`,
    `multi_tpfit()`, `multi_tpfit_ml()`, `multi_tpfit_ils()`,
    `multi_tpfit_me()`, `mlen()` and `pemt()` allows for the choice
    among serveral mean lengths estimation method through the
    argument `"mle"`.
  
  * `transiogram()` can also compute empirical probabilities for
    reversible chains throught the argument `"reverse"`.

* **UTILITIES**

  * Other robust methods are implemented for computing more
    reliable mean lengths. The choice can be perfrome among
    the averaging method (`"avg"`), the maximum likelihood estimator
    (`"mlk"`), trimmed average (`"trm"`) or trimmed median (`"mdn"`).

* **DEPRECATED AND DEFUNCT**

  * the arguments `GA`, `optype` and `max.it` in functions `sim_ik()`
    and `sim_ck()` are deprecated. They will be considered defunct
    from next version release.

  * `tpfit(mle = FALSE)` is deprecated.

  * `tpfit_ml(mle = FALSE)` is deprecated.

  * `tpfit_ils(mle = FALSE)` is deprecated.

  * `tpfit_me(mle = FALSE)` is deprecated.

  * `multi_tpfit(mle = FALSE)` is deprecated.

  * `multi_tpfit_ml(mle = FALSE)` is deprecated.

  * `multi_tpfit_ils(mle = FALSE)` is deprecated.

  * `multi_tpfit_me(mle = FALSE)` is deprecated.

  * `mlen(mle = FALSE)` is deprecated.

  * `pemt(mle = FALSE)` is deprecated.

* **BUG FIXES**

  * The polar transformation performed by the C procedure `"nsph"`
    was not much accurate, and computationally inefficient.

  * Memory bugs were fixed for C procedures `"getCKPrbs"`,
    `"getIKPrbs"`, `"nsph"` and `"transCount"`.
  
  * colors correspondence between transiograms and legend was fixed
    in functions `image.multi_tpfit()` and `image.pemt()`.

## CHANGES IN spMC 0.3.1

* **NEW FEATURES**

  * `tpfit_ml()`, `tpfit_ils()`, `tpfit_me()`, `multi_tpfit_ml()`,
    `multi_tpfit_ils()`, `multi_tpfit_me()` and other functions are
    introduced for R-methods consistency (see list of deprecated
    and defunct functions).

  * `multi_tpfit()` and `tpfit()` may perform all the other estimation
    methods by the use of the new argument method.
  
  * `sim()` may simulate and predict with all the available methods.

* **UTILITIES**

  * `contour.pemt()` display contours of the pseudo-empirical
    multidimensional and superpose the contour lines of the
    theoretical transition probabilities.

  * `image.pemt()` plot the pseudo-empirical multidimensional
    transiograms, which where previously computed.
  
  * `is.pemt()` tests an object if it is of the class `'pemt'`.
  
  * `pemt()` returns pseudo-empirical multidimensional transiograms.

* **DEPRECATED AND DEFUNCT**

  * `ck.sim()` is defunct. Use `sim_ck()` instead.

  * `embed.MC()` is defunct. Use `embed_MC()` instead.

  * `ik.sim()` is defunct. Use `sim_ik()` instead.

  * `ilstpfit()` and `multi.ilstpfit()` are defunct.

    Iterated least squares are available by the use of the functions
    `tpfit_ils()` or `multi_tpfit_ils()`.
  
  * `image.multi.tpfit()` is defunct. Use `image.multi_tpfit()`
    instead.
  
  * `imgMultiTransiogram()` is defunct.
  
    Pseudo-empirical multidimensional transiograms are now computed
    first by the function `pemt()` and then plotted through
    `image.pemt()`.
  
  * `is.multi.tpfit()` is defunct. Use `is.multi_tpfit()` instead.
  
  * `is.multi.transiogram()` is defunct. Use `is.multi_transiogram()`
    instead.
  
  * `mcs.sim()` is defunct. Use `sim_mcs()` instead.
  
  * `metpfit` and `multi.metpfit()` are defunct.
  
    Maximum entropy methods is available by the use of the functions
    `tpfit_me()` or `multi_tpfit_me()`.
  
  * `multi.tpfit()` is defunct.
  
    Multidimensional version of `tpfit()` is available by the use of
    the function `multi_tpfit()`.
  
  * `path.sim()` is defunct. Use `sim_path()` instead.
  
  * `predict.multi.tpfit()` is defunct. Use `predict.multi_tpfit()`
    instead.
  
  * `print.multi.tpfit()` is defunct. Use `print.multi_tpfit()`
    instead.
  
  * `print.multi.transiogram()` is defunct. Use
    `print.multi_transiogram()` instead.
  
  * `which.lines()` is defunct. Use `which_lines()`.
