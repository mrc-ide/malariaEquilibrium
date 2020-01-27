
<!-- README.md is generated from README.Rmd. Please edit that file -->
malariaEquilibrium
==================

[![Travis build status](https://travis-ci.org/mrc-ide/malariaEquilibrium.svg?branch=master)](https://travis-ci.org/mrc-ide/malariaEquilibrium) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mrc-ide/malariaEquilibrium?branch=master&svg=true)](https://ci.appveyor.com/project/mrc-ide/malariaEquilibrium)

Often we are interested in the state of a malaria transmission model at equibrium. However, some models (e.g. the Griffin et al. 2014 model) are quite complex, and can result in equilibrium solutions that are fairly in-depth. In these situations it is useful to have a "canonical" equilibrium solution that is tried and tested, and can be used reliably by multiple users. This package aims to be a place for hosting these canonical solutions, and for storing useful tests, checks and plotting functions for exploring model predictions at equilibrium.

How to interact with this repos
-------------------------------

If you have a new model with an equilibrium solution that you would like to host here, then please go ahead and do so *in a new branch*. The master branch, on the other hand, should always correspond to the current most active model used by the malaria group (currently the Griffin et al. 2014 model). This should generally be left alone unless you know what you're doing.

Any changes that you propose to branches that you did not create yourself (e.g. the master branch) should be done through pull requests, rather than editing directly. This gives others the opportunity to review your changes before they are implemented in code.

NB. These stipulations are in place because there are other R packages that test against the equilibrium solutions from this package, and so any changes here may cause other packages to fail if they are not know about beforehand.

Installation
------------

You can install from the master branch of malariaEquilibrium with:

``` r
# install.packages("devtools")
devtools::install_github("mrc-ide/malariaEquilibrium")
```

Or from a specific branch with:

``` r
# install.packages("devtools")
devtools::install_github("mrc-ide/malariaEquilibrium", ref = "mybranch")
```

Finally, don't forget to load the package with:

``` r
library(malariaEquilibrium)
```

Example
-------

### Working with parameters

We start by loading a set of model parameters with the `load_parameter_set()` function. This loads an object from the *inst/extdata* folder of the package. Alternatively you can load your own parameter set from file.

``` r
myparams <- load_parameter_set("Jamie_parameters.rds")
```

Parameters are of class `model_params`, which is essentially just an ordinary R list with a few custom methods. For example, when we print we see the following:

``` r
myparams
#>              value
#> eta   0.0001304631
#> rho           0.85
#> a0            2920
#> s2            1.67
#> rA           0.005
#> rT             0.2
#> rD             0.2
#> rU     0.009090909
#> rP            0.04
#> dE              12
#> tl            12.5
#> cD           0.068
#> cT         0.02176
#> cU          0.0062
#> g_inf         1.82
#> d1           0.161
#> dd            3650
#> ID0           1.58
#> kd           0.477
#> ud            9.45
#> ad0         7993.5
#> fd0         0.0071
#> gd            4.81
#> aA           0.757
#> aU           0.186
#> b0            0.59
#> b1             0.5
#> db            3650
#> IB0           43.9
#> kb            2.16
#> ub             7.2
#> phi0         0.792
#> phi1       0.00074
#> dc           10950
#> IC0             18
#> kc            2.37
#> uc            6.06
#> PM           0.774
#> dm            67.7
#> tau             10
#> mu           0.132
#> f        0.3333333
#> Q0            0.92
#> cd_w         0.723
#> cd_p         0.342
```

We can modify parameter values exactly as we would with a named list:

``` r
myparams$eta <- 0.1
head(myparams)
#>     value
#> eta   0.1
#> rho  0.85
#> a0   2920
#> s2   1.67
#> rA  0.005
#> rT    0.2
```

### Getting the equilibrium solution

Now we can calculate the equilibrium solution using the `human_equilibrium()` function. This assumes a fixed EIR and treatment rate (`ft`), and takes model parameters as input. It calculates the equilibrium solution for a defined age range:

``` r
eq <- human_equilibrium(EIR = 10, ft = 0.2, p = myparams, age = 0:10)
```

The output elements are "states" and "FOIM". The former is a matrix with age brackets in rows, and model states or other downstream calculations in columns. For example, the column "S" gives the equilibrium proportion of humans in the susceptible state, while the column "pos\_M" gives the proportion of hosts positive by microscopy in this bracket.

``` r
head(eq$states)
#>      age            S            T            D            A            U
#> [1,]   0 9.467880e-01 1.563207e-03 6.252830e-03 1.588967e-02 6.493094e-04
#> [2,]   1 2.438063e-02 7.314807e-05 2.925923e-04 1.026347e-03 5.530062e-05
#> [3,]   2 6.251534e-04 2.618414e-06 1.047366e-05 4.558052e-05 3.000507e-06
#> [4,]   3 1.599499e-05 8.441349e-08 3.376539e-07 1.716239e-06 1.320333e-07
#> [5,]   4 4.089291e-07 2.574924e-09 1.029970e-08 5.887601e-08 5.139869e-09
#> [6,]   5 1.045661e-08 7.592526e-11 3.037010e-10 1.903552e-09 1.846483e-10
#>                 P          inf         prop       psi        pos_M      pos_PCR
#> [1,] 2.190291e-03 1.541308e-03 9.733333e-01 0.2014989 2.368415e-02 2.433854e-02
#> [2,] 1.275394e-04 9.139654e-05 2.595556e-02 0.2953252 1.390077e-03 1.445847e-03
#> [3,] 5.321638e-06 3.874773e-06 6.921481e-04 0.3781267 5.856141e-05 6.158762e-05
#> [4,] 1.919563e-07 1.417641e-07 1.845728e-05 0.4511988 2.133356e-06 2.266525e-06
#> [5,] 6.374654e-09 4.769418e-09 4.921942e-07 0.5156846 7.155090e-08 7.673621e-08
#> [6,] 2.007472e-10 1.520110e-10 1.312518e-08 0.5725932 2.275346e-09 2.461764e-09
#>               inc
#> [1,] 2.366225e-03
#> [2,] 8.931033e-05
#> [3,] 2.961462e-06
#> [4,] 9.190789e-08
#> [5,] 2.741310e-09
#> [6,] 7.965503e-11
```

The "FOIM" element gives the onward force of infection from humans to mosquitoes, which is a single number:

``` r
eq$FOIM
#> [1] 0.002690066
```
