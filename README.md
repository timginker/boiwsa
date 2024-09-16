
<!-- README.md is generated from README.Rmd. Please edit that file -->

# boiwsa

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/boiwsa?color=green)](https://cran.r-project.org/package=boiwsa)
[![](https://img.shields.io/github/last-commit/timginker/boiwsa.svg)](https://github.com/timginker/boiwsa/commits/master)
[![](http://cranlogs.r-pkg.org/badges/grand-total/boiwsa)](https://cran.r-project.org/package=boiwsa)
<!-- badges: end -->

`boiwsa` is an R package for seasonal adjustment of weekly data. It
provides a simple, easy-to-use interface for calculating the seasonally
adjusted estimates, as well as a number of diagnostic tools for
evaluating the quality of the adjustments.

The seasonal adjustment procedure approach aligns closely with the
locally-weighted least squares procedure introduced by Cleveland et
al. (2014) albeit with several adjustments. Firstly, instead of relying
on differencing to detrend the data, we opt for a more explicit approach
by directly estimating the trend through smoothing techniques. Secondly,
we incorporate a variation of Discount weighted regression (Harrison and
Johnston, 1984) to enable the seasonal component to evolve dynamically
over time.

We consider the following decomposition model of the observed series
$y_{t}$:

$$
y_{t}=T_{t}+S_{t}+H_{t}+O_{t}+I_{t},
$$

where $T_{t}$, $S_{t}$, $H_{t}$, $O_{t}$ and $I_{t}$ represent the
trend, seasonal, outlier, holiday- and trading-day, and irregular
components, respectively. To evade ambiguity, it is important to note
that in weekly data, $t$ typically denotes the date of the last day
within a given week. The seasonal component is specified using
trigonometric variables as:

``` math
\begin{eqnarray*}
S_{t} &=&\sum_{k=1}^{K}\left( \alpha _{k}^{y}\sin (\frac{2\pi kD_{t}^{y}}{
n_{t}^{y}})+\beta _{k}^{y}\cos (\frac{2\pi kD_{t}^{y}}{n_{t}^{y}})\right) +
\\
&&\sum_{l=1}^{L}\left( \alpha _{l}^{m}\sin (\frac{2\pi lD_{t}^{m}}{n_{t}^{m}}
)+\beta _{l}^{m}\cos (\frac{2\pi lD_{t}^{m}}{n_{t}^{m}})\right) ,
\end{eqnarray*}
```

where $D_{t}^{y}$ and $D_{t}^{m}$ are the day of the year and the day of
the month, and $n_{t}^{y}$ and $n_{t}^{m}$ are the number of days in the
given year or month. Thus, the seasonal adjustment procedure takes into
account the existence of two cycles, namely intrayearly and
intramonthly.

Like the X-11 method (Ladiray and Quenneville, 2001), the `boiwsa`
procedure uses an iterative principle to estimate the various
components. The seasonal adjustment algorithm comprises eight steps,
which are documented below:

- Step 1: Estimation of trend ($T_{t}^{(1)}$) using `stats::supsmu()`.

- Step 2: Estimation of the Seasonal-Irregular component:

$$y_{t}-T_{t}^{(1)}=S_{t}+H_{t}+O_{t}+I_{t}$$

- Step 2\*: Searching for additive outliers using the method proposed by
  Finfley et al. (1998)

- Step 2\*\*: Identifying the optimal number of trigonometric variables

- Step 3: Calculation of seasonal factors, along with other potential
  factors such as $H_{t}$ or $O_{t}$, is achieved through discount
  weighted regression on the seasonal-irregular component extracted in
  Step 2. In this application, the discounting rate decays over the
  years. For each year $t$ and the observed year $\tau$, a geometrically
  decaying weight function is represented as: $w_{t}=r^{|t-\tau|}$,
  where $r \in (0,1]$. This approach differs from the customary one-way
  discounting, allowing us to incorporate future observations in
  computing seasonal factors and thus avoid the limitations of the
  forecasting methods mentioned in Bandara et al. (2021). In addition,
  in traditional discount-weighted regression, even with a conservative
  choice of $r=0.95$, in weekly data, observations separated by more
  than $2$ years would carry nearly negligible weight. Thus, the use of
  year-based discounting prevents an overly rapid decay which may
  potentially lead to unstable estimates of the seasonal component.

- Step 4: Estimation of trend ($T_{t}^{(2)}$) from seasonally and
  outlier adjusted series using `stats::supsmu()` function (R Core Team,
  2013)

- Step 5: Estimation of the Seasonal-Irregular component:
  $$y_{t}-T_{t}^{(2)}=S_{t}+H_{t}+O_{t}+I_{t}$$

- Step 6: Computing the final seasonal factors (and possibly other
  factors such as $H_{t}$ or $O_{t}$) using discount weighted
  regression, as in step 3.

- Step 7: Estimation of the final seasonally adjusted series:
  $$y_{t}-S_{t}-H_{t}$$

- Step 8: Computing final trend ($T_{t}^{(3)}$) estimate from seasonally
  and outlier adjusted series using `stats::supsmu()`.

## Installation

To install boiwsa, you can use devtools:

``` r
# install.packages("devtools")
devtools::install_github("timginker/boiwsa")
```

Alternatively, you can clone the repository and install the package from
source:

``` bash
git clone https://github.com/timginker/boiwsa.git
cd boiwsa
R CMD INSTALL .
```

## Usage

Using `boiwsa` is simple. First, load the `boiwsa` package:

``` r
library(boiwsa)
```

Next, load your time series data into a data frame object. Here is an
example that is based on the `gasoline` data from the US Energy
Information Administration that we copied from the from the `fpp2`
package:

``` r
data("gasoline.data")
plot(gasoline.data$date,
     gasoline.data$y,
     type="l"
     ,xlab="Year",
     ylab=" ",
     main="Weekly US gasoline production")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="0.5" height="0.5" />

Once you have your data loaded, you can use the `boiwsa` function to
perform weekly seasonal adjustment:

``` r
res=boiwsa(x=gasoline.data$y,dates=gasoline.data$date)
```

The `x` argument takes the series to be seasonally adjusted, while the
dates argument takes the associated dates in date format. Unless
specified otherwise (i.e., `my.k_l = NULL`), the procedure automatically
identifies the best number of trigonometric variables to capture the
yearly ($K$) and monthly ($L$) cycles based on the AICc. The information
criterion is specified by the `ic` option. The weighting decay rate is
specified by `r` (by default `r=0.8`).

The procedure automatically searches for additive outliers (AO) using
the method described in Appendix C of Findley et al. (1998). To disable
the automatic AO search, set `auto.ao.search = F`. To add user-defined
AOs, use the `ao.list` option.

The `boiwsa` function returns a list object containing the results. The
seasonally adjusted series is stored in a vector called `sa`. In
addition, the estimated seasonal factors are stored as `sf`.

You can then plot the adjusted data to visualize the seasonal pattern:

``` r
plot(res)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="0.5" />

To evaluate the quality of the adjustment, you can use the `plot_spec`
function provided by the package, which generates a plot of the
autoregressive spectrum of the raw and seasonally adjusted data:

``` r
plot_spec(res)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="0.3" />

# References

Bandara, K., Hyndman, R. J. and C. Bergmeir (2021). MSTL: A
seasonal-trend decomposition algorithm for time series with multiple
seasonal patterns. arXiv preprint arXiv:2107.13462 .

Cleveland, W.P., Evans, T.D. and S. Scott (2014). Weekly Seasonal
Adjustment-A Locally-weighted Regression Approach (No. 473). Bureau of
Labor Statistics.

Findley, D.F., Monsell, B.C., Bell, W.R., Otto, M.C. and B.C Chen
(1998). New capabilities and methods of the X-12-ARIMA
seasonal-adjustment program. Journal of Business & Economic Statistics,
16(2), pp.127-152.

Harrison, P. J. and F. R. Johnston (1984). Discount weighted regression.
Journal of the Operational Research Society 35(10), 923–932.

Hyndman, R. (2023). fpp2: Data for “Forecasting: Principles and
Practice” (2nd Edition). R package version 2.5.

Ladiray, D. and B. Quenneville (2001). Seasonal adjustment with the X-11
method.

R Core Team (2013). R: A Language and Environment for Statistical
Computing. Vienna, Austria: R Foundation for Statistical Computing. ISBN
3-900051-07-0.

# Disclaimer

The views expressed here are solely of the author and do not necessarily
represent the views of the Bank of Israel.

Please note that `boiwsa` is still under development and may contain
bugs or other issues that have not yet been resolved. While we have made
every effort to ensure that the package is functional and reliable, we
cannot guarantee its performance in all situations.

We strongly advise that you regularly check for updates and install any
new versions that become available, as these may contain important bug
fixes and other improvements. By using this package, you acknowledge and
accept that it is provided on an “as is” basis, and that we make no
warranties or representations regarding its suitability for your
specific needs or purposes.
