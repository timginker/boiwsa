
<!-- README.md is generated from README.Rmd. Please edit that file -->

# boiwsa

<!-- badges: start -->
<!-- badges: end -->

boiwsa is an R package for performing weekly seasonal adjustment on time
series data. It provides a simple, easy-to-use interface for calculating
seasonally adjusted estimates of weekly data, as well as a number of
diagnostic tools for evaluating the quality of the adjustments.

The seasonal adjustment procedure is based on a locally-weighted least
squares procedure (Cleveland et al., 2014).

We consider the following decomposition model:

$$
y_{t}=T_{t}+S_{t}+H_{t}+O_{t}+I_{t},
$$

where $T_{t}$, $S_{t}$ , $O_{t},$ $H_{t}$ and $I_{t}$ represent the
trend, seasonal, outlier, holiday- and trading-day, and irregular
components, respectively. The seasonal component is modeled as

$$\begin{eqnarray*}
S_{t} &=&\sum_{k=1}^{K}\left( \alpha _{k}^{y}\sin (\frac{2\pi kD_{t}^{y}}{
n_{t}^{y}})+\beta _{k}^{y}\cos (\frac{2\pi kD_{t}^{y}}{n_{t}^{y}})\right) +
\\
&&\sum_{l=1}^{L}\left( \alpha _{l}^{m}\sin (\frac{2\pi kD_{t}^{m}}{n_{t}^{m}}
)+\beta _{l}^{m}\cos (\frac{2\pi kD_{t}^{m}}{n_{t}^{m}})\right) ,
\end{eqnarray*}$$

where $D_{t}^{y}$ and $D_{t}^{m}$ are the day of the year and the day of
the month, and $n_{t}^{y}$ and $n_{t}^{m}$ are the number of days in the
given month or year.

The trend component is extracted with Friedman’s SuperSmoother using
`stats::supsmu()`.

## Installation

To install boiwsa, you can use devtools:

``` r
# install.packages("devtools")
devtools::install_github("timginker/boiwsa")
```

Alternatively, you can clone the repository and install the package from
source:

``` bash
git clone https://github.com/yourusername/boiwsa.git
cd boiwsa
R CMD INSTALL .
```

## Usage

Using `boiwsa` is simple. First, load the `boiwsa` package:

``` r
library(boiwsa)
```

Next, load your time series data into a data frame object. Here is an
example that is based on the `gasoline` data from the `fpp2` package:

``` r
data("gasoline.data")

plot(gasoline.data$date,gasoline.data$y,type="l",xlab="Year",ylab=" ", main="Weekly US gasoline production")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

Once you have your data loaded, you can use the `boiwsa` function to
perform weekly seasonal adjustment:

``` r
res=boiwsa(x=gasoline.data$y,dates=gasoline.data$date)
```

The `x` argument takes series to seasonally adjust, and the `dates`
argument takes the associated dates in a date format.

The `boiwsa` function returns a list object containing the results. The
seasonally adjusted series is stored in a vector called `sa`. In
addition, the estimated seasonal factors are stored as `sf`.

You can then plot the adjusted data to visualize the seasonal pattern:

``` r
plot(gasoline.data$date,gasoline.data$y,type="l",xlab="Year",ylab=" ", main="Weekly US gasoline production")
lines(gasoline.data$date,res$sa,col="red")
legend(
  "topleft",
  legend = c("Original", "SA"),
  lwd = c(2,2),
  col = c("black", "red"),
  bty = "n"
)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

To evaluate the quality of the adjustment, you can use the `plot_spec`
function provided by the package, which generates a plot of the
autoregressive spectrum of the raw and seasonally adjusted data:

``` r
plot_spec(res)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

# Refrences

Cleveland, W.P., Evans, T.D. and S. Scott (2014). Weekly Seasonal
Adjustment-A Locally-weighted Regression Approach (No. 473). Bureau of
Labor Statistics.
