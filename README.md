
<!-- README.md is generated from README.Rmd. Please edit that file -->

# boiwsa

<!-- badges: start -->
<!-- badges: end -->

boiwsa is a package for seasonal adjustment of weekly data. It is
implemented using a locally-weighted least squares procedure (Cleveland
et al., 2014). We consider the following decomposition model:

$$
y_{t}=T_{t}+S_{t}+H_{t}+O_{t}+I_{t},
$$ where $T_{t}$, $S_{t}$ , $O_{t},$ $H_{t}$ and $I_{t}$ represent the
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

You can install the development version of boiwsa from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("timginker/boiwsa")
```

## Example

TBA

``` r
library(boiwsa)
data("gasoline.data")

plot(gasoline.data$date,gasoline.data$y,type="l",xlab="Year",ylab=" ", main="Weekly US gasoline production")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## Performing seasonal adjustment using defaults.

``` r
res=boiwsa(x=gasoline.data$y,dates=gasoline.data$date)
```

## Inspecting the results:

Number of trigonometric terms chosen by the automatic procedure

``` r
res$my.k_l
#>   yearly variables monthly variables
#> 1               12                 0
```

## Plotting

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

## Inspecting the spectrum

``` r
plot_spec(res)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

# Refrences

Cleveland, W.P., Evans, T.D. and S. Scott (2014). Weekly Seasonal
Adjustment-A Locally-weighted Regression Approach (No. 473). Bureau of
Labor Statistics.
