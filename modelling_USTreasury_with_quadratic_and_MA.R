# load packages
library(dlm)
library(dplyr)
library(lubridate)
library(plotly)


# load data
df <- read.csv('full-daily-treasury-rates-20182023.csv')
df$Date <- as.Date(as.character(df$Date))

y10 <- df$X10.Yr


# time plot
year_begin <- c()
for (i in 2019:2023){
    year_begin <- append(year_begin, which(year(df$Date)==i)[1])
}

plot(y10, type='l')
for (i in 1:5) abline(v=year_begin[i])

#weekdays(as.Date("2023-08-18"))


# helper functions
smape <- function(forecast, actual){
    num <- abs(forecast-actual)
    den <- (abs(actual) + abs(forecast))/2
    (100/length(actual)) * sum(num/den)
}

myplot <- function(mod_filter){
    ## only works for models with more than one variable
    
    plot_ly(df, x = ~Date) %>%
        add_markers(y = ~X10.Yr, name = 'data',
                    marker = list(color = 'gray')) %>%
        add_lines(y = mod_filter$m[-1,1], name = 'filtered level',
                  line = list(color = 'orange', width = 2)) %>%
        add_lines(y = mod_filter$f, name = 'one-step ahead forecast',
                  line = list(width = 2))
}

plot_resids <- function(arr){
    par(mfrow=c(2,1), mar = c(2,5,4,2))
    acf(arr, main = '', ylab = 'ACF', xlab = '')
    pacf(arr, main = '', ylab = 'PACF')
}


# polynomial model
mod_poly <- dlmModPoly(3, m0 = c(2.5, 0, 0), dV = 5, dW = c(10, 0, 1))
mod_poly.f <- dlmFilter(y10, mod_poly)

myplot(mod_poly.f)
mean(abs(y10-mod_poly.f$f)); smape(mod_poly.f$f, y10)

res_poly <- residuals(mod_poly.f, sd = F)
acf(res_poly)
shapiro.test(res_poly)
qqnorm(res_poly); qqline(res_poly)


# polynomial and MA
macoef <- arima(res_poly, order = c(0, 0, 5), include.mean = F)$coef
mod_poly_arma <- mod_poly + dlmModARMA(ma = macoef)
mod_poly_arma.f <- dlmFilter(y10, mod_poly_arma)
myplot(mod_poly_arma.f)
mean(abs(y10-mod_poly_arma.f$f)); smape(mod_poly_arma.f$f, y10)

res_poly_arma <- residuals(mod_poly_arma.f, sd = F)
acf(res_poly_arma)
shapiro.test(res_poly_arma)
qqnorm(res_poly_arma); qqline(res_poly_arma)


# polynomial, MA, seasonal
mod_poly_arma_seas <- mod_poly_arma + dlmModSeas(3, dW = c(10, 1))
mod_poly_arma_seas.f <- dlmFilter(y10, mod_poly_arma_seas)

myplot(mod_poly_arma_seas.f)
mean(abs(y10-mod_poly_arma_seas.f$f)); smape(mod_poly_arma_seas.f$f, y10)

res_poly_arma_seas <- residuals(mod_poly_arma_seas.f, sd = F)
plot_resids(res_poly_arma_seas)
acf(res_poly_arma_seas)
shapiro.test(res_poly_arma_seas)
qqnorm(res_poly_arma_seas); qqline(res_poly_arma_seas)


# model fitting comparisons
plot_ly(df, x = ~Date) %>%
    add_markers(y = ~X10.Yr, name = 'data', marker = list(color = 'gray')) %>%
    add_lines(y = mod_poly.f$f, name = 'polynomial') %>%
    add_lines(y = mod_poly_arma.f$f, name = 'polynomial and MA') %>%
    add_lines(y = mod_poly_arma_seas.f$f, name = 'polynomial, MA, seasonal')


# forecasts
dlmForecast(mod_poly.f, 9)
dlmForecast(mod_poly_arma.f, 9)
dlmForecast(mod_poly_arma_seas.f, 9)$f
