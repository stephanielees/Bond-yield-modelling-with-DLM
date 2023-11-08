# LOAD PACKAGES
library(dlm)
library(dplyr)
library(lubridate)
library(plotly)


# LOAD DATA
df <- read.csv('full-daily-treasury-rates-20182023.csv')
df$Date <- as.Date(as.character(df$Date))

#taking out the 10-year bond yield rate
y10 <- df$X10.Yr


# TIME PLOT
#getting the index of the beginning of each year
year_begin <- c()
for (i in 2019:2023){
    year_begin <- append(year_begin, which(year(df$Date)==i)[1])
}

#making the time plot
plot(y10, type='l')
for (i in 1:5) abline(v=year_begin[i])

weekdays(as.Date("2023-08-18"))

# POLYNOMIAL MODEL ONLY
mod_poly <- dlmModPoly(3, m0 = c(2.5, 0, 0), dV = 5, dW = c(10, 0, 1))
eigen(mod_poly$GG)
mod_poly.f <- dlmFilter(y10, mod_poly)
res_poly <- residuals(mod_poly.f, sd = F)
pred_poly <- dlmForecast(mod_poly.f, 9)
```

# POLYNOMIAL AND MA COMPONENTS
macoef <- arima(res_poly, order = c(0, 0, 5), include.mean = F)$coef
mod_poly_ma <- mod_poly + dlmModARMA(ma = macoef)
mod_poly_ma.f <- dlmFilter(y10, mod_poly_ma)
pred_poly_ma <- dlmForecast(mod_poly_ma.f, 9)
pred_poly_ma$Q
```

# POLYNOMIAL, MA, AND SEASONAL
mod_poly_ma_seas <- mod_poly_ma + dlmModSeas(3, dW = c(10, 1))
mod_poly_ma_seas.f <- dlmFilter(y10, mod_poly_ma_seas)
pred_poly_ma_seas <- dlmForecast(mod_poly_ma_seas.f, 9)
```

# MA ONLY
mod_ma <- dlmModARMA(ma = macoef)
eigen(mod_ma$GG)
mod_ma.f <- dlmFilter(y10, mod_ma)
pred_ma <- dlmForecast(mod_ma.f, 9)
pred_ma$f
```

# SEASONAL COMPONENT ONLY
mod_seas <- dlmModSeas(3, dW = c(10, 1))
eigen(mod_seas$GG)
mod_seas.f <- dlmFilter(y10, mod_seas)
pred_seas <- dlmForecast(mod_seas.f, 9)


# COMPARING PREDICTIONS SIDE BY SIDE
preds <- data.frame('poly' = pred_poly$f,
                    'ma' = pred_ma$f,
                    'seas' = pred_seas$f,
                    'poly_ma' = pred_poly_ma$f,
                    'poly_ma_seas' = pred_poly_ma_seas$f)


# VISUALIZING THE FORECASTS
#x-axis
days_ahead <- seq(as.Date("2023-08-21"), as.Date("2023-08-31"), by = 'days')
remove_ind <- which(weekdays(days_ahead) %in% (c("Saturday", "Sunday")))
days_ahead <- days_ahead[-remove_ind]

#the real data
real_vals <- c(4.34, 4.34, 4.19, 4.23, 4.25, 4.2, 4.12, 4.12, 4.09)

#plot
plot_ly(preds, x = days_ahead) %>%
    add_trace(y = ~poly, name = 'poly', 
              type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~poly_ma, name = 'poly_ma', 
              type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~poly_ma_seas, name = 'poly_ma_seas', 
              type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = real_vals, name = 'real data', 
              type = 'scatter', mode = 'lines+markers') %>%
    layout(yaxis = list(title = 'forecasted yield rate (%)'))
