Assignment 2: Time Series Regression
================
Joshua Goldberg
April, 17 2019

# Data Description

``` r
stock_data <- read_csv("Assign 2 TS regression.csv") %>% 
  mutate(date = dmy(date)) %>% 
  gather(key = stock, value = return, -1)

stock_data_ts <- stock_data %>%
  as_tsibble(key = id(stock), index = date)
```

All are daily stock exchange returns.

ISE: Istanbul stock exchange national 100 index

SP: Standard & Poor™s 500 return index

DAX: Stock market return index of Germany

FTSE: Stock market return index of UK

NIKKEI: Stock market return index of Japan

BOVESPA: Stock market return index of Brazil

# Questions

Determine if all the TS are stationary:

1.  qualitatively: the data for each stock all look stationary. \(\mu\)
    and \(\sigma^2\) remain constant overtime. Oscillations are offset
    by each other.

<!-- end list -->

``` r
stock_data_ts %>% 
  ggplot(aes(date, return, color = stock)) +
  geom_line() +
  scale_x_date(date_breaks = "6 month", date_minor_breaks = "3 month", date_labels = "%m-%y") +
  scale_color_viridis_d(name = "Stock") +
  facet_wrap( ~ stock) +
  labs(x = "Date",
       y = "Return",
       caption = "date format: MM/YY")
```

![](Figs/unnamed-chunk-2-1.png)<!-- -->

2.  quantitatively: use **ADF** and **KPSS** from package tseries.

<!-- end list -->

``` r
(stationary_tests <- stock_data_ts %>% 
  nest(-stock) %>% 
  mutate(adf_test = map(data, ~ suppressWarnings(adf.test(.x$return))),
         kpss_test = map(data, ~ suppressWarnings(kpss.test(.x$return))),
         adf_p_value = map_df(adf_test, ~ glance(.x)) %>% pull(p.value),
         kpss_p_value = map_df(kpss_test, ~ glance(.x)) %>% pull(p.value)))
```

    ## # A tibble: 6 x 6
    ##   stock   data              adf_test    kpss_test  adf_p_value kpss_p_value
    ##   <chr>   <list>            <list>      <list>           <dbl>        <dbl>
    ## 1 BOVESPA <tsibble [536 × … <S3: htest> <S3: htes…        0.01          0.1
    ## 2 DAX     <tsibble [536 × … <S3: htest> <S3: htes…        0.01          0.1
    ## 3 FTSE    <tsibble [536 × … <S3: htest> <S3: htes…        0.01          0.1
    ## 4 ISE     <tsibble [536 × … <S3: htest> <S3: htes…        0.01          0.1
    ## 5 NIKKEI  <tsibble [536 × … <S3: htest> <S3: htes…        0.01          0.1
    ## 6 SP      <tsibble [536 × … <S3: htest> <S3: htes…        0.01          0.1

``` r
stationary_tests %>% 
  gather(key = key, value = value, -c(1:4)) %>% 
  ggplot(aes(stock, value, fill = key)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = .05, linetype = 2) +
  annotate("text", -Inf, .0575, label = "Null Rejection Threshold", hjust = 0, vjust = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(name = "Hypothesis Test", labels = c("ADF", "KPSS")) +
  labs(title = "Determining stationarity with ADF and KPSS",
       x = "Stock",
       y = "P-Value") +
  theme(legend.position = "top")
```

![](Figs/unnamed-chunk-3-1.png)<!-- -->

2.  Split the data into train and test, keeping only the last 10 rows
    for test (from date 9-Feb-11). Remember to use only train dataset.

<!-- end list -->

``` r
model_data <- stock_data_ts %>% 
  spread(stock, return)

train <- model_data %>% 
  filter(date < "2011-02-09")

test <- model_data %>% 
  anti_join(train, "date")
```

3.  Linearly regress ISE against the remaining 5 stock index returns.
    Determine which coefficients are equal or better than 0.02 (\*)
    level of significance.

<!-- end list -->

``` r
lm_model <- lm(ISE ~ BOVESPA + DAX + FTSE + NIKKEI + SP, data = train)
summary(lm_model)
```

    ## 
    ## Call:
    ## lm(formula = ISE ~ BOVESPA + DAX + FTSE + NIKKEI + SP, data = train)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.071180 -0.009248  0.000083  0.009304  0.051863 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.0008833  0.0006640   1.330 0.183979    
    ## BOVESPA      0.1117630  0.0626647   1.784 0.075087 .  
    ## DAX          0.3417440  0.0961243   3.555 0.000412 ***
    ## FTSE         0.6033493  0.1077621   5.599 3.50e-08 ***
    ## NIKKEI       0.3266529  0.0462163   7.068 5.09e-12 ***
    ## SP          -0.0607521  0.0770823  -0.788 0.430970    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0152 on 520 degrees of freedom
    ## Multiple R-squared:  0.493,  Adjusted R-squared:  0.4881 
    ## F-statistic: 101.1 on 5 and 520 DF,  p-value: < 2.2e-16

``` r
signif_vars <- lm_model %>% 
  tidy() %>% 
  slice(-1) %>% 
  filter(p.value < .02) %>% pull(term)
```

Significant variables: DAX, FTSE, NIKKEI.
