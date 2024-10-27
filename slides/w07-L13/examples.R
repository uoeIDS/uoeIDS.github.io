## inspired by https://stat545.com/
library(ggplot2)
library(dplyr)
library(tidyr)
library(gapminder)

head(gapminder)

## say we want to compute the range of some numeric vector
## range = difference between max and min
## note: in R range(x) returns c(max(x),min(x))

gapminder %>% group_by(continent) %>%
  summarize(range_len = max(lifeExp)-min(lifeExp))

gapminder %>% group_by(continent,year) %>%
  summarize(range_len = max(lifeExp)-min(lifeExp))

gapminder %>% group_by(continent) %>%
  summarize(range_len = max(pop)-min(pop))

## instead of copy pasing, let's define our function

max_minus_min <- function(x){
  return(max(x)-min(x))
}

max_minus_min(1:10)

gapminder %>% group_by(continent) %>%
  summarize(range_len = max_minus_min(lifeExp))

max_minus_min(c(TRUE,FALSE,FALSE,TRUE))

## hey, that should have given an error!

max_minus_min2 <- function(x) {
  if(!is.numeric(x)) {
    stop('Please provide a numeric input.\n',
         'You have provided an object of class: ', class(x)[1])
  }
  return(max(x) - min(x))
}

max_minus_min2(c(TRUE,FALSE,FALSE,TRUE))
max_minus_min2(gapminder)
max_minus_min2(letters)

max_minus_min(c(1:10,NA))
max_minus_min2(c(1:10,NA))

max_minus_min3 <- function(x, na.rm = TRUE) {
  if(!is.numeric(x)) {
    stop('Please provide a numeric input.\n',
         'You have provided an object of class: ', class(x)[1])
  }
  return(max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
}

max_minus_min3(c(1:10,NA), na.rm = TRUE)
max_minus_min3(c(1:10,NA))

## if we are happy with this function, let's rename it with a better name:

max_minus_min <- max_minus_min3
rm("max_minus_min3", "max_minus_min2")


my_gapminder <- gapminder %>% group_by(country) %>%
  summarize(range_lifeExp = max_minus_min(lifeExp),
         median_lifeExp = median(lifeExp))

ggplot(my_gapminder) + geom_point(aes(median_lifeExp, range_lifeExp))

range_by_median <- function(variable){
  gapminder %>% group_by(country) %>%
    summarize(range = max_minus_min({{ variable }}),
           med = median({{ variable }})
           ) %>% ggplot() + 
    geom_point(aes(med, range))
}

range_by_median(lifeExp)
range_by_median(pop)
range_by_median(gdpPercap)


 







## extra: generalize for other quantiles
## (did you know that the max is the 100% quantile, and the min is the 0% quantile?)

max_minus_min3b <- function(x, na.rm = TRUE) {
  if(!is.numeric(x)) {
    stop('Please provide a numeric input.\n',
         'You have provided an object of class: ', class(x)[1])
  }
  q_max <- quantile(x, probs = 1, na.rm = na.rm)
  q_min <- quantile(x, probs = 0, na.rm = na.rm)
  return(unname(q_max-q_min))
}

max_minus_min3b(c(1:10,NA))

quantile_diff <- function(x, prob = 1, na.rm = TRUE) {
  if(!is.numeric(x)) {
    stop('Please provide a numeric input.\n',
         'You have provided an object of class: ', class(x)[1])
  }
  if(!is.numeric(prob) | length(prob)>1){
    stop('prob needs to be a numeric of length 1.')
  }
  if(prob < 0 | prob > 1){
    stop('prob needs to be a number between 0 and 1.')
  }
  
  min_prob <- min(prob,1-prob)
  max_prob <- 1-min_prob
  
  q_min <- quantile(x, probs = min_prob, na.rm = na.rm)
  q_max <- quantile(x, probs = max_prob, na.rm = na.rm)
  return(unname(q_max-q_min))
}

quantile_diff(c(1:10,NA))
quantile_diff(c(1:10,NA), prob = 0.3)




gapminder %>% pivot_longer(cols = c("lifeExp","pop","gdpPercap"),
                           names_to = "variable",
                           values_to = "vals") %>%
  mutate(range_len = )
  ggplot(mapping=aes(x = vals,fill = variable)) + 
  geom_density(position = "identity", alpha = 0.5)

my_gapminder <- gapminder %>% mutate(lifeExp = scale(lifeExp),
                                     pop = scale(pop),
                                     gdpPercap = scale(gdpPercap))

  
my_gapminder %>% pivot_longer(cols = c("lifeExp","pop","gdpPercap"),
                           names_to = "variable",
                           values_to = "vals") %>%
  ggplot(mapping=aes(x = vals,fill = variable)) + 
  geom_density(position = "identity", alpha = 0.5)


## debugging and traceback

f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) {
  if (!is.numeric(d)) {
    stop("`d` must be numeric", call. = FALSE)
  }
  d + 10
}

f("a")


g <- function(b) {
  browser()
  h(b)
}
f(10)
