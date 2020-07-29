## ---- include=FALSE, echo=FALSE-----------------------------------------------
library(knitr)
opts_chunk$set(fig.align = 'center', 
               fig.show = 'hold', fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE)

## -----------------------------------------------------------------------------
library(gginnards)
library(tibble)

## -----------------------------------------------------------------------------
set.seed(4321)
# generate artificial data
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
my.data <- data.frame(x, 
                      y, 
                      group = c("A", "B"), 
                      y2 = y * c(0.5, 2),
                      block = c("a", "a", "b", "b"))

## -----------------------------------------------------------------------------
old_theme <- theme_set(theme_bw())

## -----------------------------------------------------------------------------
class(ggplot())

## -----------------------------------------------------------------------------
p0 <- ggplot()
p0

## -----------------------------------------------------------------------------
str(p0)

## -----------------------------------------------------------------------------
p1 <- ggplot(data = my.data, aes(x, y, colour = group))
str(p1)

## -----------------------------------------------------------------------------
str(p1, max.level = 2, components = "data")

## -----------------------------------------------------------------------------
p2 <- p1 + geom_point()
str(p2)

## -----------------------------------------------------------------------------
summary(p2)

## -----------------------------------------------------------------------------
str(p2, max.level = 2, components = "mapping")

## -----------------------------------------------------------------------------
p3 <- p2 + theme_classic()
str(p3)

## -----------------------------------------------------------------------------
str(p3, max.level = 2, components = "theme")

## -----------------------------------------------------------------------------
ggplot(mpg, aes(cyl, hwy, colour = factor(cyl))) + 
  geom_point() +
  geom_debug()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + 
  geom_point() + 
  geom_debug(summary.fun = head, summary.fun.args = list(n = 3))

## -----------------------------------------------------------------------------
ggplot(mpg, aes(cyl, hwy, colour = factor(cyl))) +
  stat_summary(fun.data = "mean_se") +
  stat_summary(fun.data = "mean_se", geom = "debug") 

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + 
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), geom = "debug")

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + 
  geom_null()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) + 
  stat_debug_group()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) + 
  geom_point() + 
  stat_debug_group()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) + 
  geom_point() + 
  stat_debug_panel()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + 
  geom_point() + 
  stat_debug_group()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + 
  geom_point() + 
  stat_debug_panel()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, shape = group)) + 
  geom_point() + 
  stat_debug_group()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + 
  geom_point() + 
  stat_debug_panel(summary.fun = nrow) +
  facet_wrap(~block)

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + 
  geom_point() + 
  stat_debug_group() +
  facet_wrap(~block)

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, shape = group)) + 
  geom_point() + 
  stat_debug_group(geom = "label", vjust = c(-0.5,1.5))

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + 
  geom_point() + 
  stat_debug_panel(geom = "label", aes(label = stat(paste("PANEL: ", PANEL)))) +
  facet_wrap(~block)

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, shape = group)) + 
  geom_point() + 
  stat_debug_group(geom = "debug")

## -----------------------------------------------------------------------------
pipe_assign <- function(value, name, pos = .GlobalEnv, ...) {
  assign(x = name, value = value, inherits = FALSE, pos = pos, ...)
}

ggplot(my.data, aes(x, y, colour = group)) + 
  geom_point() + 
  geom_debug(summary.fun = pipe_assign, 
             summary.fun.args = list(name = "debug_data"),
             print.fun = NULL)

head(debug_data)

