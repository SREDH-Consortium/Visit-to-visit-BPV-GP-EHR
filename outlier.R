if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("MathiasHarrer/dmetar")
install.packages("tidyverse")
library("dmetar")
library("metafor")
library("meta")
library("tidyverse")
library("metasens")
dtSDonly <- SDonly
m.gen <- metagen (TE = HR, seTE = selogHR, data = dtSDonly)
m.gen
find.outliers(m.gen)
# Using all studies
tf <- trimfill(m.gen)

# Analyze with outliers removed
tf.no.out <- trimfill(update(m.gen, 
                             subset = -c(1, 5, 14, 19, 21, 23, 25, 27)))
summary(tf)
summary(tf.no.out)

metabias(m.gen, method.bias = "linreg")
eggers.test(m.gen)



m.gen$data %>% 
  mutate(y = HR/selogHR, x = 1/selogHR) %>% 
  lm(y ~ x, data = .) %>% 
  summary()

cop1 <- copas(m.gen)
plot(cop1)
cop1
