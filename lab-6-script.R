
# load packages -----------------------------------------------------------
install.packages(tidyverse)
library(tidyverse)
install.packages("lobstr")

# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")


# do stuff ----------------------------------------------------------------
fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()

fish_summary <-
  fish_long %>% 
  group_by(location) %>% 
  summarize(
    n = n(),
    mean = mean(species),
    sd = sd(species),
    sem = sd/sqrt(n),
    upper = mean + 1.96 * sem,
    lower = mean - 1.96 * sem
  ) %>% 
  print()


# T test ------------------------------------------------------------------

t.test(formula = species ~ location, data = fish_long)


#graphing the data 
ggplot(aes(x=))
