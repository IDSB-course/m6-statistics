library(tidyverse)
set.seed(0)
# bowl <- data.frame(
#   ball_number = 1:1e6,
#   color = sample(c('red', 'white'),prob = c(.3,.7),replace = T,size = 1e6)
# )
# 
# write_csv(bowl, 'data/bowl.csv')

# importamos bowl
bowl_df <- read_csv('data/bowl.csv') %>% mutate(is_red = if_else(color == 'red',1,0))


# sampleamos
muestras <- list()



for(i in 1:1000){

  muestras[['sample_25']] <- c(muestras[['sample_25']], mean(sample(bowl_df$is_red,size = 25,replace = TRUE)))
  muestras[['sample_300']] <- c(muestras[['sample_300']], mean(sample(bowl_df$is_red,size = 300,replace = TRUE)))
  muestras[['sample_5000']] <- c(muestras[['sample_5000']], mean(sample(bowl_df$is_red,size = 5000,replace = TRUE)))
}




# Hacemos histogramas

hist(muestras$sample_25)
hist(muestras$sample_300)
hist(muestras$sample_5000)

bind_rows(muestras) %>% 
  gather() %>% 
  ggplot(aes(x = value))+
  geom_density(aes(fill = key), alpha = .3)


bind_rows(muestras) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(mean = mean(value),
            sd = sd(value))

# sampleo sr
letters[1:5] %>% sample(5)

# sampleo cr
letters[1:5] %>% sample(5,replace = TRUE)


# bootstrap balls
muestra_bootstrap <- sample(bowl_df$is_red, 300, replace = TRUE)


#bootstrap reps

bootstrap_means <- function(sample_bs){
  x = sample(sample_bs,size = length(sample_bs),replace = TRUE) %>% 
    mean()
  return(x)
}


bootstrap_means(muestra_bootstrap)


replicas_bs <- map_dbl(1:1000, ~bootstrap_means(muestra_bootstrap))


hist(replicas_bs)

mean(replicas_bs)
sd(replicas_bs)


# percentile method CI

quantile(replicas_bs, c(.025, .975))

replicas_bs %>% 
  data.frame(prop_red = .) %>% 
  ggplot()+
  geom_histogram(aes(x = prop_red), col = 'white')+
  geom_vline(xintercept = quantile(replicas_bs, .025), col = 'red', size = 2) +
  geom_vline(xintercept = quantile(replicas_bs, .975), col = 'red', size = 2)


# SE method CI

data.frame(
  low_end = mean(replicas_bs) - 1.96* sd(replicas_bs),
  hi_end = mean(replicas_bs) + 1.96* sd(replicas_bs)
)



#movies dataset
# install.packages('moderndive')
library(moderndive)

movies_sample

ggplot(data = movies_sample, aes(x = genre, y = rating)) +
  geom_boxplot() +
  labs(y = "IMDb rating")



#imdb_copies
mean_diff_rep <- function(df){
  x <- df %>%
    sample_n(size = nrow(.), replace = TRUE) %>% 
    group_by(genre) %>% 
    summarise(means = mean(rating)) %>% 
    spread(key = genre, value = means) %>% 
    transmute(mean_diff = Action - Romance) %>% pull()
  return(x)
}


replicas_imdb <- map_dbl(1:1000, ~mean_diff_rep(movies_sample))

hist(replicas_imdb)


#CI
alpha <- 0.01
quantile(replicas_imdb, c(alpha/2, 1 - alpha/2))




movies_sample %>% 
  specify(formula = rating ~ genre) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("Action", "Romance")) %>% 
  get_p_value(obs_stat = mean(replicas_imdb), direction = 'both')
  

movies_sample %>% 
  specify(formula = rating ~ genre) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("Action", "Romance")) %>%
  visualise()+
  shade_p_value(obs_stat = mean(replicas_imdb), direction = 'both')
