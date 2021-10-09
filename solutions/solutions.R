library(tidyverse)
library(infer)

#0a
set.seed(0)
tiradas <- sample(x = 0:1,size = 1000, replace = T)

#b
tiradas_cum <- cummean(tiradas)

#c

plot(tiradas_cum,type = 'l')
abline(h = 0.5, col = 'red')


# 1
gen_df <- read_csv('data/genincome.csv')

muestra_a <- sample_n(gen_df, 100)

muestra_b <- rbind(
    gen_df %>% filter(genero == 'hombre') %>% sample_n(30),
    gen_df %>% filter(genero == 'mujer') %>% sample_n(30),
    gen_df %>% filter(genero == 'otros') %>% sample_n(40)
)


muestra_c <- 
  gen_df %>% 
  mutate(row = row_number()) %>% 
  filter(row %% 1000 == 0)

muestra_d <- rbind(
  gen_df %>% filter(sede == 'Buenos Aires') %>% sample_n(50),
  gen_df %>% filter(sede == 'Córdoba') %>% sample_n(50)
)
  
# 2 


reps_a <- map_dbl(1:1000, ~mean(sample(muestra_a$ingreso,size = 100,replace = T)))
reps_b <- map_dbl(1:1000, ~mean(sample(muestra_b$ingreso,size = 100,replace = T)))
reps_c <- map_dbl(1:1000, ~mean(sample(muestra_c$ingreso,size = 100,replace = T)))
reps_d <- map_dbl(1:1000, ~mean(sample(muestra_d$ingreso,size = 100,replace = T)))


hist(reps_a)
hist(reps_b)
hist(reps_c)
hist(reps_d)

reps_a %>% quantile(c(.025,.975))
reps_b %>% quantile(c(.025,.975))
reps_c %>% quantile(c(.025,.975))
reps_d %>% quantile(c(.025,.975))


# 3
diff_means <- function(x){
  
  h_mu = x %>% filter(genero == 'hombre') %>% pull(ingreso) %>% mean()
  m_mu = x %>% filter(genero == 'mujer') %>% pull(ingreso) %>% mean()
  
  return(h_mu - m_mu)
}

reps_diff_means <- map_dbl(1:1000, ~ diff_means(sample_n(muestra_a, 100, replace = T)))

reps_diff_means %>% quantile(c(.05,.95))
reps_diff_means %>% quantile(c(.025,.975))
reps_diff_means %>% quantile(c(.005,.995))


reps_diff_means %>% 
  data.frame(diff_income = .) %>% 
  ggplot()+
  geom_histogram(aes(x = diff_income), col = 'white')+
  geom_vline(xintercept = quantile(reps_diff_means, .025), col = 'red', size = 2) +
  geom_vline(xintercept = quantile(reps_diff_means, .975), col = 'red', size = 2)


muestra_a %>% filter(genero != 'otros') %>%  
  specify(formula = ingreso ~ genero) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("hombre", "mujer")) %>%
  visualise()+
  shade_confidence_interval(quantile(reps_diff_means, c(.005, .995)))

