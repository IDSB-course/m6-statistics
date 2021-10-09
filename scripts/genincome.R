library(tidyverse)
set.seed(0)


genero <- c('hombre','mujer','otros')
sede <- c('Buenos Aires', 'Córdoba', 'San Luis', 'Santa Fe', 'Mendoza')

genincome <- data.frame(
  genero = sample(genero, 100000, prob = c(.4723,.4913, .0364), replace = T),
  sede = sample(sede, 100000, prob = c(.3602,.1925, .1412, .1223, .1838), replace = T)
  
) %>% mutate(ingreso = NA) 

genincome[genincome$genero == 'hombre', 'ingreso'] <- genincome[genincome$genero == 'hombre',] %>% 
  mutate(ingreso = rlnorm(nrow(.),log(80000), log(2))) %>% 
  pull(ingreso)


genincome[genincome$genero == 'mujer', 'ingreso'] <- genincome[genincome$genero == 'mujer',] %>% 
  mutate(ingreso = rlnorm(nrow(.),log(58000), log(2))) %>% 
  pull(ingreso)

genincome[genincome$genero == 'otros', 'ingreso'] <- genincome[genincome$genero == 'otros',] %>% 
  mutate(ingreso = rlnorm(nrow(.),log(25000), log(2))) %>% 
  pull(ingreso)

genincome <- genincome %>% 
  mutate(row = row_number()) %>% 
  mutate(ingreso = case_when( row %% 100 ==0 ~ .5*ingreso,
                              T~ingreso))

genincome <- genincome %>% 
  mutate(row = row_number()) %>% 
  mutate(ingreso = case_when( sede == 'Buenos Aires' ~ 1.65*ingreso,
                              sede == 'Córdoba' ~1.4*ingreso,
                              sede == 'Santa Fe' ~ .7 * ingreso,
                              T~ingreso)) %>% 
  select(-row)


write_csv(genincome, 'data/genincome.csv')
