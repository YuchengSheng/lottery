library(tidyverse)
set.seed=1
selected <- c(1:5)
fsample <- function (n2) {sample_n(tibble(1:300), n2,replace = FALSE)}
fin <- function(n2) {selected %in% pull(fsample(n2)) %>% sum}
#fin(80)
flucky <- function (n){tibble(ID=1:n,n2=100) %>%     
                        mutate(lucky=map_int(n2,fin),           
                        llucky=ifelse(lucky>0,1,0)) %>%     
                        group_by(n2) %>%     
                        summarise(prop = sum(llucky)/n) %>% 
                        select(prop) %>% pull}
flucky(20)
tibble(SimID = 1:10,n1=100) %>%     
  mutate(prop=map_dbl(n1,flucky)) %>% 
  group_by(n1) %>% 
  summarise(mean=mean(prop))

1-(2/3)^5
