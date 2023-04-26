
install.packages("psych")
library(psych)
library(tidyverse)
install.packages("ltm")
library(ltm)
install.packages("jogRu")
library(jogRu)
dom_t1 <- elsoc %>% select(starts_with("dom_")) %>% select(ends_with("1")) %>% select(-starts_with("dom_index"))
dom_t2 <- elsoc %>% select(starts_with("dom_")) %>% select(ends_with("2")) %>% select(-starts_with("dom_index"))
dom_t3 <- elsoc %>% select(starts_with("dom_")) %>% select(ends_with("3")) %>% select(-starts_with("dom_index"))
dom_t4 <- elsoc %>% select(starts_with("dom_")) %>% select(ends_with("4")) %>% select(-starts_with("dom_index"))



ltm::cronbach.alpha(dom_t1,na.rm = T)  
ltm::cronbach.alpha(dom_t2,na.rm = T)  
ltm::cronbach.alpha(dom_t3,na.rm = T)  
ltm::cronbach.alpha(dom_t4,na.rm = T)  

psych:::alpha(dom_t1)
psych:::alpha(dom_t2)
psych:::alpha(dom_t3)
psych:::alpha(dom_t4)
