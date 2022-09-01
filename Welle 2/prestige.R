P <- dat %>% select(starts_with("C11"))

P <- P %>% mutate_each(funs(replace(., . == 1, 4)))
P <- P %>% mutate_each(funs(replace(., . == 3, 1)))
P <- P %>% mutate_each(funs(replace(., . == 4, 3)))