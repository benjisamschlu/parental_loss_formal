library(tidyverse)

d <- read_rds("~/Desktop/lt_initial/df_rates.rda")

d |> 
  ggplot(aes(age_loss, rate, color = parent)) + 
  geom_point()

lt <- read_table("~/Desktop/lt_initial/USlt.txt", skip = 2)

lt <- lt |> 
  filter(Year == "2020-2021") 

ages <- unique(d$age_loss)

lt <- lt |> 
  mutate(age_group = as.character(c(ages[1], ages, rep(ages[length(ages)], 9))))

lt_red <- lt |> 
  group_by(age_group) |> 
  summarize(dx = sum(dx), 
            Lx = sum(Lx)) |> 
  mutate(age_group = as.numeric(str_extract(age_group, "[0-9]+"))) |> 
  arrange(age_group) |> 
  mutate(lx = c(100000, (100000- cumsum(dx))[-14]) ) 
  


lt_initial <- lt_red |> 
  bind_cols(d |> 
              pivot_wider(names_from = "parent", values_from = "rate")
  ) |> 
  mutate(Lx_lost_mom = Lx*mom, 
         Lx_lost_dad = Lx*dad,
         Lx_lost_both = Lx*both,
         Lx_lost_neither = Lx*neither) |> 
  mutate(ex_lost_mom = rev(cumsum(rev(Lx_lost_mom)))/lx) |> 
  mutate(ex_lost_dad = rev(cumsum(rev(Lx_lost_dad)))/lx) |> 
  mutate(ex_lost_both = rev(cumsum(rev(Lx_lost_both)))/lx,
         ex_lost_neither = rev(cumsum(rev(Lx_lost_neither)))/lx) |>
  select(age_group, lx, dx, Lx, mom:neither, ex_lost_mom:ex_lost_neither)

lt_initial |> 
  write_csv("~/Desktop/lt_initial.csv")

lt_initial |> 
  select(age_group, ex_lost_mom:ex_lost_neither) |> 
  pivot_longer(-age_group) |> 
  mutate(name = str_remove(name, "ex_")) |> 
  ggplot(aes(age_group, value, color = name)) +
  geom_line()+
  labs(y = "expected number of years of life left", title = "expected years lived with/without parents",
       x = "age")
