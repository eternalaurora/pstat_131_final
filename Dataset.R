library(tidyverse)
library(fuzzyjoin)
library(stringdist)
library(janitor)

# load data
shooting = read_csv("fbref/shooting.csv") %>% clean_names()
passing = read_csv("fbref/passing.csv") %>% clean_names()
pass_types = read_csv("fbref/pass types.csv") %>% clean_names()
defense = read_csv("fbref/defense.csv") %>% clean_names()
possession = read_csv("fbref/possession.csv") %>% clean_names()
miscellany = read_csv("fbref/miscellaneous stats.csv") %>% clean_names()
fbref = shooting %>%
  left_join(passing) %>% 
  left_join(pass_types) %>% 
  left_join(defense) %>% 
  left_join(possession) %>%
  left_join(miscellany)
fbref = fbref %>% filter(x90s >= 10 & !str_detect(pos, "GK")) %>% select(-pos)
transfermarkt = read_csv("transfermarkt.csv") %>% 
  filter(position != "Goalkeeper")
whoscored = read_csv("whoscored.csv")
match_names = function(left, right, threshold) {
  fuzzy_left_join(left, right,
                  by = c("squad", "player"),
                  list(`==`, function (x, y) {
                    stringdist(x, y, "jw") <= threshold
                  })) %>%
    relocate(player.y, .after = player.x) %>% filter(!is.na(player.y))
}

# transfermarkt initial join
f_t_join = inner_join(fbref, transfermarkt)
f_t_disjoint = fbref %>% filter(!(rk %in% f_t_join$rk))
t_f_disjoint = transfermarkt %>% filter(!(id %in% f_t_join$id))

# transfermarkt attempt 1
transfermarkt_match = match_names(f_t_disjoint, t_f_disjoint, 0.1)
transfermarkt_match %>% count(squad.x, player.x) %>% filter(n > 1)
transfermarkt_match = transfermarkt_match %>%
  select(-c(squad.y, player.y)) %>% 
  rename(squad = squad.x, player = player.x)
f_t_join = f_t_join %>% bind_rows(transfermarkt_match)
f_t_disjoint = fbref %>% filter(!(rk %in% f_t_join$rk))
t_f_disjoint = transfermarkt %>% filter(!(id %in% f_t_join$id))

# transfermarkt attempt 2
transfermarkt_match = match_names(f_t_disjoint, t_f_disjoint, 0.2)
transfermarkt_match %>% count(squad.x, player.x) %>% filter(n > 1)
transfermarkt_match = transfermarkt_match %>%
  select(-c(squad.y, player.y)) %>% 
  rename(squad = squad.x, player = player.x)
f_t_join = f_t_join %>% bind_rows(transfermarkt_match)
f_t_disjoint = fbref %>% filter(!(rk %in% f_t_join$rk))
t_f_disjoint = transfermarkt %>% filter(!(id %in% f_t_join$id))

# transfermarkt attempt 3
transfermarkt_match = match_names(f_t_disjoint, t_f_disjoint, 0.3)
repeats = transfermarkt_match %>% count(squad.x, player.x) %>% filter(n > 1)
distinct_match = transfermarkt_match %>% 
  filter(!(player.x %in% repeats$player.x))
repeated_match = transfermarkt_match %>%
  filter(player.x %in% repeats$player.x)
distinct_match = distinct_match %>% filter(rk != 757)
repeated_match = repeated_match %>% filter(id == 2672)
transfermarkt_match = bind_rows(distinct_match, repeated_match) %>%
  select(-c(squad.y, player.y)) %>% 
  rename(squad = squad.x, player = player.x)
f_t_join = f_t_join %>% bind_rows(transfermarkt_match)
f_t_disjoint = fbref %>% filter(!(rk %in% f_t_join$rk))
t_f_disjoint = transfermarkt %>% filter(!(id %in% f_t_join$id))

# transfermarkt manual matching
t_manual = tibble(id = c(77, 2653, 2670, 3346, 956, 2720, 3192, 1829, 121, 
                         3690, 3417, 2171, 2098, 2310, 2870, 3602, 3832, 149,
                         1034, 19, 1152, 3355, 3493, 600, 1861, 2511)) %>%
  inner_join(transfermarkt)
transfermarkt_match = f_t_disjoint %>% 
  mutate(position = t_manual$position, id = t_manual$id)
f_t_join = f_t_join %>% bind_rows(transfermarkt_match) %>% select(-id)

# whoscored initial join
f_w_join = inner_join(fbref, whoscored)
f_w_disjoint = fbref %>% filter(!(rk %in% f_w_join$rk))
w_f_disjoint = whoscored %>% filter(!(id %in% f_w_join$id))

# whoscored attempt 1
whoscored_match = match_names(f_w_disjoint, w_f_disjoint, 0.1)
whoscored_match %>% group_by(squad.x, player.x) %>%
  summarise(count = n()) %>% filter(count > 1)
whoscored_match = whoscored_match %>%
  select(-c(squad.y, player.y)) %>% 
  rename(squad = squad.x, player = player.x)
f_w_join = f_w_join %>% bind_rows(whoscored_match)
f_w_disjoint = fbref %>% filter(!(rk %in% f_w_join$rk))
w_f_disjoint = whoscored %>% filter(!(id %in% f_w_join$id))

# whoscored attempt 2
whoscored_match = match_names(f_w_disjoint, w_f_disjoint, 0.2)
whoscored_match %>% count(squad.x, player.x) %>% filter(n > 1)
whoscored_match = whoscored_match %>%
  select(-c(squad.y, player.y)) %>% 
  rename(squad = squad.x, player = player.x)
f_w_join = f_w_join %>% bind_rows(whoscored_match)
f_w_disjoint = fbref %>% filter(!(rk %in% f_w_join$rk))
w_f_disjoint = whoscored %>% filter(!(id %in% f_w_join$id))

# whoscored attempt 3
whoscored_match = match_names(f_w_disjoint, w_f_disjoint, 0.3)
whoscored_match %>% count(squad.x, player.x) %>% filter(n > 1)
repeats = whoscored_match %>% count(squad.x, player.x) %>% filter(n > 1)
distinct_match = whoscored_match %>% filter(!(player.x %in% repeats$player.x))
repeated_match = whoscored_match %>% filter(player.x %in% repeats$player.x)
distinct_match = distinct_match %>% filter(rk != 757)
repeated_match = repeated_match %>% filter(id %in% c(2723, 2007, 1416))
whoscored_match = bind_rows(distinct_match, repeated_match) %>%
  select(-c(squad.y, player.y)) %>% 
  rename(squad = squad.x, player = player.x)
f_w_join = f_w_join %>% bind_rows(whoscored_match)
f_w_disjoint = fbref %>% filter(!(rk %in% f_w_join$rk))
w_f_disjoint = whoscored %>% filter(!(id %in% f_w_join$id))

# whoscored manual matching
w_manual = tibble(id = c(51, 2011, 2009, 2567, 730, 711, 2064, 97, 2791, 2249,
                         1607, 1573, 2204, 2732, 2910, 93, 769, 2, 856, 871,
                         2570, 2916, 2660, 1406)) %>% inner_join(whoscored)
whoscored_match = f_w_disjoint %>% 
  mutate(rating = w_manual$rating, id = w_manual$id)
f_w_join = f_w_join %>% bind_rows(whoscored_match) %>% select(-id)

# final merge
football_data = inner_join(f_t_join, f_w_join)
save(football_data, file = "football.Rdata")
