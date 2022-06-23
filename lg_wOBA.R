
lg_bat_sum <- pbp %>% group_by(game_date,game_year, p_throws) %>%
  summarise(BF = n_distinct(game_pk, at_bat_number),
            uBB = sum(events == "walk", na.rm = TRUE),
            HBP = sum(events == "hit_by_pitch", na.rm = TRUE),
            X1B = sum(events == "single", na.rm = TRUE),
            X2B = sum(events == "double", na.rm = TRUE),
            X3B = sum(events == "triple", na.rm = TRUE),
            HR = sum(events == "home_run", na.rm = TRUE),
            SH = sum(substr(events,1,3) == "sac", na.rm = TRUE),
            SO = sum(substr(events,1,9) == "strikeout", na.rm = TRUE),
            FB = sum(bb_type == "fly_ball", na.rm = TRUE),
            GB = sum(bb_type == "ground_ball", na.rm = TRUE),
            FB_HR = sum(events == "home_run" & bb_type == "fly_ball", na.rm = TRUE),
            PF = mean(PF)) %>%
  mutate(AB = BF - (uBB + HBP + SH)
         #     ,HR_FB_rate = FB_HR/FB
  ) %>%
  #  left_join(innings, by = c("game_date", "pitcher", "p_throws", "home_team", "away_team", "p_team", "p_league", "inning_topbot")) %>%
  arrange(p_throws, game_date)


bat_cum_r_lg <- roll_lg_woba(lg_bat_sum) %>%
  filter(p_throws == "R") %>%
  arrange(p_throws, game_date) %>%
  select("game_date","game_year", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  rename(wOBA_cum_r_lg = wOBA) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))

bat_cum_l_lg <- roll_lg_woba(lg_bat_sum) %>%
  filter(p_throws == "L") %>%
  arrange(p_throws, game_date) %>%
  select("game_date","game_year", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  rename(wOBA_cum_l_lg = wOBA) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))


bat_cum_tot_lg <- roll_lg_woba(lg_bat_sum, rl_excl = "Y") %>%
  arrange(game_date) %>%
  select("game_date", "game_year", "AB", "uBB", "SO", "wOBA") %>%
  rename(wOBA_cum_tot_lg = wOBA) %>%
  select(-c("AB", "uBB", "SO"))

bat_rol_r_lg <- roll_lg_woba(lg_bat_sum, 15) %>%
  filter(p_throws == "R") %>%
  arrange(p_throws, game_date) %>%
  select("game_date","game_year", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  rename(wOBA_rol_r_lg = wOBA) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))

bat_rol_l_lg <- roll_lg_woba(lg_bat_sum, 15) %>%
  filter(p_throws == "L") %>%
  arrange(p_throws, game_date) %>%
  select("game_date", "game_year", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  rename(wOBA_rol_l_lg = wOBA) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))


bat_rol_tot_lg <- roll_lg_woba(lg_bat_sum, 15, rl_excl = "Y") %>%
  arrange(game_date) %>%
  select("game_date", "game_year", "AB", "uBB", "SO", "wOBA") %>%
  rename(wOBA_rol_tot_lg = wOBA) %>%
  select(-c("AB", "uBB", "SO"))

bat_all_lg <- bat_cum_tot_lg %>%
  left_join(bat_cum_l_lg, by = c("game_date", "game_year")) %>%
  left_join(bat_cum_r_lg, by = c("game_date", "game_year")) %>%
  left_join(bat_rol_tot_lg, by = c("game_date", "game_year")) %>%
  left_join(bat_rol_l_lg, by = c("game_date", "game_year")) %>%
  left_join(bat_rol_r_lg, by = c("game_date", "game_year")) %>%
  distinct() %>%
  group_by(game_year) %>%
  mutate(real_dt = lag(game_date))





