library(tidyverse)

path <- "./BruinSports/data/draftdata.csv"
df_career_stats <- read_csv(path)
  
drop_cols <- c('team_id', 'skip', 'mp', 'pts', 'trb', 'ast')

df_career_stats <- df_career_stats |> select(!drop_cols)
df_lot_picks <- df_career_stats |> filter(pick_overall < 15)

colnames(df_career_stats)

# plotting the distribution of points + rebounds + asts (PRA) per game
df_lot_picks <- df_lot_picks |> mutate(
  pra_per_g = pts_per_g + trb_per_g + ast_per_g,
  pick_overall = factor(pick_overall)
)

# Group by draft position, and getting the mean
draft_means <- df_lot_picks |> group_by(pick_overall) |>
  summarize(avg_mpg = mean(mp_per_g),
            avg_ppg = mean(pts_per_g),
            avg_trbpg = mean(trb_per_g),
            avg_apg = mean(ast_per_g),
            avg_prapg = mean(pra_per_g))

print(draft_means, n = 14)

df_lot_picks |> ggplot(aes(x = pick_overall, y = pra_per_g)) +
  geom_boxplot() + 
  labs(x = "Draft Pick", y = "Points-Rebounds-Assists Per Game")

