# =============================================================
# GenAI FYP — Estimation & Plots
# Requires the 8 .dta files produced by panel_construction.ipynb
# =============================================================

setwd("C:/LocalOneDrive/Documents/Desktop/NUS/Y4S1/FYP/causal-impact-GenAI/data/Positions")

needed <- c("haven", "dplyr", "fixest", "ggplot2", "purrr", "tibble",
            "tidyr", "patchwork", "readr", "lubridate", "stringr", "did", "scales")
to_install <- needed[!needed %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(haven);    library(dplyr);    library(fixest);   library(ggplot2)
library(purrr);    library(tibble);   library(tidyr);    library(patchwork)
library(readr);    library(lubridate);library(stringr);  library(did)
library(scales)

# =============================================================
# 1.  Shared style
# =============================================================

COL_ZERO  <- "#888780"
COL_TREAT <- "#3C3489"
COL_GRID  <- "#D3D1C7"
COL_TEXT  <- "#5F5E5A"
COL_TITLE <- "#2C2C2A"

TREAT_LABEL          <- "ChatGPT release (2022Q4)"
TREAT_LABEL_STAGGERED <- "First GenAI posting"

PAL_EST <- c(
  stock       = "#185FA5",
  hires       = "#2E7D32",
  separations = "#B23A2B",
  promotions  = "#C46A1C",
  placebo     = "#888780",
  series1     = "#185FA5",
  series2     = "#993C1D",
  lvl1        = "#185FA5",
  lvl2        = "#993C1D",
  lvl37       = "#888780"
)

PAL_CI <- c(
  stock       = "#B5D4F4",
  hires       = "#BFDDBB",
  separations = "#E6B8AF",
  promotions  = "#F2D1B3",
  placebo     = "#D3D1C7",
  lvl1        = "#B5D4F4",
  lvl2        = "#F5C4B3",
  lvl37       = "#D3D1C7"
)

theme_report <- function(base_size = 13, legend = "none") {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background   = element_rect(fill = "white", colour = NA),
      plot.background    = element_rect(fill = "white", colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = COL_GRID, linewidth = 0.4),
      axis.line.x        = element_line(colour = COL_GRID, linewidth = 0.5),
      axis.ticks.x       = element_line(colour = COL_GRID, linewidth = 0.4),
      axis.ticks.y       = element_blank(),
      axis.text          = element_text(colour = COL_TEXT,  size = base_size - 2),
      axis.title         = element_text(colour = COL_TEXT,  size = base_size - 1),
      plot.title         = element_text(colour = COL_TITLE, size = base_size + 2,
                                        face = "bold", margin = margin(b = 5)),
      plot.subtitle      = element_text(colour = COL_ZERO,  size = base_size - 1,
                                        margin = margin(b = 14)),
      plot.caption       = element_text(colour = COL_ZERO,  size = base_size - 4,
                                        hjust = 0, margin = margin(t = 12)),
      legend.position    = legend,
      legend.title       = element_blank(),
      legend.text        = element_text(colour = COL_TEXT, size = base_size - 2),
      legend.key.size    = unit(0.9, "lines"),
      plot.margin        = margin(14, 18, 12, 14)
    )
}

tq_of <- function(year, quarter) (year - 1960L) * 4L + (quarter - 1L)

# =============================================================
# 2.  Baseline DiD event study
# =============================================================

run_baseline_es <- function(data_path, outcome_var, rel_min = -7, rel_max = 10) {
  df <- read_dta(data_path) %>%
    mutate(firm_id = as.integer(firm_id), tq = as.integer(tq),
           treated = as.integer(treated))

  treat_tq    <- tq_of(2022, 4)
  panel_start <- tq_of(2021, 1)
  base_t      <- tq_of(2022, 3) - panel_start

  df <- df %>%
    filter(tq >= panel_start, tq <= tq_of(2025, 2)) %>%
    mutate(relq = tq - treat_tq, t_index = tq - panel_start) %>%
    filter(relq >= rel_min, relq <= rel_max)

  est      <- feols(
    as.formula(paste0(outcome_var,
                      " ~ i(t_index, treated, ref = ", base_t, ") | firm_id + tq")),
    data = df, cluster = ~firm_id
  )
  coef_vec <- coef(est)
  vc       <- vcov(est)

  map_dfr(c(seq(rel_min, -2L), -1L, seq(0L, rel_max)), function(r) {
    if (r == -1L)
      return(tibble(relq = r, att = 0, se = 0, ci_lo = 0, ci_hi = 0))
    ti   <- r + (treat_tq - panel_start)
    term <- paste0("t_index::", ti, ":treated")
    if (!(term %in% names(coef_vec)))
      return(tibble(relq = r, att = NA_real_, se = NA_real_,
                    ci_lo = NA_real_, ci_hi = NA_real_))
    att <- unname(coef_vec[term])
    se  <- sqrt(vc[term, term])
    tibble(relq = r, att = att, se = se,
           ci_lo = att - 1.96 * se, ci_hi = att + 1.96 * se)
  }) %>% arrange(relq)
}

plot_baseline_es <- function(es_df, title, subtitle = "Baseline DiD event study",
                             ylab, key, rel_min = -7, rel_max = 10) {
  post_sig <- filter(es_df, relq >= 0, !is.na(ci_lo), ci_lo > 0 | ci_hi < 0)

  ggplot(es_df, aes(x = relq, y = att)) +
    geom_hline(yintercept = 0, colour = COL_ZERO, linewidth = 0.5) +
    geom_vline(xintercept = -0.5, colour = COL_TREAT, linewidth = 0.6,
               linetype = "dashed") +
    annotate("label", x = -0.5, y = Inf, label = TREAT_LABEL,
             vjust = 1.5, hjust = 0.5, size = 3.2, colour = COL_TREAT,
             fill = "white", label.size = 0.3,
             label.padding = unit(0.2, "lines")) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
                fill = PAL_CI[[key]], alpha = 0.55) +
    geom_line(colour = PAL_EST[[key]], linewidth = 0.85) +
    geom_point(colour = PAL_EST[[key]], size = 2.5,
               shape = 21, fill = "white", stroke = 1.3) +
    geom_point(data = post_sig, colour = PAL_EST[[key]],
               size = 3, shape = 21, fill = PAL_EST[[key]], stroke = 0) +
    scale_x_continuous(breaks = seq(rel_min, rel_max),
                       labels = as.character) +
    labs(title = title, subtitle = subtitle,
         x = "Quarters relative to 2022Q4", y = ylab,
         caption = paste("Firm and quarter FE. 95% CI. SE clustered by firm.",
                         "Reference period: t = \u22121.")) +
    theme_report()
}

# Run and save
stock_es <- run_baseline_es(
  "firm_quarter_stock_active_ge20_chatgpt_treated_pretrend2021.dta",
  "log_employees_total"
)
ggsave("baseline_event_study_stock.png",
       plot_baseline_es(stock_es, "Employee stock",
                        ylab = "Effect on log employee stock", key = "stock"),
       width = 8, height = 4.5, dpi = 300, bg = "white")

hires_es <- run_baseline_es(
  "firm_quarter_new_hires_active_ge20_chatgpt_treated_pretrend2021.dta",
  "log_new_hires_total"
)
ggsave("baseline_event_study_new_hires.png",
       plot_baseline_es(hires_es, "New hires",
                        ylab = "Effect on log new hires", key = "hires"),
       width = 8, height = 4.5, dpi = 300, bg = "white")

seps_es <- run_baseline_es(
  "firm_quarter_separations_active_ge20_chatgpt_treated_pretrend2021.dta",
  "log_separations_total"
)
ggsave("baseline_event_study_separations.png",
       plot_baseline_es(seps_es, "Separations",
                        ylab = "Effect on log separations", key = "separations"),
       width = 8, height = 4.5, dpi = 300, bg = "white")

promos_es <- run_baseline_es(
  "firm_quarter_promotions_active_ge20_chatgpt_treated_pretrend2021.dta",
  "log_promotions_total"
)
ggsave("baseline_event_study_promotions.png",
       plot_baseline_es(promos_es, "Promotions",
                        ylab = "Effect on log promotions", key = "promotions"),
       width = 8, height = 4.5, dpi = 300, bg = "white")

# =============================================================
# 3.  Placebo test (new hires)
# =============================================================

get_event_df <- function(df, outcome_var, rel_min = -7, rel_max = 10) {
  treat_tq    <- tq_of(2022, 4)
  panel_start <- tq_of(2021, 1)
  base_t      <- tq_of(2022, 3) - panel_start

  df <- df %>%
    mutate(firm_id = as.integer(firm_id), tq = as.integer(tq),
           treated = as.integer(treated),
           relq = tq - treat_tq, t_index = tq - panel_start) %>%
    filter(relq >= rel_min, relq <= rel_max)

  est      <- feols(
    as.formula(paste0(outcome_var,
                      " ~ i(t_index, treated, ref = ", base_t, ") | firm_id + tq")),
    data = df, cluster = ~firm_id
  )
  coef_vec <- coef(est)
  vc       <- vcov(est)

  map_dfr(c(seq(rel_min, -2L), -1L, seq(0L, rel_max)), function(r) {
    if (r == -1L) return(data.frame(relq = r, coef = 0, se = 0))
    ti   <- r + (treat_tq - panel_start)
    term <- paste0("t_index::", ti, ":treated")
    if (!(term %in% names(coef_vec)))
      return(data.frame(relq = r, coef = NA_real_, se = NA_real_))
    data.frame(relq = r,
               coef = unname(coef_vec[term]),
               se   = sqrt(vc[term, term]))
  }) %>%
    mutate(ci_low = coef - 1.96 * se, ci_high = coef + 1.96 * se)
}

N_PLACEBO  <- 10
real_path  <- "firm_quarter_new_hires_active_ge20_chatgpt_treated_pretrend2021.dta"
real_event <- get_event_df(read_dta(real_path), "log_new_hires_total")

placebo_band <- map_dfr(seq_len(N_PLACEBO), function(i) {
  df <- read_dta(
    paste0("firm_quarter_new_hires_active_ge20_placebo", i, "_pretrend2021.dta")
  )
  get_event_df(df, "log_new_hires_total") %>% mutate(placebo_id = i)
}) %>%
  group_by(relq) %>%
  summarise(lower        = quantile(coef, 0.10, na.rm = TRUE),
            upper        = quantile(coef, 0.90, na.rm = TRUE),
            mean_placebo = mean(coef, na.rm = TRUE),
            .groups = "drop")

plot_df <- left_join(real_event, placebo_band, by = "relq")

p_placebo <- ggplot(plot_df, aes(x = relq)) +
  geom_hline(yintercept = 0, colour = COL_ZERO, linewidth = 0.5) +
  geom_vline(xintercept = 0, colour = COL_TREAT, linewidth = 0.6,
             linetype = "dashed") +
  annotate("label", x = 0, y = Inf, label = TREAT_LABEL,
           vjust = 1.5, hjust = 0.5, size = 3.2, colour = COL_TREAT,
           fill = "white", label.size = 0.3,
           label.padding = unit(0.2, "lines")) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = PAL_CI[["placebo"]], alpha = 0.55) +
  geom_line(aes(y = mean_placebo), colour = PAL_EST[["placebo"]],
            linetype = "dashed", linewidth = 0.7) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, y = coef),
                width = 0.2, colour = PAL_EST[["hires"]], alpha = 0.9) +
  geom_line(aes(y = coef), colour = PAL_EST[["hires"]], linewidth = 0.9) +
  geom_point(aes(y = coef), colour = PAL_EST[["hires"]], size = 2.5,
             shape = 21, fill = "white", stroke = 1.2) +
  scale_x_continuous(breaks = seq(min(plot_df$relq), max(plot_df$relq), 2)) +
  labs(title    = "Event study with placebo envelope",
       subtitle = "New hires",
       x = "Quarters relative to 2022Q4",
       y = "Effect on log new hires",
       caption  = paste("Grey band: 10th\u201390th percentile of placebo estimates.",
                        "Coloured line: actual estimate with 95% CI.")) +
  theme_report()

ggsave("event_study_placebo_band_new_hires.png", p_placebo,
       width = 8, height = 4.5, dpi = 300, bg = "white")

# =============================================================
# 4.  Baseline characteristics (treated vs control)
# =============================================================

stock_bc <- read_dta(
  "firm_quarter_stock_active_ge20_chatgpt_treated_pretrend2021.dta"
) %>%
  mutate(tq = as.integer(tq), treated = as.integer(treated),
         year  = 1960L + (tq %/% 4L), qtr = (tq %% 4L) + 1L,
         quarter   = paste0(year, "Q", qtr),
         Treatment = ifelse(treated == 1, "Treated", "Control")) %>%
  filter(tq >= tq_of(2021, 1))

# Compute group-quarter means for four share variables
bc_data <- bind_rows(
  stock_bc %>% transmute(tq, quarter, Treatment,
    value = employees_highexp / employees_total,  series = "% high-exposure workers"),
  stock_bc %>% transmute(tq, quarter, Treatment,
    value = employees_lvl1_2 / employees_total,   series = "% junior workers"),
  stock_bc %>% transmute(tq, quarter, Treatment,
    value = employees_lvl1   / employees_total,   series = "% entry-level workers"),
  stock_bc %>% transmute(tq, quarter, Treatment,
    value = employees_lvl2   / employees_total,   series = "% early professionals")
) %>%
  group_by(series, tq, quarter, Treatment) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

treat_tq_val <- tq_of(2022, 4)

plot_desc <- function(df, title_text) {
  ggplot(df, aes(x = tq, y = value, colour = Treatment)) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 2.2, shape = 21, fill = "white", stroke = 1.1) +
    geom_vline(xintercept = treat_tq_val, linetype = "dashed",
               colour = COL_TREAT, linewidth = 0.55) +
    scale_colour_manual(values = c("Control" = PAL_EST[["series1"]],
                                   "Treated" = PAL_EST[["series2"]])) +
    scale_x_continuous(
      breaks = sort(unique(df$tq)),
      labels = df %>% distinct(tq, quarter) %>% arrange(tq) %>% pull(quarter)
    ) +
    labs(title = title_text, x = NULL, y = NULL) +
    theme_report(legend = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
}

bc_plots <- bc_data %>%
  group_by(series) %>%
  group_map(~ plot_desc(.x, unique(.x$series)), .keep = TRUE)

ggsave("baseline_characteristics_clean.png",
       (bc_plots[[1]] | bc_plots[[2]]) / (bc_plots[[3]] | bc_plots[[4]]),
       width = 13, height = 10, dpi = 300, bg = "white")

# =============================================================
# 5.  % change in average employment: junior vs senior
# =============================================================

stock_emp <- read_dta(
  "firm_quarter_stock_active_ge20_chatgpt_treated_pretrend2021.dta"
) %>%
  mutate(tq = as.integer(tq),
         year    = 1960L + (tq %/% 4L),
         qtr     = (tq %% 4L) + 1L,
         quarter = paste0(year, "Q", qtr)) %>%
  filter(tq >= tq_of(2021, 1), tq <= tq_of(2025, 2))

base_tq <- tq_of(2021, 1)

df_emp <- stock_emp %>%
  group_by(tq, quarter) %>%
  summarise(`Junior (1-2)` = mean(employees_lvl1_2, na.rm = TRUE),
            `Senior (3-7)` = mean(employees_lvl3_7, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(c(`Junior (1-2)`, `Senior (3-7)`),
               names_to = "group", values_to = "avg_employment") %>%
  group_by(group) %>%
  arrange(tq, .by_group = TRUE) %>%
  mutate(base_value = avg_employment[tq == base_tq],
         pct_change = 100 * (avg_employment / base_value - 1)) %>%
  ungroup() %>%
  filter(!is.na(pct_change))

p_emp_change <- ggplot(df_emp,
  aes(x = tq, y = pct_change, colour = group, group = group)) +
  geom_hline(yintercept = 0, colour = COL_ZERO, linewidth = 0.5) +
  geom_vline(xintercept = treat_tq_val, linetype = "dashed",
             colour = COL_TREAT, linewidth = 0.55) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.2, shape = 21, fill = "white", stroke = 1.1) +
  annotate("label", x = treat_tq_val, y = Inf, label = TREAT_LABEL,
           vjust = 1.5, hjust = 0.5, size = 3.2, colour = COL_TREAT,
           fill = "white", label.padding = unit(0.2, "lines")) +
  scale_colour_manual(values = c("Junior (1-2)" = PAL_EST[["series1"]],
                                 "Senior (3-7)" = PAL_EST[["series2"]])) +
  scale_x_continuous(
    breaks = sort(unique(df_emp$tq)),
    labels = df_emp %>% distinct(tq, quarter) %>% arrange(tq) %>% pull(quarter)
  ) +
  labs(title = "% change in average employment over time",
       x = "Quarter", y = "% change in average employment") +
  theme_report(legend = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("stock_pct_change_avg_employment_all_firms.png", p_emp_change,
       width = 11, height = 5.5, dpi = 300, bg = "white")

# =============================================================
# 6.  Positions per quarter by occupation × seniority
#     (uses raw CSV — not produced by panel_construction)
# =============================================================

clean_positions <- read_csv("clean_positions.csv", show_col_types = FALSE) %>%
  mutate(onet_code = trimws(as.character(onet_code)),
         startdate = as.Date(startdate),
         enddate   = as.Date(enddate),
         seniority = as.numeric(seniority))

build_quarterly_position_stock_panel <- function(positions_df, occ_code,
                                                 start_quarter = "2021Q1",
                                                 end_quarter   = "2025Q2") {
  df <- filter(positions_df, onet_code == occ_code, !is.na(startdate))

  start_yr <- as.integer(substr(start_quarter, 1, 4))
  start_q  <- as.integer(substr(start_quarter, 6, 6))
  end_yr   <- as.integer(substr(end_quarter,   1, 4))
  end_q    <- as.integer(substr(end_quarter,   6, 6))

  quarter_seq <- seq(
    from = as.Date(sprintf("%d-%02d-01", start_yr, (start_q - 1) * 3 + 1)),
    to   = as.Date(sprintf("%d-%02d-01", end_yr,   (end_q   - 1) * 3 + 1)),
    by   = "3 months"
  )

  map_dfr(quarter_seq, function(q_start) {
    q_end   <- ceiling_date(q_start, "quarter") - days(1)
    q_label <- paste0(year(q_start), "Q", quarter(q_start))
    active  <- filter(df, startdate <= q_end, is.na(enddate) | enddate >= q_start)
    tibble(quarter          = q_label,
           positions_lvl1_2 = sum(active$seniority >= 1 & active$seniority <= 2, na.rm = TRUE),
           positions_lvl3_7 = sum(active$seniority >= 3 & active$seniority <= 7, na.rm = TRUE))
  })
}

plot_seniority_split <- function(panel, title_label, out_file) {
  df_plot <- bind_rows(
    transmute(panel, quarter, series = "Junior (1-2)", value = positions_lvl1_2),
    transmute(panel, quarter, series = "Senior (3-7)", value = positions_lvl3_7)
  )
  p <- ggplot(df_plot, aes(x = quarter, y = value,
                           colour = series, group = series)) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 2.2, shape = 21, fill = "white", stroke = 1.1) +
    geom_vline(xintercept = "2022Q4", linetype = "dashed",
               colour = COL_TREAT, linewidth = 0.55) +
    annotate("label", x = "2022Q4", y = Inf, label = TREAT_LABEL,
             vjust = 1.5, hjust = 0.5, size = 3.2, colour = COL_TREAT,
             fill = "white", label.size = 0.3,
             label.padding = unit(0.2, "lines")) +
    scale_colour_manual(values = c("Junior (1-2)" = PAL_EST[["series1"]],
                                   "Senior (3-7)" = PAL_EST[["series2"]])) +
    labs(title = paste0(title_label, ": Level 1-2 vs 3-7"),
         x = "Quarter", y = "Number of active positions") +
    theme_report(legend = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(out_file, p, width = 11, height = 6, dpi = 300, bg = "white")
  p
}

plot_seniority_split(
  build_quarterly_position_stock_panel(clean_positions, "15-1252.00"),
  "Software Developers", "software_developers_lvl12_vs_lvl37.png"
)
plot_seniority_split(
  build_quarterly_position_stock_panel(clean_positions, "13-1071.00"),
  "HR Specialists", "hr_specialists_lvl12_vs_lvl37.png"
)

# =============================================================
# 7.  GenAI integrator posting trends
# =============================================================

df_postings <- bind_rows(
  read_csv("../flagged_postings_gpt_predictions_2021Sep_Dec.csv", show_col_types = FALSE),
  read_csv("../flagged_postings_gpt_predictions.csv",             show_col_types = FALSE),
  read_csv("../flagged_postings_gpt_predictions_2025.csv",        show_col_types = FALSE)
) %>%
  mutate(post_date    = as.Date(post_date),
         quarter_date = as.Date(cut(post_date, "quarter")),
         quarter      = paste0(format(quarter_date, "%Y"), "Q",
                               ((as.integer(format(quarter_date, "%m")) - 1L) %/% 3L) + 1L))

integrators <- filter(df_postings, is_integrator_gpt == 1)

postings_per_q <- integrators %>%
  group_by(quarter_date, quarter) %>%
  summarise(postings_count = n(), .groups = "drop") %>%
  arrange(quarter_date)

firms_first_q <- integrators %>%
  group_by(company) %>%
  summarise(first_quarter_date = min(quarter_date, na.rm = TRUE), .groups = "drop") %>%
  mutate(quarter = paste0(format(first_quarter_date, "%Y"), "Q",
                          ((as.integer(format(first_quarter_date, "%m")) - 1L) %/% 3L) + 1L)) %>%
  group_by(first_quarter_date, quarter) %>%
  summarise(new_firms_count = n(), .groups = "drop") %>%
  arrange(first_quarter_date) %>%
  mutate(cumulative_firms = cumsum(new_firms_count))

p_postings <- ggplot(postings_per_q, aes(x = quarter, y = postings_count, group = 1)) +
  geom_line(colour = PAL_EST[["stock"]], linewidth = 0.85) +
  geom_point(colour = PAL_EST[["stock"]], size = 2.2,
             shape = 21, fill = "white", stroke = 1.1) +
  labs(title = "GenAI integrator postings per quarter",
       x = "Quarter", y = "Count of postings") +
  theme_report() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_new_firms <- ggplot(firms_first_q, aes(x = quarter, y = new_firms_count)) +
  geom_col(fill = PAL_EST[["hires"]], width = 0.7) +
  labs(title = "New GenAI adopter firms per quarter",
       x = "Quarter", y = "Count of new firms") +
  theme_report() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_cumulative <- ggplot(firms_first_q,
                       aes(x = quarter, y = cumulative_firms, group = 1)) +
  geom_area(fill = PAL_CI[["promotions"]], alpha = 0.7) +
  geom_line(colour = PAL_EST[["promotions"]], linewidth = 0.85) +
  geom_point(colour = PAL_EST[["promotions"]], size = 2.2,
             shape = 21, fill = "white", stroke = 1.1) +
  labs(title = "Cumulative firms with GenAI integrator postings",
       x = "Quarter", y = "Cumulative firm count") +
  theme_report() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("genai_integrator_postings_and_firms.png",
       p_postings / p_new_firms / p_cumulative,
       width = 10, height = 16, dpi = 300, bg = "white")

# =============================================================
# 8.  Industry distribution — treated vs control firms
# =============================================================

RECRUITMENT_INDUSTRIES <- c(
  "Recruitment and Staffing Services", "Employment and Staffing Services",
  "Employment and Recruitment Services", "Human Resources and Recruitment Services",
  "Online Employment Platforms", "Human Resources and Workforce Solutions",
  "Business Process Outsourcing Services"
)

# Expects raw postings already loaded as postings_df (bind 2022-2025 here)
# postings_df <- bind_rows(postings_df_2022, postings_df_2023,
#                          postings_df_2024, postings_df_2025)

flagged_df <- bind_rows(
  read_csv("../flagged_postings_gpt_predictions.csv",      show_col_types = FALSE),
  read_csv("../flagged_postings_gpt_predictions_2025.csv", show_col_types = FALSE)
)

integrator_firms <- flagged_df %>%
  filter(is_integrator_gpt == 1, !is.na(company), str_trim(company) != "") %>%
  distinct(company) %>% pull(company)

firm_industry_df <- postings_df %>%
  filter(!rics_k400 %in% RECRUITMENT_INDUSTRIES,
         !is.na(company), str_trim(company) != "",
         !is.na(rics_k50),  str_trim(rics_k50)  != "") %>%
  count(company, rics_k50, name = "n_postings_in_industry") %>%
  arrange(company, desc(n_postings_in_industry), rics_k50) %>%
  group_by(company) %>% slice(1) %>% ungroup() %>%
  mutate(treated = company %in% integrator_firms)

get_top10 <- function(df, treated_flag) {
  df %>%
    filter(treated == treated_flag) %>%
    count(rics_k50, name = "n_firms") %>%
    mutate(share_pct = 100 * n_firms / sum(n_firms)) %>%
    slice_max(share_pct, n = 10) %>%
    arrange(share_pct) %>%
    mutate(rics_k50 = factor(rics_k50, levels = rics_k50))
}

plot_top10 <- function(df, title_text, fill_colour, output_file) {
  p <- ggplot(df, aes(x = share_pct, y = rics_k50)) +
    geom_col(fill = fill_colour, width = 0.65) +
    geom_text(aes(label = sprintf("%.1f%%", share_pct)),
              hjust = -0.12, size = 3.8) +
    labs(title = title_text, x = "Share of firms (%)", y = "Industry (RICS K50)") +
    coord_cartesian(xlim = c(0, max(df$share_pct) * 1.18)) +
    theme_minimal(base_size = 12) +
    theme(plot.title       = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title       = element_text(size = 11),
          axis.text.y      = element_text(size = 10),
          axis.text.x      = element_text(size = 10),
          plot.background  = element_rect(fill = "white", colour = NA))
  ggsave(output_file, p, width = 9, height = 6, dpi = 300, bg = "white")
  p
}

plot_top10(get_top10(firm_industry_df, FALSE), "Top 10 Industries: Control Firms",
           "#4E79A7", "top10_industries_control_firms_firmshare.png")
plot_top10(get_top10(firm_industry_df, TRUE),  "Top 10 Industries: Treated Firms",
           "#E15759", "top10_industries_treated_firms_firmshare.png")

# =============================================================
# 9.  Callaway-Sant'Anna staggered DiD
# =============================================================

run_cs_event_study <- function(data_path, outcome_var, rel_min = -8, rel_max = 8) {
  df <- read_dta(data_path) %>%
    mutate(firm_id        = as.integer(firm_id),
           tq             = as.integer(tq),
           first_treat_tq = ifelse(is.na(first_treat_tq), 0L,
                                   as.integer(round(first_treat_tq))))

  att_obj <- att_gt(
    yname = outcome_var, tname = "tq", idname = "firm_id",
    gname = "first_treat_tq", data = df, panel = TRUE,
    control_group = "nevertreated", est_method = "dr",
    allow_unbalanced_panel = TRUE
  )
  dyn_obj  <- aggte(att_obj, type = "dynamic")
  event_df <- tibble(
    relq  = dyn_obj$egt,
    att   = dyn_obj$att.egt,
    se    = dyn_obj$se.egt,
    ci_lo = att - 1.96 * se,
    ci_hi = att + 1.96 * se
  ) %>% filter(relq >= rel_min, relq <= rel_max) %>% arrange(relq)

  list(att_gt = att_obj, dynamic = dyn_obj, event_df = event_df)
}

plot_cs_es <- function(es_df, title, subtitle = "Callaway-Sant\u2019Anna staggered DiD",
                       ylab, key, rel_min = -8, rel_max = 8) {
  post_sig <- filter(es_df, relq >= 0, !is.na(ci_lo), ci_lo > 0 | ci_hi < 0)

  ggplot(es_df, aes(x = relq, y = att)) +
    geom_hline(yintercept = 0, colour = COL_ZERO, linewidth = 0.5) +
    geom_vline(xintercept = 0, colour = COL_TREAT, linewidth = 0.6,
               linetype = "dashed") +
    annotate("label", x = 0, y = Inf, label = TREAT_LABEL_STAGGERED,
             vjust = 1.5, hjust = 0.5, size = 3.2, colour = COL_TREAT,
             fill = "white", label.size = 0.3,
             label.padding = unit(0.2, "lines")) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
                fill = PAL_CI[[key]], alpha = 0.55) +
    geom_line(colour = PAL_EST[[key]], linewidth = 0.85) +
    geom_point(colour = PAL_EST[[key]], size = 2.5,
               shape = 21, fill = "white", stroke = 1.3) +
    geom_point(data = post_sig, colour = PAL_EST[[key]],
               size = 3, shape = 21, fill = PAL_EST[[key]], stroke = 0) +
    scale_x_continuous(breaks = seq(rel_min, rel_max), labels = as.character) +
    labs(title = title, subtitle = subtitle,
         x = "Quarters relative to first GenAI integrator posting", y = ylab,
         caption = "Never-treated firms as controls. 95% CI.") +
    theme_report()
}

stock_cs <- run_cs_event_study("stock_staggered.dta",  "log_employees_total")
hires_cs <- run_cs_event_study("hires_staggered.dta",  "log_new_hires_total")
seps_cs  <- run_cs_event_study("seps_staggered.dta",   "log_separations_total")
promos_cs <- run_cs_event_study("promos_staggered.dta","log_promotions_total")

ggsave("cs_event_study_stock.png",
       plot_cs_es(stock_cs$event_df,  "Employee stock",  ylab = "Effect on log employee stock",  key = "stock"),
       width = 8, height = 4.5, dpi = 300, bg = "white")
ggsave("cs_event_study_new_hires.png",
       plot_cs_es(hires_cs$event_df,  "New hires",       ylab = "Effect on log new hires",        key = "hires"),
       width = 8, height = 4.5, dpi = 300, bg = "white")
ggsave("cs_event_study_separations.png",
       plot_cs_es(seps_cs$event_df,   "Separations",     ylab = "Effect on log separations",      key = "separations"),
       width = 8, height = 4.5, dpi = 300, bg = "white")
ggsave("cs_event_study_promotions.png",
       plot_cs_es(promos_cs$event_df, "Promotions",      ylab = "Effect on log promotions",       key = "promotions"),
       width = 8, height = 4.5, dpi = 300, bg = "white")

(plot_cs_es(stock_cs$event_df,  "Employee stock",  ylab = "Effect on log employee stock",  key = "stock") |
 plot_cs_es(hires_cs$event_df,  "New hires",       ylab = "Effect on log new hires",        key = "hires")) /
(plot_cs_es(seps_cs$event_df,   "Separations",     ylab = "Effect on log separations",      key = "separations") |
 plot_cs_es(promos_cs$event_df, "Promotions",      ylab = "Effect on log promotions",       key = "promotions"))

# =============================================================
# 10.  Sun-Abraham staggered DiD
# =============================================================

run_sa_event_study <- function(data_path, outcome_var, rel_min = -8, rel_max = 8) {
  df <- read_dta(data_path) %>%
    mutate(firm_id            = as.integer(firm_id),
           tq                 = as.integer(tq),
           first_treat_tq_raw = as.integer(round(first_treat_tq)))

  never_val <- max(df$tq, na.rm = TRUE) + 100L

  df <- mutate(df,
    first_treat_tq_sa = ifelse(
      is.na(first_treat_tq_raw) | first_treat_tq_raw == 0L,
      never_val, first_treat_tq_raw
    )
  )

  est    <- feols(
    as.formula(paste0(outcome_var,
                      " ~ sunab(first_treat_tq_sa, tq, ref.p = -1) | firm_id + tq")),
    data = df, cluster = ~firm_id
  )
  sa_sum <- summary(est, agg = "period")

  coef_df <- as.data.frame(sa_sum$coeftable) %>%
    mutate(term = rownames(.)) %>%
    transmute(term, att = Estimate, se = `Std. Error`,
              ci_lo = att - 1.96 * se, ci_hi = att + 1.96 * se) %>%
    filter(grepl("^tq::", term)) %>%
    mutate(relq = as.numeric(sub("^tq::", "", term))) %>%
    filter(relq >= rel_min, relq <= rel_max) %>%
    arrange(relq)

  if (!(-1L %in% coef_df$relq))
    coef_df <- bind_rows(coef_df,
                         tibble(term = "ref", att = 0, se = 0,
                                ci_lo = 0, ci_hi = 0, relq = -1)) %>%
               arrange(relq)

  list(model = est, event_df = coef_df, agg_summary = sa_sum)
}

plot_sa_es <- function(es_df, title, subtitle = "Sun-Abraham staggered DiD",
                       ylab, key, rel_min = -8, rel_max = 8) {
  post_sig <- filter(es_df, relq >= 0, !is.na(ci_lo), ci_lo > 0 | ci_hi < 0)

  ggplot(es_df, aes(x = relq, y = att)) +
    geom_hline(yintercept = 0, colour = COL_ZERO, linewidth = 0.5) +
    geom_vline(xintercept = 0, colour = COL_TREAT, linewidth = 0.6,
               linetype = "dashed") +
    annotate("label", x = 0, y = Inf, label = TREAT_LABEL_STAGGERED,
             vjust = 1.5, hjust = 0.5, size = 3.2, colour = COL_TREAT,
             fill = "white", label.padding = unit(0.2, "lines")) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
                fill = PAL_CI[[key]], alpha = 0.55) +
    geom_line(colour = PAL_EST[[key]], linewidth = 0.85) +
    geom_point(colour = PAL_EST[[key]], size = 2.5,
               shape = 21, fill = "white", stroke = 1.3) +
    geom_point(data = post_sig, colour = PAL_EST[[key]],
               size = 3, shape = 21, fill = PAL_EST[[key]], stroke = 0) +
    scale_x_continuous(breaks = seq(rel_min, rel_max), labels = as.character) +
    labs(title = title, subtitle = subtitle,
         x = "Quarters relative to first GenAI integrator posting", y = ylab,
         caption = "Never-treated firms as references. 95% CI. Reference: t = \u22121.") +
    theme_report()
}

stock_sa  <- run_sa_event_study("stock_staggered.dta",  "log_employees_total")
hires_sa  <- run_sa_event_study("hires_staggered.dta",  "log_new_hires_total")
seps_sa   <- run_sa_event_study("seps_staggered.dta",   "log_separations_total")
promos_sa <- run_sa_event_study("promos_staggered.dta", "log_promotions_total")

ggsave("sa_event_study_stock.png",
       plot_sa_es(stock_sa$event_df,  "Employee stock",  ylab = "Effect on log employee stock",  key = "stock"),
       width = 8, height = 4.5, dpi = 300, bg = "white")
ggsave("sa_event_study_new_hires.png",
       plot_sa_es(hires_sa$event_df,  "New hires",       ylab = "Effect on log new hires",        key = "hires"),
       width = 8, height = 4.5, dpi = 300, bg = "white")
ggsave("sa_event_study_separations.png",
       plot_sa_es(seps_sa$event_df,   "Separations",     ylab = "Effect on log separations",      key = "separations"),
       width = 8, height = 4.5, dpi = 300, bg = "white")
ggsave("sa_event_study_promotions.png",
       plot_sa_es(promos_sa$event_df, "Promotions",      ylab = "Effect on log promotions",       key = "promotions"),
       width = 8, height = 4.5, dpi = 300, bg = "white")

(plot_sa_es(stock_sa$event_df,  "Employee stock",  ylab = "Effect on log employee stock",  key = "stock") |
 plot_sa_es(hires_sa$event_df,  "New hires",       ylab = "Effect on log new hires",        key = "hires")) /
(plot_sa_es(seps_sa$event_df,   "Separations",     ylab = "Effect on log separations",      key = "separations") |
 plot_sa_es(promos_sa$event_df, "Promotions",      ylab = "Effect on log promotions",       key = "promotions"))

# =============================================================
# 11.  Mechanism plots — hiring rate, seniority event study,
#      seniority share, triple-DiD
# =============================================================

hires_df <- read_dta(
  "firm_quarter_new_hires_active_ge20_chatgpt_treated_pretrend2021.dta"
) %>% mutate(firm_id = as.integer(firm_id), tq = as.integer(tq),
             treated = as.integer(treated))

stock_df <- read_dta(
  "firm_quarter_stock_active_ge20_chatgpt_treated_pretrend2021.dta"
) %>% mutate(firm_id = as.integer(firm_id), tq = as.integer(tq),
             treated = as.integer(treated))

# ---- 11a. Hiring rate ----
hiring_rate_df <- hires_df %>%
  select(firm_id, tq, treated, new_hires_total) %>%
  left_join(select(stock_df, firm_id, tq, employees_total),
            by = c("firm_id", "tq")) %>%
  filter(tq >= tq_of(2021, 1), tq <= tq_of(2025, 2), employees_total > 0) %>%
  mutate(hiring_rate = new_hires_total / employees_total,
         group   = ifelse(treated == 1, "GenAI Adopting Firms", "Control Firms"),
         year    = 1960L + (tq %/% 4L),
         qtr     = (tq %% 4L) + 1L,
         quarter = paste0(year, "Q", qtr)) %>%
  group_by(group, tq, quarter) %>%
  summarise(mean_rate = mean(hiring_rate, na.rm = TRUE),
            se = sd(hiring_rate, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

p_hiring_rate <- ggplot(hiring_rate_df,
  aes(x = tq, y = mean_rate, colour = group, group = group)) +
  geom_hline(yintercept = 0, colour = COL_ZERO, linewidth = 0.4) +
  geom_vline(xintercept = treat_tq_val, colour = COL_TREAT,
             linewidth = 0.55, linetype = "dashed") +
  annotate("label", x = treat_tq_val, y = Inf, label = TREAT_LABEL,
           vjust = 1.5, hjust = 0.5, size = 3.2, colour = COL_TREAT,
           fill = "white", label.size = 0.3,
           label.padding = unit(0.2, "lines")) +
  geom_ribbon(aes(ymin = mean_rate - 1.96 * se,
                  ymax = mean_rate + 1.96 * se,
                  fill = group), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.2, shape = 21, fill = "white", stroke = 1.1) +
  scale_colour_manual(values = c("Control Firms"        = PAL_EST[["stock"]],
                                 "GenAI Adopting Firms" = PAL_EST[["series2"]])) +
  scale_fill_manual(values   = c("Control Firms"        = PAL_CI[["stock"]],
                                 "GenAI Adopting Firms" = PAL_CI[["separations"]])) +
  scale_x_continuous(
    breaks = sort(unique(hiring_rate_df$tq)),
    labels = hiring_rate_df %>% distinct(tq, quarter) %>% arrange(tq) %>% pull(quarter)
  ) +
  labs(title = "Hiring rate over time",
       subtitle = "Mean new hires / employees, by treatment group",
       x = NULL, y = "Hiring rate (hires / employees)",
       caption = "Shaded bands: 95% CI of the mean.") +
  theme_report(legend = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("hiring_rate_treated_vs_control.png", p_hiring_rate,
       width = 10, height = 5, dpi = 300, bg = "white")

# ---- 11b. Event study by seniority group ----
run_baseline_es_seniority <- function(df, outcome_var, rel_min = -7, rel_max = 10) {
  treat_tq    <- tq_of(2022, 4)
  panel_start <- tq_of(2021, 1)
  base_t      <- tq_of(2022, 3) - panel_start

  df <- df %>%
    filter(tq >= panel_start, tq <= tq_of(2025, 2)) %>%
    mutate(relq = tq - treat_tq, t_index = tq - panel_start) %>%
    filter(relq >= rel_min, relq <= rel_max)

  est      <- feols(
    as.formula(paste0(outcome_var,
                      " ~ i(t_index, treated, ref = ", base_t, ") | firm_id + tq")),
    data = df, cluster = ~firm_id
  )
  coef_vec <- coef(est);  vc <- vcov(est)

  map_dfr(c(seq(rel_min, -2L), -1L, seq(0L, rel_max)), function(r) {
    if (r == -1L) return(tibble(relq = r, att = 0, se = 0, ci_lo = 0, ci_hi = 0))
    ti   <- r + (treat_tq - panel_start)
    term <- paste0("t_index::", ti, ":treated")
    if (!(term %in% names(coef_vec)))
      return(tibble(relq = r, att = NA_real_, se = NA_real_,
                    ci_lo = NA_real_, ci_hi = NA_real_))
    att <- unname(coef_vec[term]);  se <- sqrt(vc[term, term])
    tibble(relq = r, att = att, se = se,
           ci_lo = att - 1.96 * se, ci_hi = att + 1.96 * se)
  }) %>% arrange(relq)
}

es_seniority <- bind_rows(
  run_baseline_es_seniority(hires_df, "log_new_hires_lvl1")   %>% mutate(group = "Entry-level (Level 1)"),
  run_baseline_es_seniority(hires_df, "log_new_hires_lvl2")   %>% mutate(group = "Early professionals (Level 2)"),
  run_baseline_es_seniority(hires_df, "log_new_hires_lvl3_7") %>% mutate(group = "Senior workers (Level 3\u20137)")
) %>%
  mutate(group = factor(group, levels = c(
    "Entry-level (Level 1)", "Early professionals (Level 2)", "Senior workers (Level 3\u20137)"
  )))

post_sig_sen <- filter(es_seniority, relq >= 0, !is.na(ci_lo), ci_lo > 0 | ci_hi < 0)

p_es_seniority <- ggplot(es_seniority,
  aes(x = relq, y = att, colour = group, fill = group, group = group)) +
  geom_hline(yintercept = 0, colour = COL_ZERO, linewidth = 0.5) +
  geom_vline(xintercept = -0.5, colour = COL_TREAT, linewidth = 0.55,
             linetype = "dashed") +
  annotate("label", x = -0.5, y = Inf, label = TREAT_LABEL,
           vjust = 1.5, hjust = 0.5, size = 3.2, colour = COL_TREAT,
           fill = "white", label.size = 0.3,
           label.padding = unit(0.2, "lines")) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.2, shape = 21, stroke = 1.1,
             aes(fill = after_scale(colour))) +
  geom_point(data = post_sig_sen, size = 3, shape = 21, stroke = 0) +
  scale_colour_manual(values = c(
    "Entry-level (Level 1)"          = PAL_EST[["lvl1"]],
    "Early professionals (Level 2)"  = PAL_EST[["lvl2"]],
    "Senior workers (Level 3\u20137)" = PAL_EST[["lvl37"]]
  )) +
  scale_fill_manual(values = c(
    "Entry-level (Level 1)"          = PAL_CI[["lvl1"]],
    "Early professionals (Level 2)"  = PAL_CI[["lvl2"]],
    "Senior workers (Level 3\u20137)" = PAL_CI[["lvl37"]]
  )) +
  scale_x_continuous(breaks = seq(-7L, 10L), labels = as.character) +
  labs(title    = "New hires by seniority group",
       subtitle = "Baseline DiD event study \u2014 treated vs. control firms",
       x = "Quarters relative to 2022Q4", y = "Effect on log new hires",
       caption  = paste("Firm and quarter FE. 95% CI. SE clustered by firm.",
                        "Reference: t = \u22121. Filled markers: CI excludes zero.")) +
  theme_report(legend = "top") +
  guides(colour = guide_legend(nrow = 1))

ggsave("event_study_new_hires_by_seniority.png", p_es_seniority,
       width = 10, height = 5.5, dpi = 300, bg = "white")

# ---- 11c. Share of new hires by seniority ----
hires_share_df <- hires_df %>%
  filter(tq >= tq_of(2021, 1), tq <= tq_of(2025, 2),
         new_hires_total > 0) %>%
  mutate(share_lvl1  = new_hires_lvl1   / new_hires_total,
         share_lvl2  = new_hires_lvl2   / new_hires_total,
         share_lvl37 = new_hires_lvl3_7 / new_hires_total,
         group   = ifelse(treated == 1, "GenAI Adopting Firms", "Control Firms"),
         year    = 1960L + (tq %/% 4L), qtr = (tq %% 4L) + 1L,
         quarter = paste0(year, "Q", qtr)) %>%
  pivot_longer(c(share_lvl1, share_lvl2, share_lvl37),
               names_to = "seniority", values_to = "share") %>%
  mutate(seniority = recode(seniority,
    share_lvl1  = "Entry-level (Level 1)",
    share_lvl2  = "Early professionals (Level 2)",
    share_lvl37 = "Senior workers (Level 3\u20137)"
  ),
  seniority = factor(seniority, levels = c(
    "Senior workers (Level 3\u20137)",
    "Early professionals (Level 2)",
    "Entry-level (Level 1)"
  ))) %>%
  group_by(group, tq, quarter, seniority) %>%
  summarise(mean_share = mean(share, na.rm = TRUE), .groups = "drop")

plot_share_panel <- function(df, group_label) {
  filter(df, group == group_label) %>%
    ggplot(aes(x = tq, y = mean_share,
               colour = seniority, fill = seniority, group = seniority)) +
    geom_vline(xintercept = treat_tq_val, colour = COL_TREAT,
               linewidth = 0.55, linetype = "dashed") +
    geom_area(alpha = 0.18, position = "identity") +
    geom_line(linewidth = 0.85) +
    geom_point(size = 1.8, shape = 21, fill = "white", stroke = 1.0) +
    scale_colour_manual(values = c(
      "Entry-level (Level 1)"          = PAL_EST[["lvl1"]],
      "Early professionals (Level 2)"  = PAL_EST[["lvl2"]],
      "Senior workers (Level 3\u20137)" = PAL_EST[["lvl37"]]
    )) +
    scale_fill_manual(values = c(
      "Entry-level (Level 1)"          = PAL_CI[["lvl1"]],
      "Early professionals (Level 2)"  = PAL_CI[["lvl2"]],
      "Senior workers (Level 3\u20137)" = PAL_CI[["lvl37"]]
    )) +
    scale_x_continuous(
      breaks = sort(unique(df$tq)),
      labels = df %>% distinct(tq, quarter) %>% arrange(tq) %>% pull(quarter)
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = group_label, x = NULL, y = "Share of new hires") +
    theme_report(legend = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
    guides(colour = guide_legend(nrow = 1))
}

ggsave("hire_share_by_seniority_treated_vs_control.png",
       plot_share_panel(hires_share_df, "GenAI Adopting Firms") /
       plot_share_panel(hires_share_df, "Control Firms") +
       plot_annotation(
         title    = "Composition of new hires by seniority level",
         subtitle = "Share of total new hires, by treatment group",
         caption  = "Sample: firms with \u226520 hires, 2021Q1\u20132025Q2.",
         theme    = theme(
           plot.title    = element_text(colour = COL_TITLE, size = 15, face = "bold"),
           plot.subtitle = element_text(colour = COL_ZERO,  size = 12),
           plot.caption  = element_text(colour = COL_ZERO,  size = 9, hjust = 0)
         )
       ),
       width = 10, height = 9, dpi = 300, bg = "white")

# ---- 11d. Triple-DiD: Level 2 vs Level 3-7 ----
hires_lvl2_vs_37 <- bind_rows(
  transmute(hires_df, firm_id, tq, treated,
            log_hires = log(new_hires_lvl2 + 1),   group = 1L),
  transmute(hires_df, firm_id, tq, treated,
            log_hires = log(new_hires_lvl3_7 + 1), group = 0L)
) %>%
  mutate(firm_group = paste0(firm_id, "_", group),
         post       = as.integer(tq >= tq_of(2022, 4)))

triple_did <- feols(
  log_hires ~ i(post, treated, ref = 0) +
    i(post, treated, ref = 0):group | firm_id + tq + firm_group,
  data = hires_lvl2_vs_37, cluster = ~firm_id
)

cat("\n=== Triple-DiD: Early Professionals (Level 2) vs Senior Workers (Level 3-7) ===\n")
print(summary(triple_did))