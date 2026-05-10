
library(tidyverse)
library(countrycode)
library(fixest)
library(modelsummary)
library(kableExtra)
library(ggrepel)
library(marginaleffects)   


#  Read and Clean WDI Data                         

cat("Reading WDI data...\n")
wdi_raw <- read_csv("World_Development_Indicators.csv")

# Reshape from wide year columns to long panel format
wdi_long <- wdi_raw %>%
  select(`Country Name`, `Country Code`, `Series Name`, `Series Code`,
         matches("\\d{4} \\[YR\\d{4}\\]")) %>%
  pivot_longer(
    cols      = matches("\\d{4} \\[YR\\d{4}\\]"),
    names_to  = "year",
    values_to = "value"
  ) %>%
  mutate(
    year  = as.integer(str_extract(year, "\\d{4}")),
    value = as.character(value),
    value = na_if(value, ".."),
    value = as.numeric(value),
    # Map series names to clean variable names
    variable = case_when(
      str_detect(`Series Name`, "Foreign direct investment") ~ "fdi_pct_gdp",
      str_detect(`Series Name`, "Portfolio equity")          ~ "portfolio_equity_bop",
      str_detect(`Series Name`, "GDP \\(current US")         ~ "gdp_current",
      str_detect(`Series Name`, "natural resources rents")   ~ "resource_rents",
      str_detect(`Series Name`, "Trade \\(% of GDP\\)")      ~ "trade_pct_gdp",
      str_detect(`Series Name`, "GDP per capita")            ~ "gdp_per_capita",
      TRUE ~ `Series Name`
    )
  ) %>%
  select(country = `Country Name`, iso3c = `Country Code`, year, variable, value)

# Handle any duplicate rows (keep mean)
wdi_long <- wdi_long %>%
  group_by(country, iso3c, year, variable) %>%
  summarize(value = mean(value, na.rm = TRUE), .groups = "drop")

# Pivot to one column per variable
wdi_panel <- wdi_long %>%
  pivot_wider(id_cols = c(country, iso3c, year),
              names_from = variable, values_from = value)


# Read and Clean WGI Data                         

cat("Reading WGI data...\n")
wgi_raw <- read_csv("wgi_data.csv")

wgi_long <- wgi_raw %>%
  select(`Country Name`, `Country Code`, `Series Name`,
         matches("\\d{4} \\[YR\\d{4}\\]")) %>%
  pivot_longer(
    cols      = matches("\\d{4} \\[YR\\d{4}\\]"),
    names_to  = "year",
    values_to = "value"
  ) %>%
  mutate(
    year  = as.integer(str_extract(year, "\\d{4}")),
    value = as.character(value),
    value = na_if(value, ".."),
    value = as.numeric(value),
    variable = case_when(
      str_detect(`Series Name`, "Regulatory Quality")       ~ "reg_quality",
      str_detect(`Series Name`, "Control of Corruption")    ~ "corruption_control",
      str_detect(`Series Name`, "Political Stability")      ~ "pol_stability",
      str_detect(`Series Name`, "Voice and Accountability") ~ "voice_accountability",
      TRUE ~ `Series Name`
    )
  ) %>%
  select(country = `Country Name`, iso3c = `Country Code`, year, variable, value)

wgi_long <- wgi_long %>%
  group_by(country, iso3c, year, variable) %>%
  summarize(value = mean(value, na.rm = TRUE), .groups = "drop")

wgi_panel <- wgi_long %>%
  pivot_wider(id_cols = c(country, iso3c, year),
              names_from = variable, values_from = value)


# Merge and Filter to Africa                       ─

# Get all African ISO3C codes using countrycode package
african_iso3c <- countrycode::codelist %>%
  filter(continent == "Africa") %>%
  pull(iso3c) %>%
  na.omit()

# Merge WDI + WGI on country code and year
panel <- wdi_panel %>%
  left_join(wgi_panel, by = c("iso3c", "year")) %>%
  mutate(country = coalesce(country.x, country.y)) %>%
  select(-country.x, -country.y) %>%
  # Add continent info and filter to Africa
  mutate(continent = countrycode(iso3c, "iso3c", "continent")) %>%
  filter(continent == "Africa") %>%
  select(-continent) %>%
  filter(year >= 2005, year <= 2022) %>%
  arrange(country, year) %>%
  # Convert portfolio equity from BoP current US$ to % of GDP
  # to make it comparable with FDI (% of GDP)
  mutate(portfolio_equity = (portfolio_equity_bop / gdp_current) * 100) %>%
  select(-portfolio_equity_bop, -gdp_current)


# Create Lagged Variables                         
# Lag corruption and regulatory quality by one year for placebo test


panel <- panel %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    corruption_control_lag1 = lag(corruption_control, 1),
    reg_quality_lag1        = lag(reg_quality, 1)
  ) %>%
  ungroup()


# Panel Diagnostics                            

cat("\n=== Panel Summary ===\n")
cat("Dimensions:", nrow(panel), "rows x", ncol(panel), "columns\n")
cat("Countries:", n_distinct(panel$country), "\n")
cat("Years:", min(panel$year), "–", max(panel$year), "\n\n")

cat("Missing data by variable:\n")
print(colSums(is.na(panel)))

reg_vars <- c("fdi_pct_gdp", "reg_quality", "corruption_control",
              "resource_rents", "trade_pct_gdp", "pol_stability",
              "gdp_per_capita", "voice_accountability")

n_complete <- sum(complete.cases(panel[, reg_vars]))
cat("\nComplete observations (all regression variables non-missing):\n")
cat(n_complete, "out of", nrow(panel), "\n")
# ^^^ Use `r n_complete` in the .qmd to fill the placeholder in the data section


# Summary Statistics Table                        ─

panel_labeled <- panel %>%
  select(
    `FDI Net Inflows (% of GDP)`          = fdi_pct_gdp,
    `Regulatory Quality (WGI)`            = reg_quality,
    `Control of Corruption (WGI)`         = corruption_control,
    `Voice and Accountability (WGI)`      = voice_accountability,
    `Natural Resource Rents (% of GDP)`   = resource_rents,
    `Trade Openness (% of GDP)`           = trade_pct_gdp,
    `Political Stability (WGI)`           = pol_stability,
    `GDP per Capita (current US$)`        = gdp_per_capita
  )

summary_table <- datasummary(
  All(panel_labeled) ~ N + Mean + SD + Min + Max,
  data    = panel_labeled,
  title   = "Table 1: Summary Statistics — African Country-Year Panel, 2005–2022",
  notes   = "Source: World Bank WDI and WGI. WGI scores range from approximately 0 (weakest) to 100 (strongest governance).",
  output  = "kableExtra"
)

print(summary_table)

# Figure 1: Two Separate Scatter Plots Combined with patchwork      
#


country_avg <- panel %>%
  group_by(country, iso3c) %>%
  summarize(
    fdi         = mean(fdi_pct_gdp,       na.rm = TRUE),
    reg_quality = mean(reg_quality,        na.rm = TRUE),
    corruption  = mean(corruption_control, na.rm = TRUE),
    resources   = mean(resource_rents,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Remove extreme FDI outliers that compress the scale
  filter(fdi > -10, fdi < 30,
         !is.na(reg_quality), !is.na(corruption)) %>%
  # Flag resource-rich countries (top quartile of resource rents)
  mutate(
    resource_rich = ifelse(
      resources >= quantile(resources, 0.75, na.rm = TRUE),
      "Resource-rich", "Other"
    )
  )

# Shared colour / shape scales so the legend is identical in both panels
col_scale  <- scale_color_manual(
  values = c("Resource-rich" = "#d95f02", "Other" = "#1f78b4"),
  name   = NULL
)
shp_scale  <- scale_shape_manual(
  values = c("Resource-rich" = 17, "Other" = 16),
  name   = NULL
)

# Shared theme applied to both panels
scatter_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle    = element_text(color = "grey40", size = 10,
                                    margin = margin(b = 8)),
    axis.title       = element_text(size = 11, color = "grey30"),
    axis.text        = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey93", linewidth = 0.4),
    legend.position  = "bottom",
    legend.text      = element_text(size = 11),
    plot.margin      = margin(10, 15, 5, 10)
  )

#
# Label the top-4 highest FDI countries, bottom-2, and any above 10 % of GDP

add_labels <- function(df, score_col) {
  df %>%
    mutate(label = case_when(
      rank(-fdi) <= 4 ~ country,
      rank(fdi)  <= 2 ~ country,
      fdi > 10        ~ country,
      TRUE            ~ NA_character_
    ))
}

#  Control of Corruption                    

df_corr <- add_labels(country_avg, corruption)

r_corr <- cor(country_avg$corruption, country_avg$fdi, use = "complete.obs")
p_corr <- cor.test(country_avg$corruption, country_avg$fdi)$p.value
corr_label <- paste0(
  "r = ", round(r_corr, 2),
  ifelse(p_corr < 0.05, "*", "")
)

fig1a <- ggplot(df_corr, aes(x = corruption, y = fdi)) +
  # Confidence band first so it sits behind points
  geom_smooth(method = "lm", se = TRUE,
              color = "#555555", fill = "grey80",
              alpha = 0.25, linewidth = 0.9) +
  # Points coloured and shaped by resource status
  geom_point(aes(color = resource_rich, shape = resource_rich),
             size = 3, alpha = 0.85, stroke = 0.4) +
  col_scale + shp_scale +
  # Country labels — repelled so they don't overlap points
  geom_text_repel(
    aes(label = label),
    size         = 3.1,
    color        = "grey20",
    fontface     = "italic",
    max.overlaps = 15,
    segment.color = "grey55",
    segment.size  = 0.3,
    box.padding   = 0.5,
    point.padding = 0.3,
    seed          = 42,
    na.rm         = TRUE
  ) +
  # Correlation coefficient in top-right corner
  annotate("text",
           x     = max(country_avg$corruption, na.rm = TRUE),
           y     = max(df_corr$fdi, na.rm = TRUE) * 0.97,
           label = corr_label,
           hjust = 1, vjust = 1,
           size  = 4.5, fontface = "bold", color = "grey25") +
  labs(
    title    = "A.  Control of Corruption",
    subtitle = "Higher score = stronger corruption control",
    x        = "Control of Corruption (WGI, 0–100)",
    y        = "FDI Net Inflows (% of GDP)"
  ) +
  scatter_theme

#  Regulatory Quality                     ─

df_reg <- add_labels(country_avg, reg_quality)

r_reg <- cor(country_avg$reg_quality, country_avg$fdi, use = "complete.obs")
p_reg <- cor.test(country_avg$reg_quality, country_avg$fdi)$p.value
reg_label <- paste0(
  "r = ", round(r_reg, 2),
  ifelse(p_reg < 0.05, "*", "")
)

fig1b <- ggplot(df_reg, aes(x = reg_quality, y = fdi)) +
  geom_smooth(method = "lm", se = TRUE,
              color = "#555555", fill = "grey80",
              alpha = 0.25, linewidth = 0.9) +
  geom_point(aes(color = resource_rich, shape = resource_rich),
             size = 3, alpha = 0.85, stroke = 0.4) +
  col_scale + shp_scale +
  geom_text_repel(
    aes(label = label),
    size          = 3.1,
    color         = "grey20",
    fontface      = "italic",
    max.overlaps  = 15,
    segment.color = "grey55",
    segment.size  = 0.3,
    box.padding   = 0.5,
    point.padding = 0.3,
    seed          = 42,
    na.rm         = TRUE
  ) +
  annotate("text",
           x     = max(country_avg$reg_quality, na.rm = TRUE),
           y     = max(df_reg$fdi, na.rm = TRUE) * 0.97,
           label = reg_label,
           hjust = 1, vjust = 1,
           size  = 4.5, fontface = "bold", color = "grey25") +
  labs(
    title    = "B.  Regulatory Quality",
    subtitle = "Higher score = stronger market regulations",
    x        = "Regulatory Quality (WGI, 0–100)",
    y        = "FDI Net Inflows (% of GDP)"
  ) +
  scatter_theme

#   7e. Combine with patchwork                         
# guide_area() places a single shared legend below both panels.
# plot_annotation() adds the overall title and caption.

fig1 <- (fig1a | fig1b) +
  plot_layout(guides = "collect") &       # merge the two identical legends into one
  theme(legend.position = "bottom")       # place shared legend below

fig1 <- fig1 +
  plot_annotation(
    title   = "FDI and Governance in Africa",
    subtitle = "Country-level averages, 2005–2022  |  Each point = one country  |  * p < 0.05",
    caption  = "Source: World Bank WDI & WGI. Countries with average FDI outside −10% to 30% of GDP excluded.",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 16, hjust = 0),
      plot.subtitle = element_text(color = "grey40", size = 11,
                                   margin = margin(b = 10)),
      plot.caption  = element_text(color = "grey50", size = 9, hjust = 0)
    )
  )

print(fig1)
ggsave("plot1.png", fig1, width = 12, height = 6, dpi = 300)


#   8. Figure 2: Interaction Visualization                   

panel_interaction_plot <- panel %>%
  filter(!is.na(corruption_control), !is.na(reg_quality),
         !is.na(fdi_pct_gdp)) %>%
  mutate(
    reg_quality_group = ifelse(
      reg_quality >= median(reg_quality, na.rm = TRUE),
      "Above Median Regulatory Quality",
      "Below Median Regulatory Quality"
    ),
    fdi_trimmed = ifelse(abs(fdi_pct_gdp) > 50, NA, fdi_pct_gdp)
  ) %>%
  filter(!is.na(fdi_trimmed))

fig2 <- ggplot(panel_interaction_plot,
               aes(x = corruption_control, y = fdi_trimmed)) +
  geom_point(alpha = 0.25, color = "#2c7fb8", size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#d95f02",
              fill = "#d95f02", alpha = 0.15, linewidth = 1.1) +
  facet_wrap(~ reg_quality_group) +
  labs(
    title    = "Does Regulatory Quality Condition Corruption's Effect on FDI?",
    subtitle = "Country-year observations, 2005–2022  |  Split at median Regulatory Quality",
    x = "Control of Corruption (WGI Estimate)",
    y = "FDI Net Inflows (% of GDP)",
    caption  = "Source: World Bank WDI & WGI. Observations with |FDI| > 50% of GDP excluded."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(face = "bold", size = 15),
    plot.subtitle     = element_text(color = "grey40", size = 11),
    plot.caption      = element_text(color = "grey50", size = 9, hjust = 0),
    strip.text        = element_text(face = "bold", size = 13),
    strip.background  = element_rect(fill = "grey95", color = NA),
    panel.grid.minor  = element_blank(),
    panel.spacing     = unit(2, "cm"),
    plot.margin       = margin(15, 15, 10, 10)
  )

print(fig2)
ggsave("plot2.png", fig2, width = 10, height = 5, dpi = 300)


#   9. Main Regression: Interaction Model with Two-Way FE           ─
#
# β₁ (corruption_control) captures the association between corruption and FDI
# when reg_quality = 0, i.e., at the THEORETICAL MINIMUM of regulatory quality
# (not the sample mean, which is ~43.72). Interpret with care.

model_main <- feols(
  fdi_pct_gdp ~ corruption_control * reg_quality +
    resource_rents + trade_pct_gdp + pol_stability + gdp_per_capita |
    country + year,
  data    = panel,
  cluster = ~country
)

summary(model_main)


#   10. Figure 3: Marginal Effects Plot (NEW)                 ─
# Plot how corruption's marginal effect on FDI varies continuously across the
# full observed range of regulatory quality, with 95% confidence intervals.
# This is more informative than reading the interaction coefficient alone because
# it shows exactly where in the distribution the moderation effect is meaningful
# and whether the effect is statistically distinguishable from zero.

reg_quality_range <- seq(
  min(panel$reg_quality, na.rm = TRUE),
  max(panel$reg_quality, na.rm = TRUE),
  length.out = 100
)

mfx <- slopes(
  model_main,
  variables = "corruption_control",
  newdata   = datagrid(reg_quality = reg_quality_range)
)

# Sample mean of reg_quality for reference line annotation
rq_mean <- mean(panel$reg_quality, na.rm = TRUE)

fig3 <- ggplot(mfx, aes(x = reg_quality, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = "#2c7fb8", alpha = 0.20) +
  geom_line(color = "#2c7fb8", linewidth = 1.1) +
  geom_vline(xintercept = rq_mean, linetype = "dotted",
             color = "grey30", linewidth = 0.7) +
  annotate("text", x = rq_mean + 1.5, y = max(mfx$conf.high, na.rm = TRUE) * 0.95,
           label = paste0("Sample mean\n(", round(rq_mean, 1), ")"),
           hjust = 0, size = 3.5, color = "grey30") +
  labs(
    title    = "Marginal Effect of Corruption Control on FDI across Regulatory Quality",
    subtitle = "Interaction model with country and year fixed effects | 95% confidence interval shaded",
    x = "Regulatory Quality (WGI, 0 = weakest → 100 = strongest)",
    y = "Marginal Effect of Corruption Control on FDI (% GDP)",
    caption  = "Source: World Bank WDI & WGI. Computed using the marginaleffects package."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "grey40", size = 11,
                                    margin = margin(b = 10)),
    plot.caption     = element_text(color = "grey50", size = 9, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92"),
    plot.margin      = margin(15, 15, 10, 10)
  )

print(fig3)
ggsave("plot3_marginal_effects.png", fig3, width = 10, height = 5, dpi = 300)


#   11. Placebo Test 1: Corruption × Voice and Accountability         ─
# Tests whether any governance dimension moderates corruption's effect on FDI,
# or whether the moderation is specific to regulatory quality.
# A non-significant interaction here supports the specificity of the main finding.

model_placebo_voice <- feols(
  fdi_pct_gdp ~ corruption_control * voice_accountability +
    resource_rents + trade_pct_gdp + pol_stability + gdp_per_capita |
    country + year,
  data    = panel,
  cluster = ~country
)

summary(model_placebo_voice)


#   12. Placebo Test 2: Lagged Governance                   
# WGI scores are very sticky year-to-year, so the lagged
# specification largely reproduces the main model with a one-year offset
# rather than providing genuinely independent evidence against reverse causality.
# The voice & accountability and portfolio equity placebos are the stronger tests.

model_placebo_lag <- feols(
  fdi_pct_gdp ~ corruption_control_lag1 * reg_quality_lag1 +
    resource_rents + trade_pct_gdp + pol_stability + gdp_per_capita |
    country + year,
  data    = panel,
  cluster = ~country
)

summary(model_placebo_lag)


#   13. Placebo Test 3: Portfolio Equity as Outcome              
# FDI requires navigating local regulations; portfolio equity does not.
# If the interaction is specific to long-horizon operational investment,
# it should NOT predict short-horizon portfolio flows.
# A non-significant interaction here strengthens the FDI-specific story.

model_placebo_portfolio <- feols(
  portfolio_equity ~ corruption_control * reg_quality +
    resource_rents + trade_pct_gdp + pol_stability + gdp_per_capita |
    country + year,
  data    = panel,
  cluster = ~country
)

summary(model_placebo_portfolio)


#   14. Comparison Table: All Models                     ─

models_list <- list(
  "Main Model"          = model_main,
  "Placebo: Voice"      = model_placebo_voice,
  "Placebo: Lagged"     = model_placebo_lag,
  "Placebo: Portfolio"  = model_placebo_portfolio
)

coef_map <- c(
  "corruption_control"                         = "Control of Corruption",
  "corruption_control_lag1"                    = "Control of Corruption (t−1)",
  "reg_quality"                                = "Regulatory Quality",
  "reg_quality_lag1"                           = "Regulatory Quality (t−1)",
  "voice_accountability"                       = "Voice & Accountability",
  "corruption_control:reg_quality"             = "Corruption × Reg Quality",
  "corruption_control:voice_accountability"    = "Corruption × Voice & Acct",
  "corruption_control_lag1:reg_quality_lag1"   = "Corruption (t−1) × Reg Quality (t−1)",
  "resource_rents"                             = "Resource Rents (% GDP)",
  "trade_pct_gdp"                              = "Trade (% GDP)",
  "pol_stability"                              = "Political Stability",
  "gdp_per_capita"                             = "GDP per Capita"
)

reg_table <- modelsummary(
  models_list,
  coef_map  = coef_map,
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map   = c("nobs", "r.squared", "adj.r.squared", "FE: country", "FE: year"),
  title     = "Table 2: Main Model and Placebo Tests",
  notes     = "Standard errors clustered by country in parentheses. All models include country and year fixed effects. Columns 1–3 use FDI (% GDP) as outcome; Column 4 uses portfolio equity (% GDP).",
  output    = "kableExtra"
)

print(reg_table)

cat("\n=== Analysis Complete ===\n")

