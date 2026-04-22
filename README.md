# Causal Impact of Generative AI Adoption on Firm-Level Workforce Outcomes

**NUS Final Year Project — Diane**

An empirical study of how firms' adoption of generative AI tools affects hiring, separations, promotions, and workforce composition, using a staggered difference-in-differences design on Singapore labour market data from Revelio Labs.

---

## Research Question

Did the widespread availability of generative AI (proxied by ChatGPT's November 2022 launch) causally change how firms hire, retain, and promote workers — and did those effects differ across seniority levels and occupation-level AI exposure?

---

## Identification Strategy

Treatment is defined at the firm level: a firm is "ever-treated" if it posted at least one GenAI *integrator* job (i.e. a role requiring building or operating LLM systems — RAG pipelines, vector databases, agents, fine-tuning) after November 2022. Treatment timing is staggered across firms.

Three estimators are used in parallel:

| Estimator | Method | Package |
|-----------|--------|---------|
| Baseline DiD | Two-way FE event study (firm + quarter FE) | `fixest` |
| Callaway-Sant'Anna | Doubly-robust ATT(g,t), never-treated controls | `did` |
| Sun-Abraham | Interaction-weighted staggered DiD | `fixest::sunab` |

Pre-trends are tested with an extended panel back to 2021Q1.

---

## Pipeline

```
deduplicate.ipynb
    ↓  deduped parquets (rolling 60-day window)
llm_inference.ipynb
    ↓  GPT-4o classifies keyword-flagged postings as integrator / non-integrator
panel_construction.ipynb
    ↓  firm-quarter .dta panels
placebo_tests.ipynb
    ↓  placebo .dta panels (random treatment assignment, 10 seeds)
analysis.R
    ↓  event study plots, robustness checks, mechanism plots
```

### 1. `deduplicate.ipynb`
Removes repost duplicates from raw Revelio job postings CSVs (2021–2025) using a rolling 60-day content-hash window. Deduplication runs across the full combined dataset before splitting by year, so cross-year reposts are caught.

**Inputs:** `data/Postings/SG-{year}.csv` (2021–2024), `data/Postings/2025/2025_postings_merged_JAN_JUNE.csv`  
**Outputs:** `data/Postings/SG-{year}-WITHOUT-REPOSTS-W60D.parquet` (2021–2025)

### 2. `llm_inference.ipynb`
Two-stage classification pipeline:
1. Keyword pre-filter (regex on ~30 GenAI-related terms) to narrow the candidate set
2. GPT-4o classifies each flagged posting as `integrator / user / both / none` with a structured JSON response

**Inputs:** Deduped parquets from step 1  
**Outputs:**
- `data/Firm Level/flagged_postings_gpt_predictions.csv` (2022–2024)
- `data/Firm Level/flagged_postings_gpt_predictions_2025.csv`
- `data/Firm Level/firms_ever_genai_integrator_first_date_2022_2025.csv`

### 3. `panel_construction.ipynb`
Builds balanced firm-quarter panels from Revelio position spells. Resolves overlapping position spells (contained positions) via a vectorised self-join. Produces four outcome panels at the `ge20` activity threshold (≥20 unique hires over the sample window):

| Panel file | Outcome |
|---|---|
| `firm_quarter_stock_active_ge20_...pretrend2021.dta` | Employee headcount (stock), with seniority and AI-exposure sub-counts |
| `firm_quarter_new_hires_active_ge20_...pretrend2021.dta` | New hires per quarter, with seniority sub-counts |
| `firm_quarter_separations_active_ge20_...pretrend2021.dta` | Separations (exits to different employer or non-employment) |
| `firm_quarter_promotions_active_ge20_...pretrend2021.dta` | Within-firm upward moves + promotion rate |
| `stock_staggered.dta`, `hires_staggered.dta`, `seps_staggered.dta`, `promos_staggered.dta` | Staggered versions with `first_treat_tq` for CS + SA estimators |

### 4. `placebo_tests.ipynb`
Generates 10 placebo new-hires panels by randomly assigning treatment to 1% of active firms per seed, holding the panel structure fixed. The resulting `.dta` files feed into the placebo envelope plot in `analysis.R`.

**Inputs:** `panel_construction.ipynb` outputs (clean positions + treated firms)  
**Outputs:** `data/Positions/firm_quarter_new_hires_active_ge20_placebo{1..10}_pretrend2021.dta`

### 5. `analysis.R`
Estimation and visualisation. Sections:
1. Baseline DiD event studies (four outcomes)
2. Placebo test — new hires
3. Baseline characteristics (treated vs control)
4. Employment trends by seniority
5. Occupation-level position stock plots
6. GenAI adopter posting trends
7. Industry distribution (treated vs control)
8. Callaway-Sant'Anna staggered DiD
9. Sun-Abraham staggered DiD
10. Mechanism plots — hiring rate, seniority event study, hire share composition, triple-DiD

---

## Data

Data from [Revelio Labs](https://www.revelio.com/), accessed via WRDS and AWS Athena at NUS. Not included in this repository due to licensing restrictions.

---

## Key Variables

| Variable | Description |
|---|---|
| `treated` | 1 if firm ever posted a GenAI integrator role after 2022-11-30 |
| `first_treat_tq` | Stata quarter index of firm's first integrator posting |
| `tq` | Stata quarter index (`(year - 1960) * 4 + (quarter - 1)`) |
| `seniority` | Revelio seniority score 1–7 (1 = intern, 7 = C-suite) |
| `high_exposure` | 1 if occupation's AI exposure score > sample median |

---

## Requirements

**Python** (notebooks)
```
pandas >= 2.0
numpy
pyarrow
openai
pyreadstat
tqdm
```

**R** (analysis)
```r
haven, dplyr, fixest, ggplot2, purrr, tibble,
tidyr, patchwork, readr, lubridate, stringr,
did, scales
```

---
