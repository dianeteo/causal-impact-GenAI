# Causal Impact of Generative AI Adoption on Firm-Level Workforce Outcomes

An empirical study of how firms' adoption of generative AI tools affects hiring, separations, promotions, and workforce composition, using a staggered difference-in-differences design on Singapore labour market data from Revelio Labs.

---

## Research Question

Did the widespread availability of generative AI (proxied by ChatGPT's November 2022 launch) causally change how firms hire, retain, and promote workers — and did those effects differ across seniority levels and occupation-level GenAI exposure?

---

## Identification Strategy

Treatment is defined at the firm level: a firm is "ever-treated" if it posted at least one GenAI *integrator* job (i.e. a role requiring building or operating LLM systems — RAG pipelines, vector databases, agents, fine-tuning) after November 2022. Treatment timing is staggered across firms.

Three estimators are used in parallel:

| Estimator | Method | Package |
|-----------|--------|---------|
| Baseline DiD | Two-way FE event study (firm + quarter FE) | `fixest` |
| Callaway-Sant'Anna | Doubly-robust ATT(g,t), never-treated controls | `did` |
| Sun-Abraham | Interaction-weighted staggered DiD | `fixest::sunab` |

Heterogeneous effects are estimated via a triple differences-in-differences model across four group dimensions:

| Dimension | Group = 1 | Group = 0 |
|-----------|-----------|-----------|
| A. Junior vs Senior | Levels 1–2 | Levels 3–7 |
| B. Entry-level vs Experienced | Level 1 | Levels 2–7 |
| C. Early Professionals vs Others | Level 2 | Levels 3–7 |
| D. AI Exposure | High (above median) | Low (below median) |

Pre-trends are tested with an extended panel back to 2019Q1.

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
1. Baseline DiD event studies (four outcomes: stock, hires, separations, promotions)
2. Placebo test — new hires (with 10-seed envelope)
3. Baseline characteristics (treated vs control over time)
4. Employment trends by seniority (% change from 2021Q1 baseline)
5. Occupation-level position stock plots (software developers, HR specialists)
6. GenAI adopter posting trends (postings per quarter, new adopters, cumulative)
7. Industry distribution (treated vs control firms, top 10 RICS K50)
8. Callaway-Sant'Anna staggered DiD (four outcomes)
9. Sun-Abraham staggered DiD (four outcomes)
10. Mechanism plots — hiring rate, seniority event study, hire share composition
11. Mechanism plots — seniority event study across Level 1 / Level 2 / Level 3–7
12. Triple differences-in-differences (heterogeneous effects across four group dimensions)

---

## Data

Data from [Revelio Labs](https://www.revelio.com/), accessed via WRDS and AWS Athena at NUS. Not included in this repository due to licensing restrictions.

| Source file | Description |
|---|---|
| `SG-{year}.csv` | Raw SG job postings from Revelio LinkedIn data (2021–2025) |
| `positions_stock.csv` | Individual position spells with seniority, company, start/end dates |
| `positions_stock_2019_onwards_robustness.csv` | Extended positions data for pre-trend robustness check |
| `occ_level.csv` | Occupation-level AI exposure scores (O\*NET, Eloundou et al. framework) |

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

## Related Literature

**Task-level exposure and susceptibility**
- Eloundou et al. (2024) — GPTs are GPTs: Labor market impact potential of LLMs
- Gmyrek et al. (2023, 2025) — Generative AI and jobs: ILO occupational exposure indices

**Micro-level productivity experiments**
- Noy & Zhang (2023) — Experimental evidence on the productivity effects of generative AI
- Brynjolfsson et al. (2025) — Canaries in the Coal Mine? Employment effects of AI
- Cui et al. (2025) — Effects of generative AI on high-skilled work: software developers

**Observational studies on aggregate labour market outcomes**
- Humlum & Vestergaard (2025) — Large language models, small labor market effects
- Hosseini & Lichtinger (2025) — GenAI as seniority-biased technological change
- Liu et al. (2025) — Labor demand in the shadow of generative AI
- Johnston & Makridis (2025) — Labor market effects of generative AI: a DiD analysis

**Identification and methodology**
- Callaway & Sant'Anna (2021) — Difference-in-differences with multiple time periods
- Sun & Abraham (2021) — Estimating dynamic treatment effects in event studies
- Liang et al. (2025) — Widespread adoption of LLM-assisted writing across society
