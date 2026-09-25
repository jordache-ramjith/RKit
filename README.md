# R Kit — Learn statistics — review release 0.5.1

A real R Shiny app, designed for beginners and for MMBS teaching. This release implements **Import and prepare data**, **Descriptive statistics**, **Estimation**, **Hypothesis testing** and **Linear regression**.

## Run the source app (easiest)

1. Install R (4.1 or newer) and RStudio if necessary.
2. Unzip the download and open the `mmbslearn` folder.
3. In RStudio choose **File → Open Project** and open `mmbslearn.Rproj`.
4. Open `run.R` and click **Source**. On the first run, missing packages are installed from CRAN. This needs an internet connection.
5. The Shiny app opens in your browser. Closing the R session stops it.

The app itself runs locally. Uploaded data are held in your session and are not sent to an external analysis service. Optional analysis ZIP downloads include the original data you loaded.

## Install as an R package

From R, install dependencies once:

```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "readr", "readxl", "zip"))
```

To use the separate source package installer, download mmbslearn_0.5.1.tar.gz from the MMBS app folder on Drive (adjust the path below). The ZIP already contains the complete source app; this installer is optional:

```r
install.packages("mmbslearn_0.5.1.tar.gz", repos = NULL, type = "source")
mmbslearn::run_mmbs_app()
```

Alternatively, install the source folder with `R CMD INSTALL mmbslearn` from a terminal. There is no compiled code in this package.

## First review: try this route

1. Select **Try example data** on Home.
2. Review the variable types and read the example data dictionary. The dataset contains 120 simulated participants, with some missing values; it does not describe real study findings.
3. Continue to preparation. Add `age ≥ 30` and `smoking ≠ Current`, choose **ALL**, preview and apply the filter.
4. Create `log_marker` from `marker` using **Natural log**. Inspect the before/after preview and apply.
5. Select **Explore these data → One numerical variable → log_marker → Explore**.
6. Inspect the histogram, boxplot and summary table. Expand **Show R code**.
7. Download **Script + original data**, unzip, open an RStudio project in that folder and run `analysis.R`. The result should match the app.
8. Open **Tutorial**, work through its five steps, and return to your own analysis. Your data and preparation are preserved.
9. Return Home and try **Two categorical variables**. Read the two-way count table first. Switch the percentage denominator: the percentage matrix, plot and script update together.
10. Try **Two numerical variables** with sleep and wellbeing; compare Pearson and Spearman and optionally add a group for separate panels.

## Changes in 0.5.1: worked regression tutorials

- **Tutorial step 3:** three programme categories become two dummy variables. A coding table, the full fitted equation and explicit 0/1 substitutions show why the reference mean is 55.95 and how the other means are obtained.
- **Tutorial step 4:** reveal overlapping sleep and age circles inside a square representing all outcome variation. The schematic explains shared information; the fitted comparison separately gives the actual sleep coefficients, intervals and plain-language interpretations. Confounding can create or distort an association. This lesson does not require the interaction lesson.
- **Tutorial step 5:** derive all three programme equations from one full model, by substitution, collection and simplification. Interpret the slopes, intervals, slope differences and joint interaction test in words.
- Tutorial scripts include the corresponding worked calculations. The confounding student script uses just sleep and age; plots and equations remain readable with internal scrolling or wrapping on narrow screens.

Find these at **Home → Linear regression → One predictor → Tutorial**, then use **Next** to reach steps 3–5. The several-predictor and interaction entry routes also open their corresponding tutorial steps.

## Changes in 0.5.0: linear regression and clearer testing

- Untouched hypothesis worked examples refresh when changing questions (including one-sample to paired). Uploaded data and prepared examples are preserved; use the worked-example button to explicitly replace them.
- Follow-up comparisons name their correction. Ordinary ANOVA offers Tukey, Holm or Bonferroni; Welch and rank comparisons offer Holm or Bonferroni. A seventh tutorial step for three or more groups explains family false-positive risk, Bonferroni thresholds, adjusted p-values, Holm and Tukey with numerical examples and a plot.
- **Home → Linear regression** offers one predictor, several predictors/adjustment, or interactions. Each has Analyse and Tutorial tabs.
- Select one numerical outcome, 1–6 predictors, categorical types/reference levels and specific two-way interactions. Main terms are retained. Both numerical and categorical interactions are supported.
- Inspect complete observations and unadjusted plots before fitting. Then read coefficients, confidence intervals, explained coefficients, full-model equations, overall fit and eligible partial F tests.
- Choose a categorical interactor to obtain each level's equation and conditional slopes or contrasts **from the same full model**. The app uses linear combinations of full-model coefficients and the full covariance matrix; it never fits separate stratum models for this display. Residual degrees of freedom remain from the full fit.
- Prediction plots hold other predictors at explicit profiles; numeric moderators use their 25th/50th/75th observed percentiles. Pointwise confidence intervals concern means. A separate profile table compares mean confidence intervals with new-person prediction intervals.
- Without interactions, compare unadjusted and adjusted focal associations using identical complete rows. A coefficient change is not automatically labelled proof of confounding.
- Residual versus fitted, normal Q–Q and Cook's-distance plots include beginner explanations. No influential observations are automatically removed.
- Seven contextual tutorial steps cover regression, equations, indicators, confounding, interactions, assumptions and uncertainty. Every worked example has executable R code and data downloads.
- Student scripts begin with ordinary `lm()`, `summary()`, `confint()` and `predict()` commands, including all data import/preparation. The advanced covariance calculation is explained in a separate section. Optional exact scripts reproduce conditional equations and complete app tables.

### Suggested regression review

1. Home → Linear regression → Interactions → Use the worked example.
2. Confirm independent observations; inspect the outcome and selected predictors, then fit the model.
3. Read the sleep coefficient and interaction coefficients; choose programme as the categorical interactor.
4. Read the three programme equations and sleep slopes. Age keeps a common coefficient because it has no interaction.
5. Change the fixed age value and inspect predictions. This changes the display profile, not the fitted model.
6. Inspect residuals and uncertainty; follow Tutorial links and return to the preserved analysis.
7. Download Script + original data; run both scripts in a fresh R session.

This module uses ordinary unweighted linear regression for independent observations. It rejects binary outcomes, constant variables, rank-deficient models and fits with fewer than three residual degrees of freedom. Such checks do not ensure a scientifically adequate sample size. It does not implement robust standard errors, mixed models, automated selection, higher-order interactions or causal identification. Regression coefficient and conditional-effect tests are unadjusted; their intervals are pointwise. Unsupported covariate combinations and extrapolation still need judgement.

R calculation references: [lm](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/lm.html), [predict.lm](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/predict.lm.html), [confint](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/confint.html), [p.adjust](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/p.adjust.html).

## Changes in 0.4.0: guided hypothesis testing

Open **Home → Hypothesis testing**. Choose a question, then use **Analyse** or **Tutorial**. Each route has a worked example that can be loaded without finding an external dataset.

| Question | Methods |
| --- | --- |
| One numerical variable | One-sample t-test, Wilcoxon signed-rank, exact sign test |
| Two paired numerical measurements | Paired t-test, signed-rank on paired changes, exact sign test |
| Numerical outcome in two independent groups | Welch or equal-variance t-test; Wilcoxon rank-sum / Mann–Whitney |
| Numerical outcome in 3–10 independent groups | Ordinary or Welch one-way ANOVA; Kruskal–Wallis; adjusted follow-up comparisons |
| One binary outcome | Exact binomial test or one-sample proportion approximation |
| Two categorical variables | Pearson chi-squared, Fisher exact for 2×2, simulated Fisher for larger tables |
| Paired binary measurements | McNemar approximation or exact binomial calculation among discordant pairs |

The numerical flow checks the study structure, shows complete-case counts and plots, asks for a normality judgement, and then asks about variances where relevant. Histograms and Q–Q plots supplement boxplots. A positive, right-skewed measurement can be inspected on a natural log scale. If the learner judges the transformed shape unsuitable, the app returns to a non-parametric route on the original scale. Log tests require an explicit acknowledgement that the question changes to geometric means or ratios. Paired t-tests inspect the distribution of changes, with no between-time variance test.

Non-parametric options link directly to their tutorials. Signed-rank choices ask about symmetry; a sign test is available when symmetry is doubtful. Rank tests are described as rank/distribution comparisons, not automatically as tests of means or medians.

Categorical routes show observed two-way counts before expected counts. For a simple conservative teaching rule, Pearson approximation options are offered only when all relevant expected counts are at least 5; approximate McNemar is offered from 25 discordant pairs. These are teaching guides, not universal definitions of validity. Fisher simulation uses 49,999 tables and seed 2026, explicitly labelled as a simulated approximation. No method switches silently after a failed calculation.

Results include context, estimates and confidence intervals when appropriate, descriptive summaries, figures, plain-language interpretation and optional native R output. Multiple-group follow-ups are Tukey for ordinary ANOVA, pairwise Welch with Holm adjustment for Welch ANOVA, and pairwise Wilcoxon with Holm adjustment for Kruskal–Wallis. Testing direction and alpha are chosen before results. A one-sided mean/proportion test gets a matching one-sided bound.

The six-step tutorials cover questions and null hypotheses, design and pairing, assumptions, each specific test, p-values and reporting. A separate simulation generates 100 imaginary sleep studies to illustrate false positives and power. Tutorial examples remain separate from uploaded analysis data.

As elsewhere, R code is collapsed and beginner-focused. Every analysis script includes original import, selected preparation, complete-case handling, test settings and ggplot code. ZIPs contain the original data, analysis.R and the optional analysis_exact.R. The New to R guide and narrated video are retained.

### Suggested review route

1. Home → Hypothesis testing → A numerical variable in two groups → Use the worked example.
2. Confirm the study structure, then inspect observations. Select approximately normal.
3. Compare both variances, choose Yes or Unsure, and read the chosen test before running it.
4. Expand the code and download Script + original data. Run analysis.R in a fresh RStudio project.
5. Review the observations again. Choose No, visit the non-parametric tutorial, return, try logs, and choose No again. Confirm that the result uses original-scale ranks.
6. Try paired measurements and compare the t, signed-rank and sign routes. Check that only complete pairs are used.
7. Try three groups and adjusted follow-ups. Try the categorical count tables and paired binary example.
8. Open the p-value tutorial, vary sample size and the true mean, then return to your analysis.

### Scope and interpretation

These routes are for independent observations, independent groups, or independent complete pairs as specified. Clustered data, survey designs, covariate adjustment and repeated measurements at three or more times need other methods. Normality judgements are educational guides; they are not proof, nor a recommendation to search for significance across tests. Normal approximations for very small tied rank samples may be poor. Degenerate data return an explanatory message instead of an invented result.

Statistical methods follow the R stats documentation: [t.test](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/t.test.html), [wilcox.test](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/wilcox.test.html), [oneway.test](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/oneway.test.html), [kruskal.test](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/kruskal.test.html), [chisq.test](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/chisq.test.html), [fisher.test](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/fisher.test.html), [binom.test](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/binom.test.html) and [mcnemar.test](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/mcnemar.test.html).

## Changes in 0.3.2

- **New to R → Watch data visualization** opens the supplied narrated video (36 minutes 34 seconds), with pause, seeking and fullscreen controls. It is also linked from the plotting lesson. The complete video is included for offline viewing, including in the practice ZIP.


- The default code view, copy button and script download now provide a student script: direct column names, ordinary assignments, table(), prop.table(), summary(), mean(), sd(), cor() and short ggplot2 calls. Custom app functions, tidy-evaluation syntax and responsive layout code are kept out of this view.
- Import and every selected preparation step remain in the student script. A nested optional download retains the exact app script. Data ZIPs contain both analysis.R (student) and analysis_exact.R, plus the original data.
- Student estimation scripts use t.test() confidence intervals, DescTools Wilson/Newcombe score functions and epitools log Wald ratios with explicit reference/event order. Proportion scripts list their additional packages at the top; the app itself does not require those suggested packages to run. Point estimates and intervals are checked against the app calculations. Student plots have simpler styling and tables may have a different layout.
- Student scripts explain missing-value handling and retain unavailable-interval safeguards. Their scripts are intended for the selected data and choices; the exact script remains available for the app's complete tables and presentation.
- The outcome label is now **Binary outcome variable**. Group choices exclude the outcome and update when it changes. If no separate variable identifies two groups, the app explains how to prepare one.
- The example study includes short_sleep: Yes for recorded sleep below seven hours, No otherwise, and missing when sleep is missing. It defaults to this outcome with programme as the grouping variable. This is a teaching category, not a clinical diagnosis.

## Changes in 0.3.1

The case-control explanation now distinguishes calculations within the selected sample from interpretation for a population. Choosing one left-handed and one right-handed person gives 50% left-handed by design; selecting different numbers changes that percentage without changing the population. Sample differences and ratios can be calculated, but do not directly estimate population risk differences or risk ratios under case-control sampling. Analysis notices and exported table notes use the same distinction. Statistical calculations are unchanged.

## Changes in 0.3.0

- All descriptive and estimation tutorials now begin with people, measurements and a study question. Each visual has context explaining what it shows and what to look for. Repeated sampling uses the idea of researchers repeating the same study with different participants; SD describes people and SE describes estimates across studies.
- Estimation has four routes: a mean, a proportion, means in two independent groups, and proportions in two independent groups. Comparisons retain each group’s summaries and add the direct comparison with a clearly selected reference group.
- Mean differences use a Welch t confidence interval. The six-step comparison tutorial shows why both means contribute uncertainty, what inclusion of zero means, and why it does not establish equality.
- Proportion comparisons start with a two-way count table and include risk difference (percentage points), risk ratio and odds ratio. An eight-step tutorial works through counts, risks, odds, comparisons, intervals and study design using the same fictional people. Ratio figures mark 1 as the no-difference value; difference figures mark 0.
- Risk-difference intervals use Newcombe’s hybrid Wilson score method without continuity correction. Risk-ratio and odds-ratio intervals use approximate log Wald methods. Small cells carry cautions; boundary cases report unavailable intervals explicitly, with no hidden continuity correction.
- One-time surveys use prevalence labels. Case-control sampling suppresses risk differences and risk ratios because the sample event percentages are not population risks. These remain simple unadjusted comparisons, not evidence of causation.
- Complete analysis exports include the original import and preparation, group estimates, direct comparisons and ggplot figures. Worked-example downloads include the exact generated data and settings.

## Changes in 0.2.1

The New to R guide now assumes no programming experience. It shows one step at a time and restores the original RStudio screenshots, with enlargement controls. It explains projects versus scripts, creating/saving/running a script, objects and assignment, functions and arguments, ?mean and the Help pane, packages, importing through the RStudio menus and preserving the generated code, selecting columns with $, and missing values. Pipes are introduced through equivalent commands before any piped preparation. Students then continue to filtering, new variables, summaries, plots and saving results. Optional references remain available.

The practice ZIP includes the offline guide, screenshots embedded in the page, practice data, the completed script and editable R Markdown. Interface screenshots are from the original course guide and the linked Posit documentation; menus may vary by version.

## Changes in 0.2.0

- Two categorical variables now show a labelled two-way count matrix with totals before the percentage denominator, in both Analyse and Tutorial. Counts and percentages have separate downloads.
- Estimation offers a population mean or binary proportion, optionally by one grouping variable, with 90%, 95% or 99% confidence intervals.
- Students inspect observations and distributions, review independence, then calculate and interpret the interval.
- Mean intervals use Student's t method; proportions use Wilson score intervals. Results report observed and missing counts, standard errors and limits. Group intervals describe each group separately, not a difference between groups.
- Six-step tutorials introduce the population and sample, repeated sampling, SD versus SE, confidence interval coverage, sample size and confidence, then reporting. Simulations generate 100 samples and include standalone R code.
- Every analysis script reproduces import, all selected preparation, estimation and figures. Tutorial scripts reproduce the worked example or the displayed simulation.

## Changes in 0.1.2

- New to R opens an embedded beginner guide with a Home button and optional full-page view.
- Simplified explanations use one fictional dataset, evaluated R examples and complete outputs.
- Downloadable practice ZIP includes the CSV, complete R script and editable R Markdown source.
- Advanced reference sections are collapsed; the guide contains no personal attribution.

## Changes in 0.1.1

- Analysis and tutorial figures keep a readable minimum width inside a scrollable area.
- Facets use one or two columns and grow vertically; labels wrap and many categories use horizontal plots.
- Figure downloads include every panel at the full figure size. Scripts include the matching plot layout.
- Tutorials begin with definitions and examples; completed descriptions use the displayed numbers and update with tutorial controls.
- Home's **Clear data & import new** button clears the current dataset, preparation history and results, then opens Import.

## Implemented

- CSV/text import with delimiter, decimal mark, encoding and missing-value settings; Excel sheet selection; preview before confirming a replacement.
- Explicit variable-type changes, with protection against losing nonnumeric text during conversion.
- Multiple filter conditions with AND/OR; new filters applied sequentially; missingness conditions; preview; undo last step.
- New variables: natural log, multiply/add a constant, difference, ratio, threshold categories, replace category labels. All operations preserve the original variable. Category replacement can be repeated through further derived columns.
- Five descriptive routes: one categorical, one numerical, numerical by groups (optionally two grouping variables), two categorical, and two numerical with Pearson/Spearman correlation (optionally by group).
- Counts, denominators and missingness are visible. Empty/invalid analyses explain the problem instead of silently losing rows.
- Step-by-step, self-contained tutorials with simulated data; interactive outlier, log, percentage and correlation examples.
- ggplot2 figures; table and 300-dpi figure downloads; collapsed complete R code, copy, `.R` and ZIP downloads. Tutorial exports contain the exact simulated scenario being displayed.

## Deliberate limits

- Correlation remains descriptive; hypothesis tests and ordinary linear regression have their own guided modules.
- Estimation covers independent observations: one mean or binary proportion, optionally within groups, and direct comparisons of exactly two groups. There are no paired, clustered, weighted or bootstrap intervals. Risk ratios require a common, meaningful follow-up period; rates and survival outcomes are outside this release.
- A mean interval requires at least two observed values and nonzero sample SD; otherwise the point estimate is shown with an explanation. Small samples with marked skew or outliers need caution. The app does not treat a checkbox as proof that assumptions hold.
- Wilson intervals retain uncertainty with zero or all events. The usual estimated SE can still be zero at those boundaries; the tutorial explains the distinction.
- No joins, reshaping, free-form R expressions, arbitrary transformations, saved sessions or batch editing.
- Natural log is the only guided distributional transformation. Zero/negative inputs stop that operation; no automatic constants or exclusions.
- At most 20 MB, 100,000 rows and 200 columns. Categorical displays allow up to 30 observed categories.
- Preparation history supports undoing the latest step. To change an earlier step, undo later steps first.
- After changing descriptive variable choices, press **Explore** again. Percentage-denominator changes update automatically. Previous results remain clearly marked until rerun; their download always matches the displayed result.
- Tutorial state is intentionally separate from uploaded data. The example datasets and all plots are statistical illustrations, not evidence for medical decisions.

## Structure

- `R/run_app.R`: exported package launcher.
- `inst/app/app.R`: Shiny UI and session orchestration.
- `inst/app/engine.R`: preparation, analysis templates and full-script generation.
- `inst/app/content.R`: descriptive lessons and explanations.
- `inst/app/learner_code.R`: beginner script generation for preparation, descriptions, estimation and simulations.
- `inst/app/estimation.R`: estimation calculations, scripts and simulations.
- `inst/app/estimation_server.R`: estimation flow.
- `inst/app/comparisons.R`: independent-group calculations, comparison plots and script templates.
- `inst/app/estimation_lessons.R` and `comparison_tutorial.R`: contextual lessons and interactive worked comparisons.
- `inst/app/www/style.css`: visual design.
- `inst/extdata/wellbeing.csv`: simulated example.
- `tests/core.R`, `tests/estimation.R`, `tests/comparisons.R` and `tests/learner.R`: statistical and fresh-session reproducibility checks.

Generated scripts use ordinary `dplyr`, `readr`/`readxl` and `ggplot2` commands. They do not require this app package. Exact-script calculation templates are shared with app execution. Student scripts use equivalent standard R functions and have separate numeric agreement checks. The code generator uses fixed operations and safely quoted names/values; it does not run arbitrary user expressions.

## Review priorities

Please assess whether the initial choices are natural for a beginner, the visual pacing feels right, the tutorials explain enough without jargon, and the generated R is suitable for your teaching. Review the model choices, interpretations, interaction equations and diagnostic guidance before teaching use.

## Implementation references

- Newcombe score difference intervals and reference examples: https://andrisignorell.github.io/DescTools/reference/BinomDiffCI.html
- Log Wald risk ratios: https://search.r-project.org/CRAN/refmans/epitools/html/riskratio.html
- Log Wald odds ratios: https://search.r-project.org/CRAN/refmans/epitools/html/oddsratio.html
- R mean intervals: https://www.stat.ethz.ch/R-manual/R-devel/library/stats/html/t.test.html
- R score intervals: https://www.stat.ethz.ch/R-manual/R-devel/library/stats/html/prop.test.html
- NIST confidence intervals: https://www.itl.nist.gov/div898/handbook/eda/section3/eda352.htm
- Posit Shiny dynamic UI: https://shiny.posit.co/r/articles/build/dynamic-ui/
- ggplot2 programming with aesthetics: https://ggplot2.tidyverse.org/reference/aes.html

The existing MMBS materials informed scope; playful course-video titles are not used in the app.

The app is now named **R Kit — Learn statistics**. Its footer credits Jordache Ramjith, PhD, with the requested Radboud affiliation and a clickable email address.
