# Getting started with R and preparing data
# All data are fictional. Open an RStudio project in this folder.
# If needed, install these packages once:
# install.packages(c("dplyr", "readr", "readxl", "ggplot2", "tidyr"))

# 1. Open RStudio
# Begin here even if you have never written a computer instruction. You only need to follow one small step at a time.

# R is the program that does the calculations. RStudio is the application in which you write instructions for R and see the results. We use RStudio throughout this guide. You do not need to open a separate R window.

# 2. Find your way around the screen
# A pane is one area of the RStudio window. Tabs let the same pane show different things.

# The screenshot below comes from an older RStudio version. Colours and extra tabs may differ on your computer. The important names are Source, Console, Environment, Files, Plots and Help. If there is no editor at the top left yet, that is normal: creating a script in step 4 opens it.

# A script tab and a data-viewer tab can share the Source pane. Clicking the tab changes what you see; it does not delete the other tab. The Environment and Files are different: an item in memory is not automatically a file saved on your computer.

# 3. Make a project folder
# An RStudio project keeps the files for one piece of work together. A folder is also called a directory.

# First download the practice files using the button below. A ZIP is a compressed folder: you must extract it before working with its contents. On macOS, double-click it in Finder. On Windows, right-click it in File Explorer and choose Extract All. Move the extracted folder somewhere easy to find, such as Documents, and name the folder r-practice.

# One project can contain several scripts. The .Rproj file does not contain all your code or data. If you share your work, send the relevant folder and its files, not just the .Rproj file. Next time, double-click the .Rproj file, or choose File → Open Project in RStudio.

# 4. Create and save an R script
# A script is a text file containing instructions for R, saved so that you can repeat or change your work later.

# Reference only (run separately if needed):
# # My first R script
# 2 + 3

# The # starts a comment: a note for people reading the script. R ignores the rest of that line. The second line asks R to add 2 and 3. Saving stores these instructions on disk; it does not carry out the calculation.

# 5. Run one instruction and read its answer
# Running code means asking R to carry out the instructions you wrote.

2 + 3

# The answer is 5. The printed [1] means that the first displayed value is item 1; it is not part of the answer. You may see > before a command in the Console. That is R’s ready prompt, not something to copy into your script.

# Optional shortcut: Control + Enter on Windows or Command + Enter on macOS runs the current line or selected code. You can keep using the Run button instead.

# 6. Give a value a name
# An object is a named item in R’s memory. It can hold one number, several values, a table or a result.

# Type and run the first line below. The <- symbol is a less-than sign followed by a hyphen. Read it as “store the value on the right under the name on the left”. Then run the second line to display that value.

hours <- 7
hours

# The first line creates hours in the Environment and usually prints no answer. The second line asks R to show what hours contains: 7. You choose the name hours; it is not a built-in keyword.

hours * 60
hours

# The calculation gives 420 minutes, but hours still contains 7. To keep a calculation’s result, assign it a name:

minutes <- hours * 60
minutes

# A vector is a sequence of values. The c() function combines values into one vector. Put commas between the values. We explain functions in the next step.

sleep <- c(6, 7, 8)
sleep

# 7. Use a function and ask for help
# A function is a named instruction that performs a task. For example, mean() calculates an arithmetic average.

# First run sleep <- c(6, 7, 8) again. Then run the next line. The function name comes before the parentheses; the input you give it goes inside them. Inputs and settings are called arguments.

sleep <- c(6, 7, 8)
mean(sleep)

# Functions often have optional settings. Separate arguments with commas. A setting is written as its name, then =, then its value. Compare the next two lines:

round(7.6667)
round(7.6667, digits = 2)

# round() rounds a number. Without specifying digits, it rounds to a whole number: 8. With digits = 2, it gives 7.67. Here = supplies a setting inside the function; <- stores a result under an object name.

average_sleep <- mean(sleep)
average_sleep

# This combines two ideas: first calculate mean(sleep), then store the answer as average_sleep. You do not need to write your own functions to use existing ones.

# Reference only (run separately if needed):
# ?mean
# help("mean")

# Use ?mean without parentheses after mean. help("mean") does the same thing. Typing just mean without parentheses shows the function definition, not an average. We will use na.rm after importing a missing value in step 10.

# 8. Add functions with packages
# Some functions, such as mean(), come with R. A package provides extra functions. We need readr for the CSV import menu used next.

# Install readr once on this computer by running the next line in the Console. Installation needs an internet connection and may take a little time. Wait for the > prompt to return. If readr is already installed, skip this line.

# Reference only (run separately if needed):
# install.packages("readr")

# You can also use Tools → Install Packages, type readr in the package box, and choose Install. If a university computer prevents installation, ask your course support for help. Do not repeatedly click Install while it is working.

library(readr)

# library(readr) loads that package for the current R session. Keep this line in your script and run it again after restarting R. Installing and loading are different steps. The package name is quoted in install.packages("readr"); library(readr) accepts the bare package name in this special form.

# The two colons, ::, identify the package a function belongs to. The dots in the table are a placeholder, not code to type. Later the app’s scripts may use this notation to make their source explicit.

# 9. Import data using the RStudio menu
# Import means reading a saved data file into R’s memory so that you can work with it. We will import six fictional participants.

# Keep the r-practice project open. Its data folder contains practice.csv. CSV means comma-separated values: a plain-text table with commas between columns. It is a data file, not an R script.

# The import menu writes R code and runs it for you. Importing does not automatically save those instructions in your script. If the copied code contains a long path to your Downloads folder, replace that quoted path with "data/practice.csv" after placing the file inside the project’s data folder.

# For the supplied practice file, the following saved import code reproduces the import. Keep one working import block in your script. This block also saves the original imported table as raw_data before making a working copy called study.

library(readr)
raw_data <- read_csv("data/practice.csv", show_col_types = FALSE)
study <- raw_data
study

# The printed table may be labelled a tibble. This is a kind of data table. It has rows and columns, like a data frame. Import messages are not necessarily errors: read them before deciding something went wrong.

# 10. Read a table, select a column and handle missing values
# An R object can contain a whole table. A column is one variable recorded for every row.

names(study)
nrow(study)
ncol(study)

# names() gives the column names. nrow() counts rows: 6. ncol() counts columns: 4. To view the table in RStudio, click study in Environment, or run View(study) with a capital V. The viewer is for inspecting data; closing its tab does not remove study.

# The dollar sign selects a column by name from a table. Read study$sleep_hours as “the sleep_hours column inside study”. The $ does not mean money or multiplication.

study$sleep_hours

# The result is a vector, just like sleep earlier. You can give that column to a function. Read the following instruction from the inside out: first select sleep_hours from study, then calculate its mean.

mean(study$sleep_hours)

# The result is NA, not a numerical mean. R cannot calculate this mean using all six values because one is unknown. NA means a missing value; it does not mean zero. You must decide how missing observations should be handled.

mean(study$sleep_hours, na.rm = TRUE)

# The setting na.rm = TRUE asks this function to leave out missing values for this calculation. TRUE means “yes” and FALSE means “no”; both use capital letters and have no quotes. This gives 7 hours from the five recorded values. It does not delete any rows from study.

is.na(study$sleep_hours)

# is.na() checks each value: TRUE marks a missing value and FALSE a recorded value. Only the third is missing.

sum(is.na(study$sleep_hours))

# This nests one function inside another. First is.na() produces the TRUE/FALSE values; then sum() adds them, counting TRUE as 1 and FALSE as 0. There is 1 missing sleep value. Report the mean together with its denominator: five recorded values and one missing value.

# 11. Read a pipe before using one
# The pipe, |>, connects steps. Read it as “take this, then …”. It is a vertical bar followed by a greater-than sign, with no space between them.

# You already know the following command. Run it first, then run the version underneath. They give the same answer:

mean(study$sleep_hours, na.rm = TRUE)
study$sleep_hours |> mean(na.rm = TRUE)

# In these examples, the pipe supplies its left-hand result as the first input to the function on its right. You do not write the same data again inside those parentheses. A pipe does not save a result by itself: use <- to keep it under a name.

average_hours <- study$sleep_hours |> mean(na.rm = TRUE)
average_hours

# The app uses |> so that preparation steps can be read from top to bottom. Older scripts may use %>% for a similar workflow; %>% comes from a package, while |> is built into R 4.1 and newer. Use |> consistently in this guide.

# For the next steps, install dplyr and ggplot2 once if needed. dplyr supplies data-preparation functions; ggplot2 makes figures. Then run the library lines in each new session.

# Reference only (run separately if needed):
# install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

# 12. Choose rows and columns
# filter() chooses rows. select() chooses columns. Neither changes study unless you assign its result back to study.

# as.data.frame() in the examples prints our small table in a simple rectangular form. It does not change the values. An exclamation mark means NOT: !is.na(sleep_hours) is true for recorded values.

# For example, select(study, id, age, sleep_hours) is the same as study |> select(id, age, sleep_hours). dplyr functions know which table you supplied, so use its column names inside select(), filter() and mutate() without writing study$ each time.

selected_columns <- study |> select(id, age, sleep_hours)
as.data.frame(selected_columns)
older_walkers <- study |>
  filter(age >= 30, programme == "Walking programme")
as.data.frame(older_walkers)

# The comma between conditions means AND: keep participants aged at least 30 who are in the Walking programme. Participants 2 and 6 meet both conditions.

outer_ages <- study |> filter(age < 25 | age > 45)
outer_ages$id

# The result is participants 1 and 5. In a condition, | means OR. With two conditions separated by a comma inside filter(), both must be true. The double equals sign == compares values; it is different from <- for assignment and = for a function setting.

# 13. Create a variable and keep the original
# mutate() adds a column or changes an existing one. Use a new name when you want to keep the original measurement.

study <- study |>
  mutate(sleep_minutes = sleep_hours * 60,
         age_group = if_else(age >= 35, "35 or older", "Under 35"))
as.data.frame(study |> select(id, sleep_hours, sleep_minutes, age_group))

# Inside mutate(), sleep_minutes = ... names the new column. if_else() makes a choice for each row: test age >= 35; if true use the text “35 or older”, otherwise use “Under 35”. Neither label is a number.

# Six hours becomes 360 minutes. A missing sleep value stays missing. The age-group boundary is a choice made for this exercise; grouping a measurement loses detail and needs a reason in a real analysis.

study <- study |>
  mutate(log_sleep_hours = log(sleep_hours))
round(study$log_sleep_hours, 3)

# This shows the syntax of the natural log. It is not a recommendation to transform these sleep data. Logarithms require strictly positive values and change the measurement scale. In the app, look at the distribution first and use its log tutorial to understand the choice.

# 14. Count categories and summarise measurements
# A summary table contains fewer rows than the original data: one overall row, or one row for each group.

# Read the next block one line at a time: take study; keep recorded programme values; count the rows in each programme; then turn those counts into percentages. count() creates a count column named n. The sum(n) denominator is the total count.

counts <- study |>
  filter(!is.na(programme)) |>
  count(programme, name = "n") |>
  mutate(percent = 100 * n / sum(n))
as.data.frame(counts)

# Three of the six participants are in each programme: 50% in Usual routine and 50% in Walking programme. These percentages use participants with a recorded programme as their denominator.

# group_by(programme) tells R to perform the following calculations separately for each programme. summarise() creates one result row per group. Each name before =, such as mean_hours, names a column in that result. sd() calculates the standard deviation; the app’s descriptive tutorial explains that measure of spread.

sleep_summary <- study |>
  group_by(programme) |>
  summarise(
    rows = n(),
    observed = sum(!is.na(sleep_hours)),
    missing = sum(is.na(sleep_hours)),
    mean_hours = mean(sleep_hours, na.rm = TRUE),
    sd_hours = sd(sleep_hours, na.rm = TRUE),
    .groups = "drop"
  )
as.data.frame(sleep_summary)

# In Usual routine, two sleep values are recorded and one is missing. Their mean is 6.25 hours (SD 0.35 hours). In Walking programme, all three values are recorded; their mean is 7.50 hours (SD 0.50 hours). These small fictional groups illustrate the code, rather than evidence of a programme effect.

# 15. Make a figure
# Start with a data table, choose the axes, then choose how observations are drawn.

# ggplot2 uses layers. First, ggplot() specifies the table. Inside it, aes() connects variables to the horizontal x axis and vertical y axis. geom_point() then draws a dot for each recorded observation. Read + here as “add this plotting layer”, not as a pipe.

sleep_plot <- ggplot(study, aes(x = programme, y = sleep_hours)) +
  geom_point(na.rm = TRUE)
sleep_plot

# The complete instruction covers two lines. Highlight both lines that create sleep_plot and click Run, then run sleep_plot to display the figure. Look in the Plots tab, usually at the bottom right. The + at the end of the first script line says that the instruction continues on the next line.

# Now add axis labels and a simple appearance. labs() supplies text labels; theme_minimal() changes appearance, not data. Assigning back to sleep_plot keeps the updated figure.

sleep_plot <- sleep_plot +
  labs(x = "Programme", y = "Sleep (hours)",
       title = "Recorded sleep in the practice data") +
  theme_minimal()
sleep_plot

# Each dot is one recorded sleep value. The missing value is absent from the figure but remains counted in the summary table. This figure uses five observations. In RStudio, use Zoom in the Plots pane if labels look cramped.

# 16. Save your work and use the app’s script
# Keep your script alongside its data. A saved figure or table is useful, but the script records how it was produced.

# First save your script with File → Save. To save results too, dir.create() makes an output folder; write_csv() writes a table to disk; saveRDS() stores an R object; ggsave() saves the figure. The image size below is 7 by 4.5 inches at 300 dots per inch. showWarnings = FALSE hides the harmless message if the output folder already exists.

dir.create("output", showWarnings = FALSE)
readr::write_csv(sleep_summary, "output/sleep_summary.csv")
saveRDS(study, "output/prepared_study.rds")
ggsave("output/sleep_plot.png", sleep_plot,
       width = 7, height = 4.5, dpi = 300)

# The output folder now contains the table, the prepared dataset and the figure. Running these lines again replaces those demonstration output files. readRDS("output/prepared_study.rds") reads the saved R object, including its column types.

# When closing RStudio, save your .R script. If separately asked to save the workspace image, you can choose Don’t Save for this exercise: rerunning the saved script recreates your objects. Saving a script and saving a workspace are different actions.

# Optional video: Data visualization
# Watch a narrated explanation of data visualization. You can use this alongside the written plotting lesson, or return to it after your first analysis.

# The video lasts about 36 minutes and 34 seconds. If shown, choose Load video, then press Play to hear the narration. You can pause to look at a figure, move back to hear an explanation again, or use the fullscreen button to read the slides more easily. There is no need to watch it all at once.

# As you watch, ask yourself: what does each axis show? What can I learn from the figure? Could a reader understand it without seeing the original data? Then try describing one plot from your own analysis in a sentence.

# Optional reference: vectors, factors and recoding
# Return to this section when a line in a script is unfamiliar. It is not required before using the app.

x <- c(6, 8, 3, 1, 7)
x[c(1, 3, 5)]
x[x > 6]
seq(1, 5, by = 2)
rep(c("A", "B"), each = 2)

# Square brackets select elements. The first selection takes positions 1, 3 and 5; the second keeps values greater than 6. For a data frame, study[rows, columns] selects rows and columns; leaving one side blank keeps all of that dimension.

# A factor represents categories with a specified set of labels called levels. factor() creates it; levels() shows the labels. Avoid as.numeric() on a factor: it gives internal codes, not necessarily the numbers in its labels.

ratings <- factor(c("Good", "Poor", "Average"),
  levels = c("Poor", "Average", "Good"))
levels(ratings)
ratings_recoded <- factor(
  if_else(as.character(ratings) == "Poor", "Negative", "Other"),
  levels = c("Negative", "Other")
)
ratings_recoded

# Here the levels set the display order. ordered = TRUE would additionally declare an ordered factor and can change how a model treats it. For recoding, we create a new object and explicitly define its categories. Assigning an unrecognised label directly into a factor can introduce missing values.

# Optional reference: long and wide data
# Decide what one row should represent before changing the shape of a table. Repeated measurements need an identifier connecting observations from the same person.

# This optional example requires tidyr. Install it first with install.packages("tidyr"). The tidyr:: prefix selects the function without a library() call. data.frame() makes a table from named columns; 1:2 means the sequence 1, 2.

wide <- data.frame(id = 1:2, sleep_day1 = c(6, 7),
                   sleep_day2 = c(6.5, 7.5))
long <- tidyr::pivot_longer(wide,
  cols = starts_with("sleep_"), names_to = "day",
  names_prefix = "sleep_", values_to = "sleep_hours")
as.data.frame(long)
wide_again <- tidyr::pivot_wider(long,
  names_from = day, values_from = sleep_hours,
  names_prefix = "sleep_")
as.data.frame(wide_again)

# In wide, one row represents a participant. In long, one row represents a participant on a particular day. The values are unchanged. pivot_longer() and pivot_wider() are the current alternatives to gather() and spread() in older material. Reshaping data does not make repeated observations independent.

# Optional reference: combine tables carefully
# A join adds information by matching a key, such as a participant ID. Check the keys before joining so rows are not unexpectedly duplicated.

site_lookup <- data.frame(
  id = 1:6,
  site = c("A", "A", "B", "B", "A", "B")
)
stopifnot(!anyDuplicated(site_lookup$id))
combined <- left_join(study, site_lookup, by = "id")
as.data.frame(combined |> select(id, programme, site))
nrow(combined)

# stopifnot() stops if a check is false. anyDuplicated() finds duplicate keys; ! turns the zero/no-duplicate result into TRUE. You can learn joins later; they are not needed for your first analysis.

# left_join() keeps all rows from study and adds matching site information. The check stops this example if an ID occurs more than once in the lookup table. Here six participants remain six rows. An unmatched ID would have a missing site; duplicated matching keys can multiply rows.

# When something goes wrong
# Read the first error, check the line that caused it, and change one thing at a time.

# Reference only (run separately if needed):
# ?mean
# help(package = "dplyr")
# citation()
# citation("ggplot2")

# The question mark goes before the function name. citation() gives the R reference and citation("ggplot2") gives the package reference for reports or manuscripts.
