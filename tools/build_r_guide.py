"""Build the beginner guide and run its examples in R. No web assets required."""
from pathlib import Path
import csv, html, json, subprocess, tempfile, base64, zipfile, sys
root=Path(__file__).resolve().parents[1]
out=root/'inst/app/www/r-guide';out.mkdir(parents=True,exist_ok=True);(out/'data').mkdir(exist_ok=True)
with (out/'data/practice.csv').open('w') as f:
 w=csv.writer(f);w.writerow(['id','age','programme','sleep_hours']);w.writerows([[1,22,'Usual routine',6],[2,35,'Walking programme',7],[3,42,'Usual routine',''],[4,28,'Walking programme',8],[5,51,'Usual routine',6.5],[6,37,'Walking programme',7.5]])
sections=[];current=None

def section(key,title,intro,optional=False):
 global current
 current={'id':key,'title':title,'intro':intro,'optional':optional,'blocks':[]};sections.append(current)
def add(kind,**kwargs):current['blocks'].append({'kind':kind,**kwargs})
def p(text):add('p',text=text)
def note(title,text):add('note',title=title,text=text)
def code(text,run=True,plot=False,label=None):add('code',text=text.strip(),run=run,plot=plot,label=label)
def table(headers,rows):add('table',headers=headers,rows=rows)
def steps(items):add('steps',items=items)
def links(items):add('links',items=items)
def shot(key,alt,caption,optional=False):add('shot',key=key,alt=alt,caption=caption,optional=optional)
def check(title,question,answer):add('check',title=title,question=question,answer=answer)

section('start','1. Open RStudio','Begin here even if you have never written a computer instruction. You only need to follow one small step at a time.')
p('R is the program that does the calculations. RStudio is the application in which you write instructions for R and see the results. We use RStudio throughout this guide. You do not need to open a separate R window.')
steps(['If R is not installed, use the Download R link below, choose your operating system, and follow the installer.','Install RStudio Desktop using the second link. Choose the free Desktop edition. If both programs are already installed, you can skip installation.','Open RStudio. Keep it open beside this guide, or switch between the two windows.','Choose Next step at the bottom of this page. You do not need to download or install an R package yet.'])
links([('Download R','https://cran.r-project.org/'),('Download RStudio Desktop','https://posit.co/download/rstudio-desktop/')])
note('How to use this guide','Each step tells you where to click, what to type and what you should see. Code boxes can be expanded. Type or copy only the code, not the Output underneath. Work through steps 1–10 first; preparation and pipes come afterwards. If you already know the basics, use Choose a step to jump ahead.')
check('Before moving on','Which application should be open?','RStudio. R works inside it to carry out your instructions.')

section('screen','2. Find your way around the screen','A pane is one area of the RStudio window. Tabs let the same pane show different things.')
p('The screenshot below comes from an older RStudio version. Colours and extra tabs may differ on your computer. The important names are Source, Console, Environment, Files, Plots and Help. If there is no editor at the top left yet, that is normal: creating a script in step 4 opens it.')
shot('panes','RStudio with a blank Source editor at top left, Console at bottom left, Environment at top right, and Files, Plots and Help tabs at bottom right.','Locate the four areas. Use Enlarge screenshot to read the tab names.')
table(['Usually where?','Name','What you do there'],[['Top left','Source / script editor','Write instructions you want to keep. This pane appears when a script is open.'],['Bottom left','Console','R runs instructions here and displays answers or messages.'],['Top right','Environment','See named items currently in R’s memory, such as a dataset.'],['Bottom right','Files','Browse the files on your computer in the current folder.'],['Bottom right','Plots','See figures made by R.'],['Bottom right','Help','Read instructions for a function.'],['Bottom right','Packages','See installed packages. We explain packages in step 8.']])
p('A script tab and a data-viewer tab can share the Source pane. Clicking the tab changes what you see; it does not delete the other tab. The Environment and Files are different: an item in memory is not automatically a file saved on your computer.')
steps(['Find the Console label. Make sure you are on Console, not Terminal.','Find the Environment tab and its Import Dataset button.','Click Files, then Help. Both are usually in the lower-right pane.'])
check('Before moving on','Where would you look for a calculation’s answer? And for a saved file?','Answers appear in the Console. Saved files appear in Files. An object in the Environment is a temporary item in memory.')

section('project','3. Make a project folder','An RStudio project keeps the files for one piece of work together. A folder is also called a directory.')
p('First download the practice files using the button below. A ZIP is a compressed folder: you must extract it before working with its contents. On macOS, double-click it in Finder. On Windows, right-click it in File Explorer and choose Extract All. Move the extracted folder somewhere easy to find, such as Documents, and name the folder r-practice.')
links([('Download practice files','practice.zip')])
steps(['In RStudio, click File → New Project.','Choose Existing Directory, because the extracted practice folder already exists.','Click Browse and select the r-practice folder itself, containing practice.R and the data folder. Do not select the ZIP or the data subfolder.','Click Create Project. The project name should now appear near the top-right corner of RStudio.','Click the Files tab. You should see practice.R, a data folder and a file ending in .Rproj.'])
note('If you are starting your own work later','Use File → New Project → New Directory → New Project, enter a folder name and choose where to put it. For this exercise use Existing Directory, so R can find the supplied data.')
table(['Item','What it contains','What opening it does'],[['r-practice folder','Your scripts, data and saved results','Shows the files together in Finder or File Explorer.'],['r-practice.Rproj','RStudio settings and the link to this project folder','Opens the project in RStudio and makes its folder the starting place for file paths.'],['my_first_script.R','Plain text containing the R instructions you write','Opens instructions in the editor. Opening a script does not run it.'],['data/practice.csv','The practice table stored as a comma-separated text file','Stores observations. It needs to be imported before R can analyse it.']])
p('One project can contain several scripts. The .Rproj file does not contain all your code or data. If you share your work, send the relevant folder and its files, not just the .Rproj file. Next time, double-click the .Rproj file, or choose File → Open Project in RStudio.')
note('Where R looks for files','With this project open, data/practice.csv means “inside the project folder, open the data folder, then find practice.csv”. The / separates folders and works in R paths on both Windows and macOS. The project sets this starting folder for you; no setwd() command is needed for this exercise.')
check('Before moving on','Which file saves your instructions: .Rproj or .R?','The .R script saves your instructions. The .Rproj file opens and organises the project in RStudio. Save both inside the same project folder.')

section('script','4. Create and save an R script','A script is a text file containing instructions for R, saved so that you can repeat or change your work later.')
steps(['In RStudio, click File → New File → R Script.','A blank tab, usually called Untitled1, opens in the Source pane. Click on line 1 in that editor.','Type the two lines below, or expand the code box and copy them.','Choose File → Save As. Save inside the r-practice project folder with the name my_first_script.R.','The tab should now show my_first_script.R. Keep this script open and add the later examples underneath it.'])
code('# My first R script\n2 + 3',run=False,label='Type these two lines into your new script')
p('The # starts a comment: a note for people reading the script. R ignores the rest of that line. The second line asks R to add 2 and 3. Saving stores these instructions on disk; it does not carry out the calculation.')
note('Your script and the supplied script','Write in my_first_script.R as you learn. The downloaded practice.R is a completed example you can consult later. You do not need to understand or run that whole file now. An asterisk beside a script’s tab name indicates unsaved edits.')

section('run-code','5. Run one instruction and read its answer','Running code means asking R to carry out the instructions you wrote.')
steps(['Click anywhere on the 2 + 3 line in your script.','Click Run near the top-right of the Source pane. Do not click Source for this first exercise.','Look at the Console: the command appears there, followed by its answer.','Change 3 to 8 in the script. Click Run on that line again. You should now see 10.','Choose File → Save to keep your change.'])
code('2 + 3')
p('The answer is 5. The printed [1] means that the first displayed value is item 1; it is not part of the answer. You may see > before a command in the Console. That is R’s ready prompt, not something to copy into your script.')
shot('run','RStudio showing two selected lines in the Source pane, the Run button, an object in Environment and its value in the Console.','This original example uses x and some text. Follow the movement: script at top left → Run → commands and output in the Console. We explain named objects next.')
table(['Action','What happens'],[['Press Enter while typing in the script','Starts a new line in the script; it does not run the previous line.'],['Click Run with the cursor on a line','Runs the current line or complete expression.'],['Highlight several complete lines, then click Run','Runs those lines in order.'],['Click Source','Runs the entire script. Bare results may not print; use Run while learning.'],['Click File → Save','Saves your text. It does not run the script.']])
p('Optional shortcut: Control + Enter on Windows or Command + Enter on macOS runs the current line or selected code. You can keep using the Run button instead.')
note('If you see a + prompt in the Console','R is waiting for the rest of an incomplete instruction, such as a closing bracket or quote. Press Esc to cancel, fix the line in your script, then run the complete line again. A + prompt is not a result and does not mean R is busy calculating.')
shot('unfinished','RStudio Console showing the unfinished command 2*5+ and the continuation prompt +.','The Console is waiting for what comes after the final plus sign. Press Esc to return to the > prompt.')
check('Try it','Type 12 / 3 on a new script line. Predict the answer, then click Run.','The answer is 4. / means division. Other arithmetic symbols are + for addition, - for subtraction, * for multiplication and ^ for a power.')

section('objects','6. Give a value a name','An object is a named item in R’s memory. It can hold one number, several values, a table or a result.')
p('Type and run the first line below. The <- symbol is a less-than sign followed by a hyphen. Read it as “store the value on the right under the name on the left”. Then run the second line to display that value.')
code('hours <- 7\nhours')
p('The first line creates hours in the Environment and usually prints no answer. The second line asks R to show what hours contains: 7. You choose the name hours; it is not a built-in keyword.')
code('hours * 60\nhours')
p('The calculation gives 420 minutes, but hours still contains 7. To keep a calculation’s result, assign it a name:')
code('minutes <- hours * 60\nminutes')
p('A vector is a sequence of values. The c() function combines values into one vector. Put commas between the values. We explain functions in the next step.')
code('sleep <- c(6, 7, 8)\nsleep')
table(['Notation','Meaning'],[['hours <- 7','Store 7 as hours. R evaluates the right side first.'],['sleep','Look up the object called sleep.'],['"sleep"','Literal text: the word sleep. Quotes make it text.'],['"Walking programme"','A category label containing text and a space.'],['sleep_hours','One possible name; the underscore is part of the name.'],['sleep and Sleep','Different names: R is case-sensitive.']])
note('A useful naming habit','Use short names starting with a letter, with underscores instead of spaces. Spell a name exactly the same each time. Assignment replaces an existing object with that name, so use a new name if you want to keep the original.')
check('Try it','Change the third value in sleep from 8 to 10. Run the assignment again, then run sleep.','R displays 6, 7 and 10. Editing the script alone does not update the object: you must run the changed line. Run sleep <- c(6, 7, 8) again before continuing, so your values match the next example.')

section('functions','7. Use a function and ask for help','A function is a named instruction that performs a task. For example, mean() calculates an arithmetic average.')
p('First run sleep <- c(6, 7, 8) again. Then run the next line. The function name comes before the parentheses; the input you give it goes inside them. Inputs and settings are called arguments.')
code('sleep <- c(6, 7, 8)\nmean(sleep)')
table(['Part of mean(sleep)','Meaning'],[['mean','Which task to perform: calculate the average.'],['( and )','Enclose the inputs supplied to that function.'],['sleep','The object containing the values to average.'],['The result, 7','The average of 6, 7 and 8.']])
p('Functions often have optional settings. Separate arguments with commas. A setting is written as its name, then =, then its value. Compare the next two lines:')
code('round(7.6667)\nround(7.6667, digits = 2)')
p('round() rounds a number. Without specifying digits, it rounds to a whole number: 8. With digits = 2, it gives 7.67. Here = supplies a setting inside the function; <- stores a result under an object name.')
code('average_sleep <- mean(sleep)\naverage_sleep')
p('This combines two ideas: first calculate mean(sleep), then store the answer as average_sleep. You do not need to write your own functions to use existing ones.')
steps(['In the Console, type ?mean and press Enter. Alternatively, type it in your script and click Run.','Look at the Help tab, usually at the bottom right. It should show the mean help page.','Read Description to see what the function does.','Find Usage and Arguments. x means the values you supply; na.rm is an optional setting for missing values.','Find Examples near the bottom. You can copy an example into your script and run it. You do not need to understand every technical detail on the page.'])
code('?mean\nhelp("mean")',run=False,label='Two ways to open the same Help page')
p('Use ?mean without parentheses after mean. help("mean") does the same thing. Typing just mean without parentheses shows the function definition, not an average. We will use na.rm after importing a missing value in step 10.')
check('Try it','Run length(sleep), then open ?length. What is counted?','The answer is 3. length() counts the values in the vector. It does not add them or calculate their average.')

section('packages','8. Add functions with packages','Some functions, such as mean(), come with R. A package provides extra functions. We need readr for the CSV import menu used next.')
p('Install readr once on this computer by running the next line in the Console. Installation needs an internet connection and may take a little time. Wait for the > prompt to return. If readr is already installed, skip this line.')
code('install.packages("readr")',run=False,label='Run once if readr is not installed')
p('You can also use Tools → Install Packages, type readr in the package box, and choose Install. If a university computer prevents installation, ask your course support for help. Do not repeatedly click Install while it is working.')
code('library(readr)')
p('library(readr) loads that package for the current R session. Keep this line in your script and run it again after restarting R. Installing and loading are different steps. The package name is quoted in install.packages("readr"); library(readr) accepts the bare package name in this special form.')
table(['Command','Purpose','When?'],[['install.packages("readr")','Put the package on your computer','Usually once per R installation, with later updates as needed.'],['library(readr)','Make its functions available in this session','Each new R session that uses those functions.'],['readr::read_csv(...)','Use read_csv specifically from readr','An alternative to loading the package first; it must still be installed.']])
p('The two colons, ::, identify the package a function belongs to. The dots in the table are a placeholder, not code to type. Later the app’s scripts may use this notation to make their source explicit.')

section('import','9. Import data using the RStudio menu','Import means reading a saved data file into R’s memory so that you can work with it. We will import six fictional participants.')
p('Keep the r-practice project open. Its data folder contains practice.csv. CSV means comma-separated values: a plain-text table with commas between columns. It is a data file, not an R script.')
shot('import-menu','RStudio Import Dataset menu with From Text (readr) and From Excel options.','Use Import Dataset in the Environment pane. This menu screenshot is from the Posit documentation.')
steps(['In the Environment pane, click Import Dataset → From Text (readr). If prompted to install readr, let installation finish.','In the import window, click Browse. Open your r-practice folder, then data, and select practice.csv.','Inspect the preview: it should show four separate columns named id, age, programme and sleep_hours. Make sure the first row supplies column names, rather than being treated as a participant.','Set the imported object’s Name to study. For this practice file, use a comma delimiter and a decimal point. The empty sleep cell is a missing value.','Find Code Preview. Select and copy its code into your script under a comment such as # Import the practice data. This preserves what the menu is doing.','Click Import. In Environment, study should now appear as 6 observations of 4 variables. Click its name to inspect the table.'])
shot('import-csv','RStudio Import Text Data dialog showing file selection, data preview, import options and Code Preview.','This Posit screenshot uses a different example file. On your screen, select practice.csv and set Name to study. Use it to locate Preview, options and Code Preview; do not copy its example filename.')
p('The import menu writes R code and runs it for you. Importing does not automatically save those instructions in your script. If the copied code contains a long path to your Downloads folder, replace that quoted path with "data/practice.csv" after placing the file inside the project’s data folder.')
p('For the supplied practice file, the following saved import code reproduces the import. Keep one working import block in your script. This block also saves the original imported table as raw_data before making a working copy called study.')
code('library(readr)\nraw_data <- read_csv("data/practice.csv", show_col_types = FALSE)\nstudy <- raw_data\nstudy')
table(['Part','Meaning'],[['read_csv(...)','Read a comma-separated table from a file.'],['"data/practice.csv"','The path from your project folder to this data file. Quotes mark it as text.'],['show_col_types = FALSE','Hide the informational message about inferred column types. It does not change the imported values.'],['raw_data <- ...','Store the imported table under the name raw_data.'],['study <- raw_data','Make study the working copy used in the following exercises.']])
p('The printed table may be labelled a tibble. This is a kind of data table. It has rows and columns, like a data frame. Import messages are not necessarily errors: read them before deciding something went wrong.')
note('Check before analysing','There should be six participants and four columns. id identifies a participant; age is in years; programme is a category; sleep_hours is a measurement in hours. Participant 3 has no recorded sleep value. If everything appears in one column, check the delimiter. If numerical measurements appear as text, check decimal marks and unexpected characters before changing the column type.')
add('extra',title='If your file is Excel instead',text='This is an alternative for your own data; use the CSV above for this exercise.',items=['Install readxl if RStudio asks for it.','Choose Import Dataset → From Excel, then Browse to the .xlsx file.','Select the worksheet containing your table. Inspect the preview and check which row contains column names. Set the Name you want.','Copy Code Preview into your script, then click Import. A worksheet is the named tab inside the Excel workbook.'])
shot('import-excel','RStudio Excel import window with worksheet selection, preview and Code Preview.','Optional Excel example from Posit. Its file and skip-row settings are specific to that example; inspect your own workbook rather than copying them.',optional=True)
check('Before moving on','Where are the imported data now, and where is the reusable import instruction?','The data object study is in Environment. The import instruction should be in your saved .R script. Next time, run that instruction again to recreate the data object.')

section('columns','10. Read a table, select a column and handle missing values','An R object can contain a whole table. A column is one variable recorded for every row.')
code('names(study)\nnrow(study)\nncol(study)')
p('names() gives the column names. nrow() counts rows: 6. ncol() counts columns: 4. To view the table in RStudio, click study in Environment, or run View(study) with a capital V. The viewer is for inspecting data; closing its tab does not remove study.')
p('The dollar sign selects a column by name from a table. Read study$sleep_hours as “the sleep_hours column inside study”. The $ does not mean money or multiplication.')
code('study$sleep_hours')
table(['Part','Meaning'],[['study','The object containing the whole table.'],['$','Select one named column from that table.'],['sleep_hours','The exact column name, as shown by names(study).'],['6, 7, NA, 8, 6.5, 7.5','The six values in that column. NA marks the missing value.']])
p('The result is a vector, just like sleep earlier. You can give that column to a function. Read the following instruction from the inside out: first select sleep_hours from study, then calculate its mean.')
code('mean(study$sleep_hours)')
p('The result is NA, not a numerical mean. R cannot calculate this mean using all six values because one is unknown. NA means a missing value; it does not mean zero. You must decide how missing observations should be handled.')
code('mean(study$sleep_hours, na.rm = TRUE)')
p('The setting na.rm = TRUE asks this function to leave out missing values for this calculation. TRUE means “yes” and FALSE means “no”; both use capital letters and have no quotes. This gives 7 hours from the five recorded values. It does not delete any rows from study.')
code('is.na(study$sleep_hours)')
p('is.na() checks each value: TRUE marks a missing value and FALSE a recorded value. Only the third is missing.')
code('sum(is.na(study$sleep_hours))')
p('This nests one function inside another. First is.na() produces the TRUE/FALSE values; then sum() adds them, counting TRUE as 1 and FALSE as 0. There is 1 missing sleep value. Report the mean together with its denominator: five recorded values and one missing value.')
note('Numbers, text and categories','A numerical column contains amounts, such as sleep hours. A character column contains text, such as programme labels. A factor is another way R represents categories; we introduce it later. A numeric participant ID is still an identifier, not a measurement that needs a mean. Quotes around "7" make it text, which is different from the number 7.')
check('Try it','Select study$age and use mean() to calculate its average. What do you expect about missing values?','The ages are 22, 35, 42, 28, 51 and 37, with none missing. mean(study$age) gives about 35.83 years. You can use ?mean to revisit what na.rm does.')

section('pipes','11. Read a pipe before using one','The pipe, |>, connects steps. Read it as “take this, then …”. It is a vertical bar followed by a greater-than sign, with no space between them.')
p('You already know the following command. Run it first, then run the version underneath. They give the same answer:')
code('mean(study$sleep_hours, na.rm = TRUE)\nstudy$sleep_hours |> mean(na.rm = TRUE)')
table(['Without a pipe','With a pipe','Read it as'],[['mean(values)','values |> mean()','Take values, then calculate their mean.'],['mean(study$sleep_hours, na.rm = TRUE)','study$sleep_hours |> mean(na.rm = TRUE)','Take the sleep column, then calculate its mean using recorded values.']])
p('In these examples, the pipe supplies its left-hand result as the first input to the function on its right. You do not write the same data again inside those parentheses. A pipe does not save a result by itself: use <- to keep it under a name.')
code('average_hours <- study$sleep_hours |> mean(na.rm = TRUE)\naverage_hours')
p('The app uses |> so that preparation steps can be read from top to bottom. Older scripts may use %>% for a similar workflow; %>% comes from a package, while |> is built into R 4.1 and newer. Use |> consistently in this guide.')
note('Two different vertical-bar symbols','|> passes a result to the next function. A single | means OR in a condition, which we introduce in the next step. In ggplot code later, + adds a plotting layer. These symbols do different jobs.')
p('For the next steps, install dplyr and ggplot2 once if needed. dplyr supplies data-preparation functions; ggplot2 makes figures. Then run the library lines in each new session.')
code('install.packages(c("dplyr", "ggplot2"))',run=False,label='Install once if these packages are missing')
code('library(dplyr)\nlibrary(ggplot2)')
check('Before moving on','How would you read study |> head()?','Take study, then show its first rows using head(). This is equivalent to head(study). Run either version to try it.')

section('filter','12. Choose rows and columns','filter() chooses rows. select() chooses columns. Neither changes study unless you assign its result back to study.')
p('as.data.frame() in the examples prints our small table in a simple rectangular form. It does not change the values. An exclamation mark means NOT: !is.na(sleep_hours) is true for recorded values.')
p('For example, select(study, id, age, sleep_hours) is the same as study |> select(id, age, sleep_hours). dplyr functions know which table you supplied, so use its column names inside select(), filter() and mutate() without writing study$ each time.')
code('selected_columns <- study |> select(id, age, sleep_hours)\nas.data.frame(selected_columns)\nolder_walkers <- study |>\n  filter(age >= 30, programme == "Walking programme")\nas.data.frame(older_walkers)')
p('The comma between conditions means AND: keep participants aged at least 30 who are in the Walking programme. Participants 2 and 6 meet both conditions.')
table(['Condition','Meaning'],[['age >= 30','At least 30 years old'],['programme == "Usual routine"','Equals this category label'],['programme != "Usual routine"','Does not equal this label'],['age < 25 | age > 45','Below 25 OR above 45'],['id %in% c(1, 3, 5)','ID is one of the listed values'],['!is.na(sleep_hours)','Sleep has a recorded value']])
code('outer_ages <- study |> filter(age < 25 | age > 45)\nouter_ages$id')
p('The result is participants 1 and 5. In a condition, | means OR. With two conditions separated by a comma inside filter(), both must be true. The double equals sign == compares values; it is different from <- for assignment and = for a function setting.')
note('Check after filtering','An ordinary comparison with a missing value does not evaluate to TRUE, so filter() drops that row. If missing values should be retained, include is.na() explicitly. Always check how many rows remain.')

section('mutate','13. Create a variable and keep the original','mutate() adds a column or changes an existing one. Use a new name when you want to keep the original measurement.')
code('study <- study |>\n  mutate(sleep_minutes = sleep_hours * 60,\n         age_group = if_else(age >= 35, "35 or older", "Under 35"))\nas.data.frame(study |> select(id, sleep_hours, sleep_minutes, age_group))')
p('Inside mutate(), sleep_minutes = ... names the new column. if_else() makes a choice for each row: test age >= 35; if true use the text “35 or older”, otherwise use “Under 35”. Neither label is a number.')
p('Six hours becomes 360 minutes. A missing sleep value stays missing. The age-group boundary is a choice made for this exercise; grouping a measurement loses detail and needs a reason in a real analysis.')
code('study <- study |>\n  mutate(log_sleep_hours = log(sleep_hours))\nround(study$log_sleep_hours, 3)')
p('This shows the syntax of the natural log. It is not a recommendation to transform these sleep data. Logarithms require strictly positive values and change the measurement scale. In the app, look at the distribution first and use its log tutorial to understand the choice.')
note('Other useful commands','rename(new_name = old_name) changes a column name. arrange(age) sorts rows by age; arrange(desc(age)) sorts from largest to smallest. Neither sorts the original study object unless you save the result.')

section('summarise','14. Count categories and summarise measurements','A summary table contains fewer rows than the original data: one overall row, or one row for each group.')
p('Read the next block one line at a time: take study; keep recorded programme values; count the rows in each programme; then turn those counts into percentages. count() creates a count column named n. The sum(n) denominator is the total count.')
code('counts <- study |>\n  filter(!is.na(programme)) |>\n  count(programme, name = "n") |>\n  mutate(percent = 100 * n / sum(n))\nas.data.frame(counts)')
p('Three of the six participants are in each programme: 50% in Usual routine and 50% in Walking programme. These percentages use participants with a recorded programme as their denominator.')
p('group_by(programme) tells R to perform the following calculations separately for each programme. summarise() creates one result row per group. Each name before =, such as mean_hours, names a column in that result. sd() calculates the standard deviation; the app’s descriptive tutorial explains that measure of spread.')
code('sleep_summary <- study |>\n  group_by(programme) |>\n  summarise(\n    rows = n(),\n    observed = sum(!is.na(sleep_hours)),\n    missing = sum(is.na(sleep_hours)),\n    mean_hours = mean(sleep_hours, na.rm = TRUE),\n    sd_hours = sd(sleep_hours, na.rm = TRUE),\n    .groups = "drop"\n  )\nas.data.frame(sleep_summary)')
p('In Usual routine, two sleep values are recorded and one is missing. Their mean is 6.25 hours (SD 0.35 hours). In Walking programme, all three values are recorded; their mean is 7.50 hours (SD 0.50 hours). These small fictional groups illustrate the code, rather than evidence of a programme effect.')
note('Read the functions carefully','n() counts rows in a group, including rows with missing sleep. sum(!is.na(sleep_hours)) counts recorded sleep values. summarise() makes summary rows; mutate() keeps the existing rows and adds columns. The .groups = "drop" setting returns an ungrouped summary table.')

section('plot','15. Make a figure','Start with a data table, choose the axes, then choose how observations are drawn.')
p('ggplot2 uses layers. First, ggplot() specifies the table. Inside it, aes() connects variables to the horizontal x axis and vertical y axis. geom_point() then draws a dot for each recorded observation. Read + here as “add this plotting layer”, not as a pipe.')
code('sleep_plot <- ggplot(study, aes(x = programme, y = sleep_hours)) +\n  geom_point(na.rm = TRUE)\nsleep_plot')
p('The complete instruction covers two lines. Highlight both lines that create sleep_plot and click Run, then run sleep_plot to display the figure. Look in the Plots tab, usually at the bottom right. The + at the end of the first script line says that the instruction continues on the next line.')
p('Now add axis labels and a simple appearance. labs() supplies text labels; theme_minimal() changes appearance, not data. Assigning back to sleep_plot keeps the updated figure.')
code('sleep_plot <- sleep_plot +\n  labs(x = "Programme", y = "Sleep (hours)",\n       title = "Recorded sleep in the practice data") +\n  theme_minimal()\nsleep_plot',plot=True)
links([('Optional narrated video: Data visualization','#video')])
p('Each dot is one recorded sleep value. The missing value is absent from the figure but remains counted in the summary table. This figure uses five observations. In RStudio, use Zoom in the Plots pane if labels look cramped.')
check('Try it','Which line would you change to give the figure a different title?','Change the text after title = inside labs(), then rerun the complete plotting instruction. Put your new title inside quotation marks.')

section('save','16. Save your work and use the app’s script','Keep your script alongside its data. A saved figure or table is useful, but the script records how it was produced.')
p('First save your script with File → Save. To save results too, dir.create() makes an output folder; write_csv() writes a table to disk; saveRDS() stores an R object; ggsave() saves the figure. The image size below is 7 by 4.5 inches at 300 dots per inch. showWarnings = FALSE hides the harmless message if the output folder already exists.')
code('dir.create("output", showWarnings = FALSE)\nreadr::write_csv(sleep_summary, "output/sleep_summary.csv")\nsaveRDS(study, "output/prepared_study.rds")\nggsave("output/sleep_plot.png", sleep_plot,\n       width = 7, height = 4.5, dpi = 300)')
p('The output folder now contains the table, the prepared dataset and the figure. Running these lines again replaces those demonstration output files. readRDS("output/prepared_study.rds") reads the saved R object, including its column types.')
p('When closing RStudio, save your .R script. If separately asked to save the workspace image, you can choose Don’t Save for this exercise: rerunning the saved script recreates your objects. Saving a script and saving a workspace are different actions.')
steps(['In the app, run an analysis and expand Show R code.','Choose Script + original data. Download and unzip it.','Create an RStudio project in that folder, then open analysis.R.','Install the listed packages if needed. Run the script from top to bottom.','Match the import, preparation, summary and plotting blocks to what you chose in the app. Save your script after making changes.'])
note('Check that your work can be reproduced','Save the script, restart R using Session → Restart R, and run it from the top. A fresh session helps reveal missing steps. Clearing the Console only clears what you see; it does not remove objects or restart R.')

section('video','Optional video: Data visualization','Watch a narrated explanation of data visualization. You can use this alongside the written plotting lesson, or return to it after your first analysis.',True)
p('The video lasts about 36 minutes and 34 seconds. If shown, choose Load video, then press Play to hear the narration. You can pause to look at a figure, move back to hear an explanation again, or use the fullscreen button to read the slides more easily. There is no need to watch it all at once.')
add('video',src='data-visualization.mp4')
p('As you watch, ask yourself: what does each axis show? What can I learn from the figure? Could a reader understand it without seeing the original data? Then try describing one plot from your own analysis in a sentence.')

section('reference','Optional reference: vectors, factors and recoding','Return to this section when a line in a script is unfamiliar. It is not required before using the app.',True)
code('x <- c(6, 8, 3, 1, 7)\nx[c(1, 3, 5)]\nx[x > 6]\nseq(1, 5, by = 2)\nrep(c("A", "B"), each = 2)')
p('Square brackets select elements. The first selection takes positions 1, 3 and 5; the second keeps values greater than 6. For a data frame, study[rows, columns] selects rows and columns; leaving one side blank keeps all of that dimension.')
p('A factor represents categories with a specified set of labels called levels. factor() creates it; levels() shows the labels. Avoid as.numeric() on a factor: it gives internal codes, not necessarily the numbers in its labels.')
code('ratings <- factor(c("Good", "Poor", "Average"),\n  levels = c("Poor", "Average", "Good"))\nlevels(ratings)\nratings_recoded <- factor(\n  if_else(as.character(ratings) == "Poor", "Negative", "Other"),\n  levels = c("Negative", "Other")\n)\nratings_recoded')
p('Here the levels set the display order. ordered = TRUE would additionally declare an ordered factor and can change how a model treats it. For recoding, we create a new object and explicitly define its categories. Assigning an unrecognised label directly into a factor can introduce missing values.')

section('reshape','Optional reference: long and wide data','Decide what one row should represent before changing the shape of a table. Repeated measurements need an identifier connecting observations from the same person.',True)
p('This optional example requires tidyr. Install it first with install.packages("tidyr"). The tidyr:: prefix selects the function without a library() call. data.frame() makes a table from named columns; 1:2 means the sequence 1, 2.')
code('wide <- data.frame(id = 1:2, sleep_day1 = c(6, 7),\n                   sleep_day2 = c(6.5, 7.5))\nlong <- tidyr::pivot_longer(wide,\n  cols = starts_with("sleep_"), names_to = "day",\n  names_prefix = "sleep_", values_to = "sleep_hours")\nas.data.frame(long)\nwide_again <- tidyr::pivot_wider(long,\n  names_from = day, values_from = sleep_hours,\n  names_prefix = "sleep_")\nas.data.frame(wide_again)')
p('In wide, one row represents a participant. In long, one row represents a participant on a particular day. The values are unchanged. pivot_longer() and pivot_wider() are the current alternatives to gather() and spread() in older material. Reshaping data does not make repeated observations independent.')

section('join','Optional reference: combine tables carefully','A join adds information by matching a key, such as a participant ID. Check the keys before joining so rows are not unexpectedly duplicated.',True)
code('site_lookup <- data.frame(\n  id = 1:6,\n  site = c("A", "A", "B", "B", "A", "B")\n)\nstopifnot(!anyDuplicated(site_lookup$id))\ncombined <- left_join(study, site_lookup, by = "id")\nas.data.frame(combined |> select(id, programme, site))\nnrow(combined)')
p('stopifnot() stops if a check is false. anyDuplicated() finds duplicate keys; ! turns the zero/no-duplicate result into TRUE. You can learn joins later; they are not needed for your first analysis.')
p('left_join() keeps all rows from study and adds matching site information. The check stops this example if an ID occurs more than once in the lookup table. Here six participants remain six rows. An unmatched ID would have a missing site; duplicated matching keys can multiply rows.')

section('help','When something goes wrong','Read the first error, check the line that caused it, and change one thing at a time.')
table(['What you see','What to check'],[['object not found','Did you run the line that creates it? Check spelling and capital letters.'],['could not find function','Did you load its package, or use package::function()?'],['there is no package called ...','Install that package, then load it.'],['File does not exist / cannot open file','Check the project folder, relative path and filename.'],['unexpected symbol or unexpected end','Look for a missing comma, quote or bracket.'],['NA where you expected a number','Check missing values, variable types and conversions.'],['Fewer rows than expected','Check filters, missing values and any joins.']])
code('?mean\nhelp(package = "dplyr")\ncitation()\ncitation("ggplot2")',run=False)
p('The question mark goes before the function name. citation() gives the R reference and citation("ggplot2") gives the package reference for reports or manuscripts.')
links([('RStudio: getting started','https://docs.posit.co/ide/user/ide/get-started/'),('dplyr: data manipulation','https://dplyr.tidyverse.org/'),('tidyr: reshaping data','https://tidyr.tidyverse.org/articles/pivot.html'),('R manuals','https://cran.r-project.org/manuals.html')])

# Evaluate runnable examples in a clean R process, then use those actual outputs.
codes=[]
for sec in sections:
 for block in sec['blocks']:
  if block['kind']=='code' and block['run']:
   block['index']=len(codes);codes.append({'code':block['text'],'plot':block['plot']})
tmp=Path(tempfile.mkdtemp(prefix='r-guide-'));(tmp/'chunks.json').write_text(json.dumps(codes))
runner=r'''
args <- commandArgs(TRUE)
chunks <- jsonlite::fromJSON(args[1], simplifyVector=FALSE)
setwd(args[2]); options(width=78, digits=4)
e <- new.env(parent=globalenv()); result <- list()
for (i in seq_along(chunks)) {
  ch <- chunks[[i]]
  lines <- capture.output({
    for (expression in parse(text=ch$code)) {
      value <- withVisible(eval(expression,e))
      if(value$visible && !inherits(value$value,'ggplot')) print(value$value)
    }
  })
  if(isTRUE(ch$plot)) ggplot2::ggsave(file.path(args[3],'guide-plot.png'),e$sleep_plot,width=7,height=4.5,dpi=144,bg='white')
  result[[i]] <- paste(lines,collapse='\n')
}
stopifnot(nrow(e$study)==6, sum(e$counts$n)==6, nrow(e$combined)==6,
          identical(as.numeric(e$sleep_summary$mean_hours),c(6.25,7.5)),
          nrow(e$long)==4, isTRUE(all.equal(as.data.frame(e$wide_again),e$wide,check.attributes=FALSE)))
writeLines(jsonlite::toJSON(result,auto_unbox=TRUE),file.path(args[3],'outputs.json'))
'''
(tmp/'run.R').write_text(runner)
subprocess.run([sys.argv[1] if len(sys.argv)>1 else 'Rscript',str(tmp/'run.R'),str(tmp/'chunks.json'),str(out),str(tmp)],check=True)
outputs=json.loads((tmp/'outputs.json').read_text());encoded=base64.b64encode((tmp/'guide-plot.png').read_bytes()).decode()
H=html.escape
assets=json.loads((root/"tools/guide_screenshots.json").read_text())
chunks=[];md=['---','title: "Getting started with R and preparing data"','output:','  html_document:','    toc: true','    code_folding: hide','---','','A practical beginner guide. All practice data are fictional. Open an RStudio project in the unzipped practice folder before running the code.','']
practice=['# Getting started with R and preparing data','# All data are fictional. Open an RStudio project in this folder.','# If needed, install these packages once:', '# install.packages(c("dplyr", "readr", "readxl", "ggplot2", "tidyr"))','']
for sec in sections:
 body=[f'<p class="intro">{H(sec["intro"])}</p>'];md.extend(['## '+sec['title'],'',sec['intro'],'']);practice.extend(['# '+sec['title'],'# '+sec['intro'],''])
 for b in sec['blocks']:
  k=b['kind']
  if k=='p':body.append('<p>'+H(b['text'])+'</p>');md.extend([b['text'],'']);practice.extend(['# '+b['text'],''])
  elif k=='note':body.append('<aside><strong>'+H(b['title'])+'</strong><p>'+H(b['text'])+'</p></aside>');md.extend(['> **'+b['title']+'** — '+b['text'],''])
  elif k=='steps':body.append('<ol>'+''.join('<li>'+H(x)+'</li>' for x in b['items'])+'</ol>');md.extend([f'{i+1}. {x}' for i,x in enumerate(b['items'])]+[''])
  elif k=='links':body.append('<ul class="links">'+''.join('<li><a href="'+H(u)+'"'+('' if u.startswith('#') else ' target="_blank" rel="noopener"')+'>'+H(t)+'</a></li>' for t,u in b['items'])+'</ul>');md.extend(['- ['+t+']('+u+')' for t,u in b['items']]+[''])
  elif k=='video':
   player='<video id="visualization-video" aria-label="Data visualization with narration" controls preload="none" playsinline style="display:block;width:100%;height:auto;max-height:75vh;background:#102c36;border-radius:10px"><source src="'+H(b['src'])+'" type="video/mp4">Your browser cannot play this video. Use the link below to open it.</video><p><a href="'+H(b['src'])+'" target="_blank" rel="noopener">Open video in a new tab</a></p>'
   body.append(player);md.extend([player,''])
  elif k=='table':
   body.append('<p class="table-hint">Scroll sideways within the table to see all columns.</p><div class="table-scroll" tabindex="0"><table><thead><tr>'+''.join('<th>'+H(x)+'</th>' for x in b['headers'])+'</tr></thead><tbody>'+''.join('<tr>'+''.join('<td>'+H(x)+'</td>' for x in row)+'</tr>' for row in b['rows'])+'</tbody></table></div>')
   md.extend(['| '+' | '.join(b['headers'])+' |','| '+' | '.join(['---']*len(b['headers']))+' |']+['| '+' | '.join(x.replace('|','\\|') for x in row)+' |' for row in b['rows']]+[''])
  elif k=='check':
   body.append('<aside class="checkpoint"><strong>'+H(b['title'])+'</strong><p>'+H(b['question'])+'</p><details><summary>Show explanation</summary><p>'+H(b['answer'])+'</p></details></aside>')
   md.extend(['### '+b['title'],'',b['question'],'','<details><summary>Show explanation</summary>','',b['answer'],'','</details>',''])
  elif k=='extra':
   body.append('<details class="extra"><summary>'+H(b['title'])+'</summary><p>'+H(b['text'])+'</p><ol>'+''.join('<li>'+H(x)+'</li>' for x in b['items'])+'</ol></details>')
   md.extend(['### '+b['title'],'',b['text'],'']+[str(i+1)+'. '+x for i,x in enumerate(b['items'])]+[''])
  elif k=='shot':
   a=assets[b['key']];src='data:'+a.get('mime','image/png')+';base64,'+a['data']
   attribution=('<a href="'+H(a['url'])+'" target="_blank" rel="noopener">'+H(a['source'])+'</a>') if a['url'] else H(a['source'])
   figure='<figure class="screenshot"><img loading="lazy" alt="'+H(b['alt'])+'" src="'+src+'"><button class="enlarge" type="button">Enlarge screenshot</button><figcaption>'+H(b['caption'])+' <span class="credit">Source: '+attribution+'.</span></figcaption></figure>'
   body.append(('<details class="extra"><summary>Show the Excel import screenshot</summary>'+figure+'</details>') if b['optional'] else figure)
   md.extend(['!['+b['alt']+']('+src+')','',b['caption']+' Source: '+a['source']+(' ('+a['url']+')' if a['url'] else '')+'.',''])
  elif k=='code':
   label=b.get('label') or ('R code — expand, then run these lines' if b['run'] else 'R code — run separately if needed')
   body.append('<details class="code"><summary>'+H(label)+'</summary><button type="button" class="copy">Copy code</button><pre><code>'+H(b['text'])+'</code></pre></details>')
   opts='' if b['run'] else ', eval=FALSE';md.extend(['```{r'+opts+'}',b['text'],'```',''])
   if b['run']:
    practice.extend([b['text'],'']);text=outputs[b['index']]
    if text:body.append('<div class="output-label">Output</div><pre class="output">'+H(text)+'</pre>')
    if b['plot']:body.append('<figure><img alt="Five recorded sleep values, split between the two programmes." src="data:image/png;base64,'+encoded+'"><figcaption>Fictional practice data: five recorded values and one missing value.</figcaption></figure>')
   else:practice.extend(['# Reference only (run separately if needed):']+['# '+line for line in b['text'].splitlines()]+[''])
 content=''.join(body)
 if sec['optional']:chunks.append('<details class="optional" id="'+sec['id']+'"><summary>'+H(sec['title'])+'</summary>'+content+'</details>')
 else:chunks.append('<section id="'+sec['id']+'"><h2>'+H(sec['title'])+'</h2>'+content+'</section>')
md.extend(['','<!-- This guide contains no personal attribution. -->'])
(out/'data-manipulation.Rmd').write_text('\n'.join(md))
(out/'practice.R').write_text('\n'.join(practice))
css='''*{box-sizing:border-box}p,li,td,th,summary,a{overflow-wrap:anywhere}pre,code{overflow-wrap:normal}html{scroll-behavior:smooth}body{margin:0;background:#f5f7f7;color:#19343d;font-family:system-ui,-apple-system,"Segoe UI",sans-serif;line-height:1.65;font-size:16px}a{color:#076e7a}a:focus-visible,button:focus-visible,summary:focus-visible{outline:3px solid #a987d8;outline-offset:3px}header{max-width:1080px;margin:auto;padding:38px 24px 22px}.eyebrow{font-size:12px;letter-spacing:.12em;color:#087f8c;font-weight:750;text-transform:uppercase}h1{font-size:clamp(28px,4vw,40px);line-height:1.15;max-width:760px;letter-spacing:-.025em}h2{font-size:24px;line-height:1.3;margin-top:0}p,li{max-width:76ch}.intro{color:#526b75;font-size:18px}.layout{display:grid;grid-template-columns:225px minmax(0,1fr);gap:26px;max-width:1080px;margin:auto;padding:0 24px 40px}nav{position:sticky;top:16px;align-self:start;background:white;border:1px solid #dce6e7;border-radius:12px;padding:18px;font-size:13px;max-height:90vh;overflow:auto}nav a{display:block;text-decoration:none;padding:7px 0;line-height:1.35}nav strong{display:block;margin-bottom:10px}main{min-width:0}section,.optional{background:white;border:1px solid #dce6e7;border-radius:14px;padding:26px;margin-bottom:20px;scroll-margin-top:16px}aside{border-left:3px solid #087f8c;padding:14px 17px;background:#edf6f5;margin:22px 0;border-radius:0 8px 8px 0}aside p{margin:6px 0 0}summary{cursor:pointer;font-weight:700}.optional>summary{font-size:20px;line-height:1.4}.optional[open]>summary{margin-bottom:18px}.code{border:1px solid #dce6e7;border-radius:9px;margin:18px 0;overflow:hidden}.code summary{padding:12px 15px;color:#076e7a;font-size:14px}.code[open] summary{border-bottom:1px solid #dce6e7}.copy{margin:10px 12px 0;float:right;border:1px solid #536c73;color:#e4f2f2;background:#23414c;border-radius:5px;padding:6px 10px;cursor:pointer}.code[open]{background:#122e38}.code[open] summary{background:white}pre{max-width:100%;overflow:auto;font-family:ui-monospace,SFMono-Regular,Consolas,monospace;font-size:13px;line-height:1.6;padding:16px;margin:0;tab-size:2;white-space:pre}code{color:#e3f2f3}.output-label{color:#627781;font-size:12px;font-weight:700;text-transform:uppercase;letter-spacing:.07em}.output{background:#f1f5f6;border-radius:8px;margin:5px 0 20px;color:#19343d}.table-scroll{max-width:100%;overflow:auto;margin:18px 0}table{border-collapse:collapse;width:100%;min-width:520px;font-size:14px}th,td{text-align:left;vertical-align:top;padding:11px 12px;border-bottom:1px solid #dce6e7}th{background:#edf4f4}td:first-child{font-weight:600;min-width:120px}figure{margin:22px 0}figure img{width:100%;height:auto}figcaption{font-size:13px;color:#627781}.actions{display:flex;gap:10px;flex-wrap:wrap;margin:22px 0}.button{display:inline-block;background:#087f8c;color:white;padding:10px 16px;border-radius:9px;text-decoration:none;font-weight:650;font-size:14px}.button.secondary{background:white;color:#076e7a;border:1px solid #c9dcde}.links{padding-left:20px}li{margin:8px 0}footer{max-width:1080px;margin:auto;padding:0 24px 32px;color:#627781;font-size:13px}@media(max-width:760px){.layout{display:block;padding:0 16px 24px}header{padding:24px 16px 12px}nav{position:static;margin-bottom:20px;max-height:240px}section,.optional{padding:19px}table{font-size:13px}}@media print{nav,.actions,.copy{display:none}.layout{display:block}section,.optional{break-inside:avoid;border:0}body{background:white}}'''
css+='''.table-hint{display:none;color:#627781;font-size:12px}@media(max-width:760px){.table-hint{display:block}}[hidden]{display:none!important}.guide-tools{display:flex;align-items:center;gap:12px;flex-wrap:wrap;margin-bottom:18px}.guide-tools button,.step-controls button,.enlarge,dialog button{border:1px solid #b8cfd2;background:white;color:#076e7a;border-radius:8px;padding:10px 14px;cursor:pointer;font:inherit;font-size:14px;font-weight:650}.step-controls{display:flex;gap:12px;align-items:center;justify-content:space-between;flex-wrap:wrap;margin:26px 0 8px}.step-controls .next{background:#087f8c;color:white}.step-controls button:disabled{opacity:.45;cursor:default}.step-progress{color:#526b75;font-size:13px}.checkpoint{background:#f2eef8;border-color:#7960ad}.checkpoint summary{font-size:14px}.checkpoint details p{margin-top:12px}.extra{border:1px solid #dce6e7;padding:15px;border-radius:10px;margin:16px 0}.screenshot{border:1px solid #dce6e7;border-radius:10px;overflow:hidden;padding:10px;background:#fafcfc}.screenshot img{display:block;cursor:zoom-in}.screenshot figcaption{padding:10px 2px 0}.credit{display:block;margin-top:6px;font-size:12px}.enlarge{margin-top:10px}dialog{width:96vw;max-width:1800px;max-height:94vh;padding:14px;border:1px solid #b8cfd2;border-radius:12px;color:#19343d}dialog::backdrop{background:#102c36cc}.zoom-scroll{max-height:76vh;overflow:auto;margin-top:10px}.zoom-scroll img{width:auto;max-width:none;height:auto;display:block}nav a[aria-current=step]{font-weight:800;background:#e7f3f2;border-radius:5px;padding-left:7px}.contents-toggle{font-size:14px}.code summary{line-height:1.5}.code pre{clear:both}.output{max-height:400px}.intro{font-size:17px}header .intro{max-width:70ch}section:focus{outline:none}@media(max-width:760px){nav{max-height:none}nav details[open]{max-height:310px;overflow:auto}.step-controls button{padding:10px}dialog{width:98vw}.screenshot{padding:6px}h2{font-size:23px}}@media print{[hidden]{display:block!important}.guide-tools,.step-controls,.enlarge,dialog{display:none!important}.code pre{white-space:pre-wrap}.code{break-inside:avoid}}'''
nav=''.join('<a href="#'+s['id']+'">'+H(s['title'])+'</a>' for s in sections)
js=r'''const movie=document.querySelector('#visualization-video');
if(movie&&location.protocol!=='file:'){
 const load=document.createElement('button');load.type='button';load.className='button';load.id='load-video';load.textContent='Load video (31 MB)';
 const status=document.createElement('p');status.id='video-status';status.setAttribute('role','status');status.textContent='Load the video when you are ready. It will not play automatically.';
 movie.before(load,status);movie.hidden=true;let movieUrl=null;
 load.addEventListener('click',async()=>{load.disabled=true;status.textContent='Loading the video… Please wait.';
  try{const response=await fetch(movie.querySelector('source').getAttribute('src'));if(!response.ok)throw Error('Video unavailable');const blob=await response.blob();movieUrl=URL.createObjectURL(blob);movie.src=movieUrl;movie.hidden=false;movie.load();load.hidden=true;status.textContent='Ready. Press Play to begin. You can pause or move to any point in the video.';}
  catch(e){movie.hidden=false;load.disabled=false;load.textContent='Try loading again';status.textContent='The video could not be loaded. Try again, or use Open video in a new tab below.';}
 });
 movie.closest('details').addEventListener('toggle',e=>{if(!e.target.open)movie.pause()});
 window.addEventListener('pagehide',()=>{movie.pause();if(movieUrl)URL.revokeObjectURL(movieUrl)});
}
const lessons=[...document.querySelectorAll('main>section, main>details.optional')];let all=false;let index=0;
function showStep(n,scroll=true){index=Math.max(0,Math.min(n,lessons.length-1));lessons.forEach((el,i)=>{el.hidden=!all&&i!==index;if(el.hidden)el.querySelectorAll('video').forEach(v=>v.pause());if(!all&&i===index&&el.tagName==='DETAILS')el.open=true;});document.querySelectorAll('nav a').forEach((a,i)=>{if(i===index)a.setAttribute('aria-current','step');else a.removeAttribute('aria-current')});document.querySelectorAll('.step-progress').forEach(el=>el.textContent=index<16?'Step '+(index+1)+' of 16':lessons[index].id==='video'?'Optional video':'Reference');document.querySelectorAll('.prev').forEach(b=>b.disabled=index===0);document.querySelectorAll('.next').forEach(b=>{b.disabled=index===lessons.length-1;b.textContent=index===15?'Optional video →':'Next step →'});if(scroll){lessons[index].scrollIntoView({block:'start',behavior:'instant'});lessons[index].setAttribute('tabindex','-1');lessons[index].focus({preventScroll:true})}}
const controls=document.createElement('div');controls.className='step-controls';controls.innerHTML='<button type="button" class="prev">← Previous</button><span class="step-progress" aria-live="polite"></span><button type="button" class="next">Next step →</button>';document.querySelector('main').append(controls);
function navigate(n){location.hash=lessons[Math.max(0,Math.min(n,lessons.length-1))].id;}
document.querySelectorAll('.prev').forEach(b=>b.addEventListener('click',()=>navigate(index-1)));document.querySelectorAll('.next').forEach(b=>b.addEventListener('click',()=>navigate(index+1)));
function fromHash(scroll){const n=lessons.findIndex(el=>'#'+el.id===location.hash);showStep(n<0?0:n,scroll)}window.addEventListener('hashchange',()=>fromHash(true));fromHash(location.hash==='#video');
document.querySelector('#show-all').addEventListener('click',e=>{all=!all;e.target.textContent=all?'Read one step at a time':'Show the whole guide';showStep(index,false)});
if(innerWidth>760)document.querySelector('#contents').open=true;
document.querySelectorAll('nav a').forEach(a=>a.addEventListener('click',()=>{if(innerWidth<=760)document.querySelector('#contents').open=false;}));
document.querySelectorAll('.copy').forEach(button=>button.addEventListener('click',async()=>{const text=button.parentElement.querySelector('code').textContent;try{await navigator.clipboard.writeText(text);button.textContent='Copied';setTimeout(()=>button.textContent='Copy code',1800)}catch(e){button.textContent='Select the code to copy';}}));
const dialog=document.querySelector('#image-dialog');const zoom=dialog.querySelector('img');document.querySelectorAll('.screenshot').forEach(f=>{const open=()=>{const img=f.querySelector('img');zoom.src=img.src;zoom.alt=img.alt;dialog.showModal()};f.querySelector('.enlarge').addEventListener('click',open);f.querySelector('img').addEventListener('click',open)});dialog.querySelector('button').addEventListener('click',()=>dialog.close());'''
page='<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Your first steps in R and RStudio</title><style>'+css+'</style></head><body><header><p class="eyebrow">New to R · Start from the beginning</p><h1>Your first steps<br>in R and RStudio</h1><p class="intro">Find your way around the screen. Save your first script. Import a small table. Learn what each instruction means before using it.</p><p>No programming experience is assumed. Steps 1–10 cover the foundations; steps 11–16 introduce preparation, tables and figures. Pause whenever you need to and return using Choose a step.</p><div class="actions"><a class="button" href="practice.zip" download>Download practice files</a><a class="button secondary" href="practice.R" download>Completed example script</a><a class="button secondary" href="#video">Watch data visualization</a></div></header><div class="layout"><nav aria-label="Guide contents"><details id="contents"><summary class="contents-toggle">Choose a step</summary>'+nav+'</details></nav><main><div class="guide-tools"><button type="button" id="show-all">Show the whole guide</button><span class="step-progress"></span></div>'+''.join(chunks)+'</main></div><dialog id="image-dialog" aria-label="Enlarged screenshot"><button type="button">Close screenshot</button><p>Scroll inside the image to inspect it at its original size. Press Escape or Close screenshot to return.</p><div class="zoom-scroll"><img alt=""></div></dialog><footer>All practice data are fictional. Screenshots are embedded and the video is included in the practice download, so both remain available offline. The interface may differ slightly by RStudio version. Installation and external help links require internet access. <a href="data-manipulation.Rmd" download>Editable R Markdown source</a>.</footer><script>'+js+'</script></body></html>'

(out/'data-manipulation.html').write_text(page)
with zipfile.ZipFile(out/'practice.zip','w',zipfile.ZIP_DEFLATED) as z:
 for name in ['practice.R','data-manipulation.Rmd','data/practice.csv']:z.write(out/name,name)
 z.write(out/'data-manipulation.html','data-manipulation.html')
 z.write(out/'data-visualization.mp4','data-visualization.mp4',compress_type=zipfile.ZIP_STORED)
 z.writestr('README.txt','Open data-manipulation.html in a browser for the step-by-step guide.\nUnzip into a folder named r-practice. In RStudio: File > New Project > Existing Directory. Select that folder.\nCreate your own my_first_script.R using File > New File > R Script. Follow the guide one step at a time.\npractice.R is the completed example. All data are fictional.\nThe last script section writes demonstration outputs; rerunning replaces those files.\n')
# Outputs generated during verification are not shipped as extra loose files.
import shutil
shutil.rmtree(out/'output',ignore_errors=True)
print('Built guide; verified',len(codes),'R examples. Files:',[p.name for p in out.iterdir()])
