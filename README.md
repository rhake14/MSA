
<!-- badges: start -->
<!-- badges: end -->

# MSA

The Musical Scene Analysis Test (MSA) is an adaptive instrument for
assessing auditory scene analysis (ASA) abilities in the context of
music in individuals with very diverse ability levels. The test has been
calibrated an online experiment among 525 normal-hearing (NH)
participants with ages ranging from 18 - 72 years ($\mu$ = 28 years; SD
= 11.02; 274 female) and 131 hearing-impaired (HI) participants with
ages ranging from 23 - 82 years ($\mu$ = 40.5 years; SD = 20.6; 67
female). The MSA is suitable for measuring ASA abilities in young (and
musically trained) NH as well as old HI individuals with up to severe
non-aided hearing impairments (i.e., the MSA is not suitable for
profound HI individuals). In a subsequent in-lab validation experiment
with 74 listeners (20 HI), MSA scores showed acceptable test-retest
reliability and moderate correlations with other music-related tests,
pure-tone-average audiograms, age, musical sophistication, and working
memory capacities. Alongside the standard MSA, an alternative
non-adaptive version of the instrument is also available as part of this
package. To better cater to individuals using hearing aids or cochlear
implants, we’ve also developed a specialized version of the test. This
includes extended musical excerpts, which prove beneficial during the
device’s internal fitting procedures. Currently, the test is available
in German (formal and informal), English, and French language. More
languages can be quickly implemented on request. For more information
see Hake et al. (2023; Behavioural Research Methods;
<https://link.springer.com/article/10.3758/s13428-023-02279-y>)

### Quick online Demo

A running demo version of the MSA can be accessed via:
<https://shiny.gold-msi.org/longgold_demo/?test=MSA>

### Special requests

The current item bank comprises 160 excerpts featuring target
instruments such as lead vocals, piano, guitar, and bass, as well as
mixtures containing either 3 or 6 instruments (see ‘Stimuli
construction’ below). If different or specific configurations are
require, we are able to generate new items from a pool of over 12,000
available candidate excerpts. Depending on your particular conditions,
an adaptive version of the test may still be feasible by applying the
Bayesian model that was used to estimate item difficulty. For questions
and special requests, please contact us directly (via
<robin.hake@uni-oldenburg.de> or <robinhake93@googlemail.com>).

## Citation

You can cite the MSA as follows:

> Hake, R., Bürgel, M., Nguyen, N.K., Greasley, A., Müllensiefen, D. &
> Siedenburg, K. (2023). Development of an adaptive test of musical
> scene analysis abilities for normal-hearing and hearing-impaired
> listeners. Behavioural Research Methods (2023).
> <https://doi.org/10.3758/s13428-023-02279-y>

We also advise mentioning the software versions you used. In particular
the versions of the `MSA` and `psychTestR` packages. You can find these
version numbers from R by running the following commands:

``` r
library(MSA)
library(psychTestR)
if (!require(devtools)) install.packages("devtools")
x <- devtools::session_info()
x$packages[x$packages$package %in% c("MSA", "psychTestR"), ]
```

## Installation instructions (local use)

1.  If you don’t have R installed, install it from here:
    <https://cloud.r-project.org/>

2.  Open R.

3.  Install the ‘devtools’ package with the following command:

`install.packages('devtools')`

4.  Install the MSA:

`devtools::install_github('rhake14/MSA')`

## Usage

### Quick local demo

You can demo the MSA at the R console, as follows:

``` r
# Load the MSA package
library(MSA)
# Run a demo test, with feedback as you progress through the test,
# and not saving your data
MSA_demo()
# Run a demo test, skipping the training phase, and only asking 5 questions,
# as well a changinge the language
MSA_demo(num_items = 5, language = "de")
```

### Testing a participant

The `MSA_standalone()` function is designed for real data collection. In
particular, the participant doesn’t receive feedback during this
version.

``` r
# Load the MSA package
library(MSA)
# Run the test as if for a participant, using default settings,
# saving data, and with a custom admin password
MSA_standalone(admin_password = "put-your-password-here")
```

You will need to enter a participant ID for each participant. This will
be stored along with their results.

Each time you test a new participant, rerun the `MSA_standalone()`
function, and a new participation session will begin.

You can retrieve your data by starting up a participation session,
entering the admin panel using your admin password, and downloading your
data (.rds file is recommended). For more details on the psychTestR
interface, see <http://psychtestr.com/>.

The MSA currently supports English (en), German (de), fomral German
(de_f), and French (fr) - more languages follow soon. You can select one
of these languages by passing a language code as an argument to
`MSA_standalone()`, e.g. `MSA_standalone(languages = "en")`, or
alternatively by passing it as a URL parameter to the test browser, eg.
<http://127.0.0.1:4412/?language=DE> (note that the `p_id` argument must
be empty).

### Get results

If you are just interested in the participants’ final scores, the
easiest solution is usually to download the results in .RDS format from
the admin panel \[use function readRDS()\]. If you need only a few
detail, you can examine the individual CSV output. If you are interested
in trial-by-trial results, you can run the command
MSA_compile_trial_by_trial_results() from the R console (having loaded
the MSA package using library(MSA)). Type
?MSA_compile_trial_by_trial_results() for more details.

Detailed results are stored as the ‘metadata’ attribute for the ability
field. You can access it something like this (see also example code at
the end of the documentation):

``` r
x <- readRDS("output/results/id=1&p_id=german_test&save_id=1&pilot=false&complete=true.rds")
attr(x$MSA$ability, "metadata")
```

## Item / Stimuli construction

The stimulus material was drawn from an open-source database
(“MedleyDB”, see Bittner et al., 2014; 2016), which comprises recorded
real-world multitrack music excerpts representing a range of popular
music genres (e.g., classical, rock, world/folk, fusion, jazz, pop, rap,
etc.).

Excerpts were generated semi-automatically in the programming
environment MATLAB (Mathworks, 2020) and consisted of a 2-sec clip of a
single instrument or voice (the target), followed by a 1-sec gap of
silence, and a 2-sec clip with multiple instruments (mixture) – each
varying in terms of (1) the choice of the target instrument (lead voice,
guitar, bass, or piano), (2) the musical complexity, that is the number
of instruments in the mixture (three instruments vs. six instruments in
the mixture), and (3) the level-ratio of the target in comparison with
the mixture (0, -5, -10, -15 dB).

In order to investigate potential candidates for the test excerpts,
instruments in the database were first categorized as lead vocals,
backing vocals, bass, drums, guitars, keys, piano, percussion, strings,
winds and others. Secondly, sound levels were analysed and calculated
based on the root-mean-square average over 500ms long time windows for
the full duration of the song. If one instrument in the target category
and two to six additional instruments had sound levels above -20 dB
relative to the instrument’s maximum sound level, they qualified as
potential candidate. To avoid click artefacts caused by the extraction
at random timepoints, a logarithmic fade-in and fade-out with a duration
of 200 ms was applied to the beginning and end of the signals.
Furthermore, the mixes has been readjusted by a professional musician.
Signals from the candidate list were extracted pseudo-randomly, using
the same song as infrequently as possible, with the target instruments
(1) lead vocal, (2) guitar, (3) bass and (4) piano being evenly
balanced.

In total, 160 excerpts were generated based on 98 different base tracks
(songs). In half of the mixes, the target instrument was not part of the
mixture - in this case, excerpts with four or seven instruments were
chosen (to ensure three and six instrument signals in the mixture,
respectively). For more information on the test construction have a look
at the publication.

More information about the stimuli used can be found in the github
repository
(<https://github.com/rhake14/MSA/blob/main/data_raw/MSA_item_bank.csv>).
There detailed configurations on instruments used within each excerpt
can be found (see also “MSA_item_bank_explanation.txt”).

## Usage notes

- The MSA runs in your web browser.
- By default, image files are hosted online on our servers, provided by
  the Deutsche Gesellschaft für Musikpsychologie
  e.V.(<http://www.music-psychology.de/>) via
  “<http://testing.musikpsychologie.de/dots_home/>”. **The test
  therefore requires internet connectivity**.
- An offline alternative would be to download the entire repository and
  stimuli set (folder: MSA - all in one) to your local computer
  (<https://drive.google.com/drive/folders/1arLGKe-tqeHLBFoj0LNc8UMmSppBP2pP?usp=sharing>
  and manually change the directory for the items in the package (using
  “location_stim” within the function “MSA_standalone()” / “MSA()”; see
  also “?MSA” and location_stim for more information).

## Acknowledgements

Special acknowledgement is extended to Klaus Frieler for his invaluable
assistance with the implementation of the MSA as an R package and for
his general technical support. Additionally, we express our gratitude to
Christian Lespioniges for his significant contribution in translating
the MSA into the French language.

## Citations

Bittner, R., Salamon, J., Tierney, M., Mauch, M., Cannam, C., & Bello,
J. P. (2014). MedleyDB: A Multitrack Dataset for Annotation-Intensive
MIR Research. In 15th International Society for Music Information
Retrieval Conference, Taipei, Taiwan.

Bittner, R., Wilkins, J., Yip, H., & Bello, J. (2016). MedleyDB 2.0: New
Data and a System for Sustainable Data Collection. International
Conference on Music Information Retrieval (ISMIR-16), New York, NY, USA.

## Script: Example Experiment

``` r
# prepare experiment ------------------------------------------------------

# install required packages
if (!require(devtools)) install.packages("devtools")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(tidyr)) install.packages("tidyr") # etc..

# download MSA from github: 
devtools::install_github('rhake14/MSA')

# Load the MSA package
library(MSA) # ?MSA_standalone #more information on the function

# run experiment ----------------------------------------------------------

# Run the test as if for a participant, using default settings,
# saving data, and with a custom admin password
# including advanced specifications
MSA_standalone(
  title = "Musical Scene Analysis Experiment #1",
  num_items = 40,
  adaptive = TRUE,
  admin_password = "password",
  researcher_email = "abc@gmail.de",
  languages = c("EN")
)

# analyse the data --------------------------------------------------------
# load packages
library(tidyverse)
pacman::p_load(dplyr, tibble, ggplot2, devtools, readxl, data.table, Hmisc, MSA,
)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
wd <- print(getwd())

# ** get results from file -------------------------------------------------------------
### result files for the main experiment 
results_list <- list.files(path = paste0(wd,"/output/results"),
                                 pattern = ".rds", full.names = TRUE)
# read the RDS file
results_list <- results_list %>%
  lapply(readRDS)

# convert to a data frame
results_dataframe <- tibble::as_tibble(do.call(rbind, results_list)) 
results_dataframe <- tibble::as_tibble(do.call(rbind, results_dataframe)) 

# & unnest
results_dataframe <- plyr::ldply(results_dataframe, data.frame)

# select only relevant coloumns
results_dataframe <- results_dataframe %>% 
  dplyr::select(p_id, complete, time_started, current_time, ability, ability_sem, num_items)

# give me trial_by_trial results of the MSA
# ?MSA_compile_trial_by_trial_results  # for more information

# **  get individual MSA data  --------------------------------------------
tmp.results <- list.files(path = paste0(wd,"/output/results"),
              pattern = ".rds", full.names = TRUE)


tmp.MSA_results <- NULL
results_per_item <- NULL
# i <- NULL #debug
# i <- 1

for (i in 1:length(tmp.results)) {
  paste(i)
  # load rds
  # i4 = 2
  tmp.load <- readRDS(tmp.results[[i]])
  
  # extract p_id from the loaded file
  tmp.p_id <- qdapRegex::ex_between(
    tmp.results[[i]],
    left = "p_id=",
    right = "&save_id",
    include.markers = FALSE) %>% as.character()
  
  # sanity check
  tmp.p_id
  
  # duration 
  tmp.duration <- abs(as.numeric(tmp.load$session$time_started - tmp.load$session$current_time))
  
  # because its the long version we need to fetch the MSA_results names
  MSA3_indices <- grep("^MSA_long", names(tmp.load))
  MSA3_names <- names(tmp.load)[MSA3_indices]
  
  # check if MSA_results exists, else just say incomplete & NA
  if (length(MSA3_indices) == 1) {
    
    MSA3 <- as.numeric(tmp.load[[MSA3_names]]$ability[1]) 
    MSA3_sem <- as.numeric(tmp.load[[MSA3_names]]$ability_sem[1])
    
    if(is.null(MSA3) || is.na(MSA3)) {MSA3_complete <- "incomplete"} else {MSA3_complete = "complete"}
  } else  {MSA3 <- NA
  MSA3_complete <- "incomplete"}
  
  if (length(MSA3_indices) == 1) {
    tmp.MSA_results <- tmp.load[[MSA3_names]]$ability
    tmp.MSA_results <- attributes(tmp.MSA_results)$metadata$results %>%
      dplyr::select(num, # = order of the item
                    item_id, # = item_id; see https://github.com/rhake14/MSA/blob/main/data_raw/MSA_item_bank.csv
                    difficulty, # = item difficulty
                    answer, # the answer provided by the participant
                    correct_answer, # the correct answer
                    score, # TRUE = the anwser was correct
                    ability_WL, # the estimated ability score 
                    ability_WL_sem # the estimated standard error of the ability estimate
                    ) %>%
      dplyr::rename(ability = ability_WL, # WL = for weighted likelihood ability estimate
                    ability_sem = ability_WL_sem) %>%
      mutate(p_id = tmp.p_id) # participants ID
    
    results_per_item <- rbind(results_per_item, tmp.MSA_results)
  }
}

# select only relevant rows
results_per_item <- results_per_item %>%  
  dplyr::select(p_id, num, item_id, difficulty, # = item difficulty
                score, ability_WL, ability_WL_sem) %>% 
  dplyr::rename(ability = ability_WL, # for weighted likelihood ability estimate
                ability_sem = ability_WL_sem)

# quick plot  --------------------------------------------------------------
results_per_item %>%
  dplyr::rename(MSA = ability) %>%
  ggplot2::ggplot(aes(num, MSA)) +
  ggplot2::geom_line(aes(fill = p_id)) +
  ggplot2::geom_point(
    aes(fill = p_id),
    size = 3,
    pch = 21, # Type of point that allows us to have both color (border) and fill.
    color = "white",
    stroke = 1 # The width of the border, i.e. stroke.
  )   +
  ggplot2::xlab("Item sequence number") +
  ggplot2::ylab("MSA ability score") +
  ggplot2::theme_bw(base_size = 15)

# quick inspection  --------------------------------------------------------------
results_per_item %>% ggplot2::ggplot(aes(score, difficulty)) +
  ggplot2::geom_point(
    aes(fill = p_id),
    size = 3, 
    pch = 21, # Type of point that allows us to have both color (border) and fill.
    color = "white", 
    stroke = 1 # The width of the border, i.e. stroke.
  ) 

# inspect per participant
loop_p_ID <- NULL
for(loop_p_ID in 1:length(unique(results_per_item$p_id))) {
  tmp.print <- results_per_item %>% 
    filter(p_id == unique(results_per_item$p_id)[loop_p_ID]) %>% 
    ggplot2::ggplot(aes(score, difficulty)) +
    ggplot2::geom_point(
      aes(fill = p_id),
      size = 3, 
      pch = 21, # Type of point that allows us to have both color (border) and fill.
      color = "white", 
      stroke = 1 # The width of the border, i.e. stroke.
    ) 
  print(tmp.print) 
}
```
