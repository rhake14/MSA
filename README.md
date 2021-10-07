
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MSA

<!-- badges: start -->
<!-- badges: end -->

# MSA

The MSA is an non-adaptive instrument to test musical scene analysis
abilities among very diverse populations. An adaptive version of this
instrument is currently in development (expected early 2022).

## Citation

We also advise mentioning the software versions you used, in particular
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

### Quick demo

You can demo the MSA at the R console, as follows:

``` r
# Load the MSA package
library(MSA)
# Run a demo test, with feedback as you progress through the test,
# and not saving your data
MSA_demo()
# Run a demo test, skipping the training phase, and only asking 5 questions, as well a changinge the language
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

The MSA currently supports English (en), German (de) - more languages
follow soon. You can select one of these languages by passing a language
code as an argument to `MSA_standalone()`,
e.g. `MSA_standalone(languages = "en")`, or alternatively by passing it
as a URL parameter to the test browser, eg.
<http://127.0.0.1:4412/?language=DE> (note that the `p_id` argument must
be empty).

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
guitar, bass, or piano), (2) the acoustic complexity (three instruments
vs. six instruments in the mixture), and (3) the energy level-ratio of
the target in comparison with the mixture (0, -5, -10, -15 dB). In order
to investigate potential candidates for the test excerpts, instruments
in the database were first categorized as lead vocals, backing vocals,
bass, drums, guitars, keys, piano, percussion, strings, winds and
others. Secondly, sound levels were analysed and calculated based on the
root-mean-square average over 500ms long time windows for the full
duration of the song. If one instrument in the target category and two
to six additional instruments had sound levels above -20 dB relative to
the instrument’s maximum sound level, they qualified as potential
candidate. To avoid click artefacts caused by the extraction at random
timepoints, a logarithmic fade-in and fade-out with a duration of 200 ms
was applied to the beginning and end of the signals. Furthermore, the
mixes has been readjusted by a professional musician.

Signals from the candidate list were extracted pseudo-randomly, using
the same song as infrequently as possible, with the target instruments
(1) lead vocal, (2) guitar, (3) bass and (4) piano being evenly
balanced. In half of the mixes, the target instrument was not part of
the mixture - in this case, excerpts with four or seven instruments were
chosen (to ensure three and six instrument signals in the mixture,
respectively).

More information about the stimuli used can be found in the github
repository
(<https://github.com/rhake14/MSA/blob/main/data_raw/MSA_item_bank.csv>).

## Usage notes

-   The MSA runs in your web browser.
-   By default, image files are hosted online on our servers
    (<http://testing.musikpsychologie.de/dots_home/>). The test
    therefore requires internet connectivity. An offline alternative
    would be to download the entire repository to your local computer.

## Citations

Bittner, R., Salamon, J., Tierney, M., Mauch, M., Cannam, C., & Bello,
J. P. (2014). MedleyDB: A Multitrack Dataset for Annotation-Intensive
MIR Research. In 15th International Society for Music Information
Retrieval Conference, Taipei, Taiwan.

Bittner, R., Wilkins, J., Yip, H., & Bello, J. (2016). MedleyDB 2.0: New
Data and a System for Sustainable Data Collection. International
Conference on Music Information Retrieval (ISMIR-16), New York, NY, USA.
