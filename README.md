
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MSAT

The MSAT is an non-adaptive instrument to test musical scene analysis
abilities among very diverse populations.

## Citation

We also advise mentioning the software versions you used, in particular
the versions of the `MSAT` and `psychTestR` packages. You can find these
version numbers from R by running the following commands:

``` r
library(MSAT)
library(psychTestR)
if (!require(devtools)) install.packages("devtools")
x <- devtools::session_info()
x$packages[x$packages$package %in% c("MSAT", "psychTestR"), ]
```

## Installation instructions (local use)

1.  If you don’t have R installed, install it from here:
    <https://cloud.r-project.org/>

2.  Open R.

3.  Install the ‘devtools’ package with the following command:

`install.packages('devtools')`

4.  Install the MSAT:

`devtools::install_github('rhake/MSAT')`

## Usage

### Quick demo

You can demo the MSAT at the R console, as follows:

``` r
# Load the MSAT package
library(MSAT)

# Run a demo test, with feedback as you progress through the test,
# and not saving your data
MSAT_demo()

# Run a demo test, skipping the training phase, and only asking 5 questions, as well a changinge the language
MSAT_demo(num_items = 5, language = "de")
```

### Testing a participant

The `MSAT_standalone()` function is designed for real data collection.
In particular, the participant doesn’t receive feedback during this
version.

``` r
# Load the MSAT package
library(MSAT)

# Run the test as if for a participant, using default settings,
# saving data, and with a custom admin password
MSAT_standalone(admin_password = "put-your-password-here")
```

You will need to enter a participant ID for each participant. This will
be stored along with their results.

Each time you test a new participant, rerun the `MSAT_standalone()`
function, and a new participation session will begin.

You can retrieve your data by starting up a participation session,
entering the admin panel using your admin password, and downloading your
data. For more details on the psychTestR interface, see
<http://psychtestr.com/>.

The MSAT currently supports English (en), German (de), You can select
one of these languages by passing a language code as an argument to
`MSAT_standalone()`, e.g. `MSAT_standalone(languages = "de")`, or
alternatively by passing it as a URL parameter to the test browser, eg.
<http://127.0.0.1:4412/?language=DE> (note that the `p_id` argument must
be empty).

## Implementation notes

By default, the MSAT implementation always estimates participant
abilities using weighted-likelihood estimation. We adopt
weighted-likelihood estimation for this release because this technique
makes fewer assumptions about the participant group being tested. This
makes the test better suited to testing with diverse participant groups
(e.g. children, clinical populations).

## Usage notes

  - The MSAT runs in your web browser.
  - By default, image files are hosted online on our servers. The test
    therefore requires internet connectivity.
