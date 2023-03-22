# Input: csv file samples/authors-team-impact.csv

# Output:
# * df             - the raw csv datapoints
# * data_logscaled - raw data with predictors data_logscaled
# * data           - data with scaled predictors A, R, C, D
# * teams          - the teams present in the data
# * repos          - the repositories present in the data
# * teamcolors     - colors that match the team name
# * q95            - 95-quantile function
# * q99            - 99-quantile function
# CHAINS
# CORES
# ITERATIONS
# THREADS
# ADAPT_DELTA
# SAVE_PARS



df <- read.csv("samples/authors-team-impact.csv")

data_logscaled <- df %>% mutate(repo=as.factor(repo),
                             file=as.factor(file),
                             ISTEST=istestfile,
                             ISNEW=isnewfile,
                             author=as.factor(author),
                             authorteam=as.factor(authorteam),
                             committer=as.factor(committer),
                             committerteam=as.factor(committerteam),
                             ADD=added,
                             DEL=removed,
                             REASON=as.factor(changereason),
                             CLOC=currCloc,
                             COMPLEX=currComplex,
                             DUP=prevDupBlocks, # duplicates prior to the change
                             INTROD=if_else(delta >= 0, delta, as.integer(0)),
                             REMOVED=if_else(delta <= 0, abs(delta), as.integer(0)),
                             logADD=log(added+1),
                             logDEL=log(removed+1),
                             logCOMPLEX=log(currComplex+1),
                             logDUP=log(prevDupBlocks+1)) %>%
  select(repo, file, ISTEST, ISNEW, author, authorteam, committer, committerteam, ADD, DEL, REASON, CLOC, COMPLEX, DUP, logADD, logDEL, logCOMPLEX, logDUP, INTROD, REMOVED)

data <- data_logscaled |> mutate(A = scale(logADD)[,1],
                             R = scale(logDEL)[,1],
                             C = scale(logCOMPLEX)[,1],
                             D = scale(logDUP)[,1])

# adapt these to fit your machine and willingness to wait for results. Models are cached, you might need to remove the cache in order to rebuild if these values are changed.
CHAINS <- 4
CORES <- 2
ITERATIONS <- 4000
THREADS <- 4
ADAPT_DELTA <- 0.95
SAVE_PARS <- save_pars(all = FALSE)

teams <- data |> select(committerteam) |> distinct()
repos <- data |> select(repo) |> distinct()

rgb_teamcolors <- c(
"Arch"="166,206,227",
"Blue"="31,120,180",
"Brown"="177,89,40",
"Green"="51,160,44",
"Orange"="255,127,0",
"Pink"="251,154,153",
"QA"="177,177,177",
"Red"="227,26,28",
"TestArch"="153,153,153",
"UI"="253,191,111",
"UX"="178,223,138",
"Violet"="106,61,154",
"Yellow"="255,255,153",
"Unknown"="202,178,214"
)

teamcolors <- sapply(strsplit(rgb_teamcolors, ","), function(x) rgb(x[1], x[2], x[3], maxColorValue=255))

q95 <- function(y) quantile(y, 0.95)
q99 <- function(y) quantile(y, 0.99)
