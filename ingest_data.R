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
                                commit=as.factor(commitid),
                             file=as.factor(file),
                             ISTEST=istestfile,
                             ISNEW=isnewfile,
                             author=as.factor(author),
                             authorteam=as.factor(authorteam),
                             committer=as.factor(committer),
                             committerteam=as.factor(committerteam),
                             churn=pmax(added, removed), 
                             addedComplexity=if_else(currComplex > prevComplex, currComplex-prevComplex, 0),
                             removedComplexity=if_else(currComplex < prevComplex, abs(currComplex-prevComplex), 0),
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
  select(repo, commit, file, ISTEST, ISNEW, author, authorteam, committer, committerteam, churn, addedComplexity, removedComplexity, 
         ADD, DEL, REASON, CLOC, COMPLEX, DUP, logADD, logDEL, logCOMPLEX, logDUP, INTROD, REMOVED)

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

COLOR_BY_TEAM <- c(
  Arch=rgb(166,206,227, maxColorValue = 255),
  Blue=rgb(31,120,180, maxColorValue = 255),
  Brown=rgb(177,89,40, maxColorValue = 255),
  Green=rgb(51,160,44, maxColorValue = 255),
  Orange=rgb(255,127,0, maxColorValue = 255),
  Pink=rgb(251,154,153, maxColorValue = 255),
  QA=rgb(177,177,177, maxColorValue = 255),
  Red=rgb(227,26,28, maxColorValue = 255),
  TestArch=rgb(153,153,153, maxColorValue = 255),
  UI=rgb(253,191,111, maxColorValue = 255),
  UX=rgb(178,223,138, maxColorValue = 255),
  Violet=rgb(106,61,154, maxColorValue = 255),
  Yellow=rgb(255,237,111, maxColorValue = 255),
  Unknown=rgb(202,178,214, maxColorValue = 255),
  NewTeam=rgb(202,178,214, maxColorValue = 255)
)


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
"Yellow"="255,237,111",
"Unknown"="202,178,214"
)

teamcolors <- sapply(strsplit(rgb_teamcolors, ","), function(x) rgb(x[1], x[2], x[3], maxColorValue=255))

q25 <- function(y) quantile(y, 0.25, names=FALSE)
q50 <- function(y) quantile(y, 0.50, names=FALSE)
q75 <- function(y) quantile(y, 0.75, names=FALSE)
q95 <- function(y) quantile(y, 0.95, names=FALSE)
q99 <- function(y) quantile(y, 0.99, names=FALSE)

scale_added <- function(x) {
  return((log(x+1)-mean(data$logADD))/sd(data$logADD))
}

scale_removed <- function(x) {
  return((log(x+1)-mean(data$logDEL))/sd(data$logDEL))
}

scale_complexity <- function(x) {
  return((log(x+1)-mean(data$logCOMPLEX))/sd(data$logCOMPLEX))
}

scale_duplicates <- function(x) {
  return((log(x+1)-mean(data$logDUP))/sd(data$logDUP))
}


unscale_added <- function(x) {
  return(round(exp(x*sd(data$logADD)+mean(data$logADD))-1))
}

unscale_removed <- function(x) {
  return(round(exp(x*sd(data$logDEL)+mean(data$logDEL))-1))
}

unscale_complexity <- function(x) {
  return(round(exp(x*sd(data$logCOMPLEX)+mean(data$logCOMPLEX))-1))
}

unscale_duplicates <- function(x) {
  return(round(exp(x*sd(data$logDUP)+mean(data$logDUP))-1))
}

ocam_metrics <- function(df) {
  df |> group_by(repo, committerteam) |> summarize(ncommits=n_distinct(commit),
                                                   sumchurn=sum(churn),
                                                   sumaddcomplex=sum(addedComplexity),
                                                   sumdelcomplex=sum(removedComplexity)) |> 
    ungroup() |> complete(repo, committerteam, fill=list(ncommits=0, sumchurn=0, sumaddcomplex=0, sumdelcomplex=0)) |> 
    mutate(relchurn=if_else(ncommits==0,0,sumchurn/ncommits),
           reladdcomplex=if_else(ncommits==0,0,sumaddcomplex/ncommits),
           reldelcomplex=if_else(ncommits==0,0,sumdelcomplex/ncommits)) |>
    group_by(committerteam) |> group_by(repo) |> mutate(rankcommits=rank(-ncommits, ties.method = "min"),
                                                        rankchurn=rank(-sumchurn, ties.method = "min"),
                                                        rankaddcomplex=rank(-sumaddcomplex, ties.method = "min"),
                                                        rankdelcomplex=rank(-sumdelcomplex, ties.method = "min"),
                                                        rankrelchurn=rank(-relchurn, ties.method = "min"),
                                                        rankreladdcomplex=rank(-reladdcomplex, ties.method = "min"),
                                                        rankreldelcomplex=rank(-reldelcomplex, ties.method = "min")
    )
}

ocam_rank_repo <- function(df) {
  repo <- df |> select(repo) |> distinct()
  stopifnot(length(repo$repo) == 1)
  df |> pivot_longer(cols=c("rankcommits", "rankchurn", "rankaddcomplex", "rankdelcomplex", "rankrelchurn", "rankreladdcomplex", "rankreldelcomplex"), values_to = "metric") |> 
    ggplot(aes(x=committerteam, y=name)) + geom_tile(aes(fill=metric)) + 
    geom_text(aes(label=metric), color="white") + xlab("team") + ylab("metric") + ggtitle(paste("OCAM rank for repo", repo$repo))
}


# https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
roundUpNice <- function(x, nice=1:10) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

figsave <- function(file, p, width=15, height=17.8, units="cm") ggsave(file, p, device = "pdf", dpi = 1200, width=width, height=height, units=units)
