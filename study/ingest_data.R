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



df <- read.csv("samples/authors-team-impact.csv") |> mutate(repo=as.factor(repo), 
                                                            commit=as.factor(commitid),
                                                            isremovedfile=if_else(currCloc == 0 & prevCloc > 0, TRUE, FALSE),
                                                            addedComplexity=if_else(currComplex > prevComplex, currComplex-prevComplex, 0),
                                                            removedComplexity=if_else(currComplex < prevComplex, abs(currComplex-prevComplex), 0),
                                                            INTROD=if_else(delta >= 0, delta, as.integer(0)),
                                                            REMOVED=if_else(delta <= 0, abs(delta), as.integer(0)))
percommit <- df |> group_by(commit) |> summarise(commitAdd=sum(added),
                                                 commitRemoved=sum(removed),
                                                 commitCurrComplex=sum(currComplex),
                                                 commitPrevComplex=sum(prevComplex),
                                                 commitAddComplex=sum(addedComplexity),
                                                 commitDelComplex=sum(removedComplexity),
                                                 commitINTROD=sum(INTROD),
                                                 commitREMOVED=sum(REMOVED)
                                                 )
df <- merge(df, percommit)

data_raw <- df %>% mutate(repo=as.factor(repo),
                            commit=as.factor(commitid),
                             file=as.factor(file),
                             ISTEST=istestfile,
                             ISNEW=isnewfile,
                          ISREMOVED=isremovedfile,
                             author=as.factor(author),
                             authorteam=as.factor(authorteam),
                             committer=as.factor(committer),
                             committerteam=as.factor(committerteam),
                             churn=pmax(added, removed), 
                             ADD=added,
                             DEL=removed,
                             cADD=commitAdd,
                             cDEL=commitRemoved,
                             cSIZE=commitAdd+commitRemoved,
                             cINTROD=commitINTROD-INTROD,  # introduced duplicates in other files in the same commit
                             cREMOVED=commitREMOVED-REMOVED, # removed duplicates in other files in the same commit
                             
                             REASON=as.factor(changereason),
                             CLOC=currCloc,
                             COMPLEX=currComplex,
                             DUP=prevDupBlocks, # duplicates prior to the change
                             logcADD=log(commitAdd+1),
                             logcDEL=log(commitRemoved+1),
                             logcSIZE=log(cSIZE+1),
                          logcADDCOMPLEX=log(commitAddComplex+1),
                          logcDELCOMPLEX=log(commitDelComplex+1),
                          logcCURRCOMPLEX=log(commitCurrComplex+1),
                          logcREMOVED=log(cREMOVED+1),
                             logADDCOMPLEX=log(addedComplexity+1),
                             logDELCOMPLEX=log(removedComplexity+1),
                             logCOMPLEXDELTA=log(addedComplexity+removedComplexity+1),
                             logADD=log(added+1),
                             logDEL=log(removed+1),
                             logCOMPLEX=log(currComplex+1),
                             logDUP=log(prevDupBlocks+1))
data_logscaled <- data_raw |> select(repo, commit, file, ISTEST, ISNEW, ISREMOVED,
                                     author, authorteam, committer, committerteam, 
                                     churn, addedComplexity, removedComplexity, 
                                     cADD, cDEL, cSIZE, cINTROD, cREMOVED,
                                     ADD, DEL, REASON, CLOC, COMPLEX, DUP, 
                                     logcADD, logcDEL, logcSIZE, logcREMOVED,
                                     logADD, logDEL, logCOMPLEX, logDUP, 
                                     INTROD, REMOVED)

data <- data_logscaled |> mutate(A = scale(logADD)[,1],
                             R = scale(logDEL)[,1],
                             C = scale(logCOMPLEX)[,1],
                             scaleDUP=scale(DUP)[,1],
                             D = scale(logDUP)[,1],
                             cA = scale(logcADD)[,1],
                             cR = scale(logcDEL)[,1],
                             cSZ = scale(logcSIZE)[,1],
                             cREM = scale(cREMOVED)[,1],
                             cREMlog = scale(logcREMOVED)[,1]
)

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
  NewTeam=rgb(202,178,214, maxColorValue = 255),
  DAT=rgb(166,206,227, maxColorValue = 255),
  CFT24=rgb(31,120,180, maxColorValue = 255),
  CFT84=rgb(177,89,40, maxColorValue = 255),
  CFT23=rgb(51,160,44, maxColorValue = 255),
  CFT53=rgb(255,127,0, maxColorValue = 255),
  CFT55=rgb(251,154,153, maxColorValue = 255),
  LSV=rgb(177,177,177, maxColorValue = 255),
  CFT22=rgb(227,26,28, maxColorValue = 255),
  TQA=rgb(153,153,153, maxColorValue = 255),
  UIx=rgb(253,191,111, maxColorValue = 255),
  UXx=rgb(178,223,138, maxColorValue = 255),
  CFT54=rgb(106,61,154, maxColorValue = 255),
  CFT25=rgb(255,237,111, maxColorValue = 255),
  Unknownx=rgb(202,178,214, maxColorValue = 255),
  NewTeamx=rgb(202,178,214, maxColorValue = 255)
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

#teamcolors <- sapply(strsplit(rgb_teamcolors, ","), function(x) rgb(x[1], x[2], x[3], maxColorValue=255))

teamcolors <- COLOR_BY_TEAM

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
    group_by(committerteam) |> group_by(repo) |> mutate(Grankcommits=rank(-ncommits, ties.method = "min"),
                                                        Frankchurn=rank(-sumchurn, ties.method = "min"),
                                                        Erankaddcomplex=rank(-sumaddcomplex, ties.method = "min"),
                                                        Drankdelcomplex=rank(-sumdelcomplex, ties.method = "min")
    )
}

ocam_labels <- c("Arankpercommitdelcomplex"=expression(rho (frac(COMP[DEL],N))), 
                 "Crankpercommitchurn"=expression(rho (frac(CHURN,N))),
                 "Brankpercommitaddcomplex"=expression(rho(frac(COMP[ADD], N))),
                 "Drankdelcomplex"=expression(rho(COMP[DEL])),
                 "Grankcommits"=expression(rho ( N ) ),
                 "Frankchurn"=expression(rho(CHURN)),
                 "Erankaddcomplex"=expression(rho(COMP[ADD]))
)

ocam_labels_plain <- c(
                 "Drankdelcomplex"="REM_COMP",
                 "Grankcommits"="NCOMMIT",
                 "Frankchurn"="CHURN",
                 "Erankaddcomplex"="ADD_COMP"
)

ocam_rank_repo <- function(df) {
  df |> pivot_longer(cols=c("Grankcommits", "Frankchurn", "Erankaddcomplex", "Drankdelcomplex"), values_to = "metric") |> 
    group_by(repo) |> ggplot(aes(x=committerteam, y=name)) + geom_tile(aes(fill=metric)) + facet_wrap(~ repo, ncol=1) + 
    geom_text(aes(label=metric)) + xlab("team") + ylab("metric") + coord_fixed() + scale_y_discrete(labels=ocam_labels_plain) + 
    scale_fill_gradient2(name="team rank", guide=guide_legend(reverse=T), midpoint=6, low = "#91BFDB", mid = "#FFFFBF", high = "#FC8D59")  + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.25)) + theme(legend.position = "none") #+ ggtitle(paste("OCAM rank for repo", repo$repo))
}

# https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
roundUpNice <- function(x, nice=1:10) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

figsave <- function(file, p, width=15, height=17.8, units="cm") ggsave(file, p, device = "pdf", dpi = 1200, width=width, height=height, units=units)

pngsave <- function(file, p, width=15, height=17.8, units="cm") ggsave(file, p, device = "png", dpi = 1200, width=width, height=height, units=units)
