## Handy functions by Olga
# 28-8-2023
# Run these functions to quickly run models

# Logistic regression: many xs, one y #########################################################

run_logreg <- function(df, xvars, y, terms){

  # make objects to store outcomes
  xs <- c()
  ys <-c()
  covmod <- c()
  bs <- c()
  dfs <- c()
  betas <- C()
  ses <-c()
  ORs <- c()
  ORciLs <- c()
  ORciUs <- c()
  ps <- c()
  fdr_ps <- c()

  for (x in xvars) {
    # create formula
    termnames <- paste(terms, collapse = " + ")
    fit_eq <- paste0(y, "~", x, "+", termnames)
    print(fit_eq)
    covmod <- c(covmod, termnames)

    # run logistic model
    model <- with(df, stats::glm(stats::as.formula(fit_eq), family = binomial(link = "logit")))
    #print(summary(mice::pool(model)))

    # extract summary of x, always on second row
    xs <- c(xs, x)

    b <- summary(mice::pool(model))[2, "estimate"]
    bs <- c(bs, b)

    se <- summary(mice::pool(model))[2, "std.error"]
    ses <- c(ses, se)

    p <- summary(mice::pool(model))[2, "p.value"]
    ps <- c(ps, p)

    # calculate betas
    # grab the complete dataset and put in list
    for(i in 1:df$m){
      dfs[[i]] <- mice::complete(df, i)
    }

    impBetas <- c()
    for (imp in 1:df$m){
      tmpDf <- mice::complete(df, i)
      xsd <- sd(tmpDf[,x], na.rm=TRUE)
      ysd <- sd(tmpDf[,y], na.rm=TRUE)
      impBetas <- c(impBetas, (b*xsd)/ysd)
    }
    beta <- mean(impBetas)
    betas <- c(betas, beta)

    # compute ORs and CIs
    OR <- exp(beta)
    ORs <- c(ORs, OR)

    ORciL <- exp(beta - 1.96 * se)
    ORciLs <- c(ORciLs, ORciL)

    ORciU <- exp(beta + 1.96 * se)
    ORciUs <- c(ORciUs, ORciU)

  }

  fdr_ps <- stats::p.adjust(ps, method = 'fdr') # FDR correction is now for every analysis separate
  final_results <- data.frame(xs, y, covmod, bs, ses, ORs, ORciLs, ORciUs, ps, fdr_ps)

  # return it to call
  return(final_results)

}


# Linear regression, many xs, one y ###########################################################

#' Quickresults
#'
#' @param df mids object multiple imputed dataset containing exposure, outcome and covariates
#' @param xvars independent variables
#' @param y dependent variable
#' @param terms covariates/confounders
#'
#' @return matrix including estimate, beta, se, t, p and FDR-corrected p
#' @export
#'
run_linreg <- function(df, xvars, y, terms){

  # make objects to store outcomes
  xs <- c()
  ys <-c()
  covmod <- c()
  bs <- c()
  ses <-c()
  ts <- c()
  ps <- c()
  betas <- c()
  fdr_ps <- c()
  dfs <- list()

  for (x in xvars) {
    # create formula
    termnames <- paste(terms, collapse = " + ")
    fit_eq <- paste0(y, "~", x, "+", termnames)
    print(fit_eq)
    covmod <- c(covmod, termnames)

    # run logistic model
    model <- with(df, stats::lm(stats::as.formula(fit_eq)))
    print(summary(mice::pool(model)))

    # extract summary of x, always on second row
    xs <- c(xs, x)

    b <- summary(mice::pool(model))[2, "estimate"]
    bs <- c(bs, b)

    se <- summary(mice::pool(model))[2, "std.error"]
    ses <- c(ses, se)

    t <- summary(mice::pool(model))[2, "statistic"]
    ts <- c(ts, t)

    p <- summary(mice::pool(model))[2, "p.value"]
    ps <- c(ps, p)

    # Calculate beta (compute one for each imp and take mean)
    # Complete each dataset and put in list
    for(i in 1:df$m){
      dfs[[i]] <- mice::complete(df, i)
    }

    # Calculate beta per imp dataset and put in list
    impBetas <- c()
    for (imp in 1:df$m){
      tmpDf <- mice::complete(df, i)
      xsd <- sd(tmpDf[,x], na.rm=TRUE)
      ysd <- sd(tmpDf[,y], na.rm=TRUE)
      impBetas <- c(impBetas, (b*xsd)/ysd)
    }
    beta <- mean(impBetas)
    betas <- c(betas, beta)

  }

  fdr_ps <- p.adjust(ps, method = 'fdr') # FDR correction is now for every analysis separate
  final_results <- data.frame(xs, y, covmod, bs, betas, ses, ts, ps, fdr_ps)
  #save it out to whatever called this function
  return(final_results)

}

make_groups <- function(names) {

  # shuffle names
  names_shuffled <- sample(names)

  # arrange names in a two-column matrix
  names_coupled <- matrix(names_shuffled, ncol = 2)

  # has to return names coupled
  return(names_coupled)

}

make_groups_by_time <- function(names){
  # input - character vector containing names
  # output - dataframe with re-arranged names in groups by time

  # step 1. make groups (using previously defined function)
  groups <- data.frame(make_groups(names))

  # step 2. rename the columns of groups
  names(groups) <- c("person1", "person2")

  # step 3. decide the times when people can meet
  possible_times <- c("09:30", "10:00", "14:00", "16:30")

  # step 4. combine possible times with groups
  groups_by_time <- dplyr::mutate(groups,
                                  time = sample(possible_times,
                                                size = nrow(groups),
                                                replace = TRUE)
  )

  # step 5. return output
  return(groups_by_time)
}
