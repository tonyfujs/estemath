#' Aide memoire pour reviser les tables de multiplications
#'
#' @param tables numeric: Les tables que l'on veut reviser
#' @param repetitions numeric: Le nombre de multiplications dont on doit trouver la reponse
#'
#' @return

revise_multiplications <- function(tables = c(1:10),
                                   repetitions = 12) {

  tictoc::tic()
  out <- vector(mode = "logical", length = repetitions)

  counter <- 1
  while (counter <= repetitions) {
    out[counter] <- revise_multiplication(vn = tables)
    counter <- counter + 1
  }

  tictoc::toc()
  perf <- sum(out) / repetitions

  return(cat(round(perf * 100, 0), "% de bonnes reponses!!"))
}

#' Helper to revise multiplications
#'
#' @param vn numeric: vector of numbers to be used for multiplications
#'
#' @return
#' @export
#'
#' @examples
revise_multiplication <- function(vn) {

  # Pick two numbers
  n1 <- sample(vn, size = 1)
  n2 <- sample(0:10, size = 1)

  # Create question
  question <- paste(n1, " x ", n2)

  # Ask for repsonse
  resp <- readline(prompt = paste(question, " = "))

  # Check response
  success <- as.numeric(resp) == n1*n2
  if (success) {
    message(sample(c("Bravo!",
                     "Super!",
                     "Tu es le meilleur!",
                     "",
                     "Champion du monde!!",
                     "Yoooouhoooou!!"), 1))
  } else {
    message(sample(c("Encore un petit effort!",
                     "Tu y es presque :-)",
                     "Aie aie aie...",
                     ""), 1))
  }
  invisible(success)
}

