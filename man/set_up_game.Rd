% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_up_game.R
\name{set_up_game}
\alias{set_up_game}
\title{Set-up a new game}
\usage{
set_up_game(players = 4)
}
\arguments{
\item{players}{number of players, an integer, defaults to 4}
}
\description{
This function creates a tibble detailing game start information. This tibble can be assigned a name and used as an input into take_turn()
}
\examples{
\dontrun{
set_up_game(players = 2)
}
}
