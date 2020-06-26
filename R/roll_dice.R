#' Roll dice
#'
#' Simulate rolling 2x 6-sided dice
#' @examples
#' \dontrun{
#' roll_dice()
#' }
#' @export
#' roll_dice

roll_dice <- function(){

  sample(6, 2, TRUE)

}
