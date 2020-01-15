#' @export
#' @title List Valid Data Pack Disaggs
#' 
#' @description
#' Lists valid Data Pack disaggs
#' 
#' @return Data list object of valid Data Pack disaggs, by sheet name.
#'
validDPDisaggs <- function() {
  
  validDisaggs <- list(
    "Epi Cascade I" = list(
      validAges = c("<01","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
      validSexes = c("Female","Male"),
      validKPs = NA_character_),
    "Epi Cascade II" = list(
      validAges = c("<15","15+"),
      validSexes = c("Female","Male"),
      validKPs = NA_character_),
    "Epi PMTCT" = list(
      validAges = NA_character_,
      validSexes = NA_character_,
      validKPs = NA_character_),
    "Prioritization" = list(
      validAges = NA_character_,
      validSexes = NA_character_,
      validKPs = NA_character_),
    "PMTCT_STAT_ART" = list(
      validAges = c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
      validSexes = c("Female"),
      validKPs = NA_character_),
    "PMTCT_EID" = list(
      validAges = NA_character_,
      validSexes = NA_character_,
      validKPs = NA_character_),
    "TB_STAT_ART" = list(
      validAges = c("<01","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
      validSexes = c("Female","Male"),
      validKPs = NA_character_),
    "VMMC" = list(
      validAges = c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
      validSexes = c("Male"),
      validKPs = NA_character_),
    "TX" = list(
      validAges = c("<01","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
      validSexes = c("Female","Male"),
      validKPs = NA_character_),
    "CXCA" = list(
      validAges = c("25-29","30-34","35-39","40-44","45-49"),
      validSexes = c("Female"),
      validKPs = NA_character_),
    "HTS" = list(
      validAges = c("01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
      validSexes = c("Female","Male"),
      validKPs = NA_character_),
    "TB_TX_PREV" = list(
      validAges = c("<15","15+"),
      validSexes = c("Female","Male"),
      validKPs = NA_character_),
    "OVC" = list(
      validAges = c("<01","01-04","05-09","10-14","15-17","18+"),
      validSexes = c("Female","Male"),
      validKPs = NA_character_),
    "KP" = list(
      validAges = NA_character_,
      validSexes = NA_character_,
      validKPs = c("Female PWID","Male PWID","PWID","FSW","MSM not SW","MSM SW","MSM","People in prisons and other enclosed settings","TG SW","TG not SW","TG")),
    "PP" = list(
      validAges = c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
      validSexes = c("Female","Male"),
      validKPs = NA_character_),
    "PrEP" = list(
      validAges = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
      validSexes = c("Female","Male"),
      validKPs = NA_character_),
    "GEND" = list(
      validAges = NA_character_,
      validSexes = NA_character_,
      validKPs = NA_character_)
  )
  
  return(validDisaggs)
}
