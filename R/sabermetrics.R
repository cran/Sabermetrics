## Title: sabermetrics
## Author: Peter Xenopoulos
## Email: peter@peterxeno.com
## Website: www.peterxeno.com
## Description: sabermetrics contains a series of sabermetrics functions

# Slugging Percentage
slg <- function(TB, AB) {
  slugging <- (TB/AB)
  return(slugging)
}

# On-base Percentage
obp <- function(H, BB, HBP, AB, SF) {
  onbase <- ((H+BB+HBP)/(AB+BB+SF+HBP))
  return(onbase)
}

# On-base plus Slugging
ops <- function(slg, obp) {
  ops <- slg+obp
  return(ops)
}

# Isolated Power
iso <- function(slg, avg) {
  iso <- slg-avg
  return(iso)
}

# Equivalent Average
eqa <- function(H, TB, BB, HBP, SB, SAC, SF, AB, CS) {
  eqa <- (H+TB+1.5*(BB+HBP)+SB+SAC+SF)/(AB+BB+HBP+SAC+SF+CS+(SB/3))
  return(eqa)
}

# Log5
# 0 = A defeats B | 1 = B defeats A
log5 <- function(probA, probB, order) {
  if(order == 0) {
    prob <- (probA - (probA*probB)) / (probA + probB - (2 * probA * probB))
  }
  if(order == 1) {
    prob <- (probB - (probB*probA)) / (probB + probA - (2 * probB * probA))
  }
  else {
    prob <- (probA - (probA*probB)) / (probA + probB - (2 * probA * probB))
  }
  return(prob)
}

# Pythagorean Expectation
pyth <- function(RS, RA) {
  pythEx <- (RS*RS)/((RS*RS)+(RA*RA))
  return(pythEx)
}

# Runs Created (Basic)
rcBasic <- function(H, BB, TB, AB) {
  rc <- ((H+BB)*TB)/(AB+BB)
  return(rc)
}

# Runs Created (Basic + SB)
rcBasicSB <- function(H, BB, TB, AB, CS, SB) {
  rc <- ((H+BB-CS)*(TB+(0.55*SB)))/(AB+BB)
  return(rc)
}

# Runs Created (Technical)
rcTech <- function(H, BB, CS, HBP, GIDP, TB, IBB, SAC, SF, SB) {
  rc <- (H+BB-CS+HBP-GIDP)*(TB + (0.26*(BB-IBB+HBP)) + (0.52*(SAC+SF+SB)))
  return(rc)
}

# Runs Created using past three MLB season data
rcPX <- function(SINGLES, DOUBLES, TRIPLES, HR, BB, SB) {
  rc <- (-391.39753 + 0.44953*(SINGLES) + 0.85285*(DOUBLES) + 1.05912*(TRIPLES) + 1.36359*(HR) + 0.31761*(BB) + 0.21599*(SB))
  return(rc)
}

# wOBA (as appeared in "The Book")
wOBA <- function(BB, HBP, SINGLE, RBOE, DOUBLE, TRIPLE, HR, PA) {
  woba <- ((0.72*BB)+(0.75*HBP)+(0.90*SINGLE)+(0.92*RBOE)+(1.24*DOUBLE)+(1.56*TRIPLE)+(1.95*HR))/PA
  return(woba)
}

# Secondary Average
secA <- function(BB, TB, H, SB, CS, AB) {
  avg <- (BB+(TB-H)+(SB-CS))/AB
  return(avg)
}

# Defense-Independent Component ERA (DICE)
dice <- function(HR, BB, HBP, K, IP) {
  defenseERA <- 3 + ((13*HR+3*BB+3*HBP-2*K)/IP)
  return(defenseERA)
}

# Field Independent Pitching
fip <- function(HR, BB, K, IP, C) {
  fieldIndPitch <- ((13*HR+3*BB-2*K)/IP) + C
  return(fieldIndPitch)
}