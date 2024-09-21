# Example 3.1
# Chapter 3
# Decision Making Criteria with Payoff Matrix

## Setup Payoff Matrix
payoff <- matrix(c(-500,700,2200,  # profits for stocks
                   -100,600,900,  # profits for bonds
                   300,500,750,   # profits for CDs
                   -200,650,1300), # profits for mixture
                 nrow = 4, byrow = TRUE) 

# Assign row and column names
dimnames(payoff)[[1]] = c("Stocks", "Bonds", "CDs", "Mixture") # rownames
dimnames(payoff)[[2]] = c("Stagnant", "Slow Growth", "Rapid Growth") # colnames
prob = c(0.25, 0.45, 0.3) # probabilities for each state of nature
payoff # display the payoff matrix

# 1. Maximax Criterion
rowmax = apply(payoff, 1, max); rowmax # maximum value in each row
max(rowmax) # maximum of rowmax
gr1 = which(rowmax == max(rowmax)); gr1 # index of the maximax strategy
maximax = rownames(payoff)[gr1] # name of the maximax strategy
print(c("Maximax solution", maximax))

# 2. Maximin Criterion
rowmin = apply(payoff, 1, min); rowmin # minimum value in each row
max(rowmin) # maximum of rowmin
gr1 = which(rowmin == max(rowmin)); gr1 # index of the maximin strategy
maximin = rownames(payoff)[gr1] # name of the maximin strategy
print(c("Maximin choice", maximin))

# 3. Hurwicz Criterion
alpha = 0.2 # Hurwicz coefficient (between 0 and 1)
hurz = rowmax * alpha + rowmin * (1 - alpha); hurz # Hurwicz weighted result
max(hurz) # maximum Hurwicz value
gr1 = which(hurz == max(hurz)); gr1 # index of Hurwicz strategy
hurwicz = rownames(payoff)[gr1] # name of the Hurwicz strategy
print(c("Hurwicz solution", hurwicz))

# 4. Equally Likely Criterion
rowavg = apply(payoff, 1, mean); rowavg # average payoff for each strategy
max(rowavg) # maximum average payoff
gr1 = which(rowavg == max(rowavg)); gr1 # index of the equally likely strategy
equally.like = rownames(payoff)[gr1] # name of the equally likely strategy
print(c("Equally likely solution", equally.like))

# 5. Minimax Regret Criterion
colmax = apply(payoff, 2, max); colmax # maximum payoff for each state of nature
U1r = colmax[1] - payoff[,1]; U1r # regret for Stagnant
U2r = colmax[2] - payoff[,2]; U2r # regret for Slow Growth
U3r = colmax[3] - payoff[,3]; U3r # regret for Rapid Growth
regret = cbind(U1r, U2r, U3r) # combine the regrets
dimnames(regret)[[2]] = c("Stagnant", "Slow Growth", "Rapid Growth") # column names for regret table
regret # display the regret matrix
rrowmax = apply(regret, 1, max); rrowmax # maximum regret for each strategy
min(rrowmax) # minimum of the maximum regrets
gr1 = which(rrowmax == min(rrowmax)); gr1 # index of the minimax regret strategy
minimax.regret = rownames(payoff)[gr1] # name of the minimax regret strategy
print(c("Minimax regret solution", minimax.regret))

# 6. EMV (Expected Monetary Value)
expected.value = payoff %*% prob; expected.value # expected value for each strategy
maxev = max(expected.value); maxev # maximum expected value
gr1 = which(expected.value == max(expected.value)); gr1 # index of the max EMV strategy
max.Exp.Value = rownames(payoff)[gr1] # name of the max EMV strategy
print(c("Maximum Expected Value", max.Exp.Value))

# 7. EOL (Expected Opportunity Loss)
expected.regret = regret %*% prob; expected.regret # expected regret for each strategy
miner = min(expected.regret); miner # minimum expected regret
gr1 = which(expected.regret == min(expected.regret)); gr1 # index of the min EOL strategy
min.Exp.Regret = rownames(payoff)[gr1] # name of the min EOL strategy
print(c("Minimum Expected Regret", min.Exp.Regret))

# 8. EVPI (Expected Value of Perfect Information)
max.certain = t(prob) %*% colmax; max.certain # maximum value under certainty
cost.of.uncertainty = max.certain - max(expected.value); cost.of.uncertainty # EVPI
print(c("EVPI (Cost of Uncertainty)", cost.of.uncertainty))

# Summary of Decisions by Various Criteria
decisions = cbind(rbind(maximax, maximin, hurwicz, equally.like, minimax.regret, max.Exp.Value, min.Exp.Regret),
                  rbind(max(rowmax), max(rowmin), max(hurz), max(rowavg), min(rrowmax), maxev, miner))
dimnames(decisions)[[2]] = c("Strategy", "Value")
print("Summary of decisions by various criteria:")
print(decisions)
