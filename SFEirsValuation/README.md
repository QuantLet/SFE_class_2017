[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFEirsValuation** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet:   SFEirsValuation

Published in:       Statistics of Financial Markets 1

Description:        Calculates the value of an IRS based on a Bond, FRA and Forward Rate approach.

Keywords:           finance, derivative, interest rates, swaps

Author:             Marvin Gauer, Laureen Lake

See also:           ''

Submitted:          Tue, December 19 2017 by Marvin Gauer

Input:              'Fixed Interest Rate, Yield-Curve and the Principal of the contract'

Output:             'Swap Valuations'

```

### R Code
```r

##### SFM1 - Project -> Compare the three different ways to value an IRS

# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("gridExtra", "grid", "gtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

######## 1. Input Parameters

Rfix     = 0.075  # Fix Rate
# Maturities and the corresponding yields
Maturity = c(0.0, 0.5, 1.0, 1.5, 2.0)
Yields   = c(0, 0.06185567, 0.06382979, 0.06593407, 0.07471264)  # Yields corresponding to the maturities

y        = data.frame(Maturity, Yields)  # zero-bond-yield-curve

P        = 10  # Principal

######## 2. Valuation

######## 2.1. Helper Functions

# Value of a fixed Coupon Bond
Bfix = function(yields, Coupon_Anually, P = 1) {

    D                  = sapply(1:(nrow(yields)), function(x) (yields[x, 2] * yields[x, 1] + 1)^(-1))  # Discountfactors
    PayDiff            = c(0, diff(yields[, 1]))  # Time between Paymemnts
    BondPrice          = (D %*% (PayDiff * Coupon_Anually) + D[length(D)]) * P  #Final Price of the Bond
    NPVs               = P * D * (PayDiff * Coupon_Anually)  # Net Present Value of the single coupon payments
    NPVs[length(NPVs)] = NPVs[length(NPVs)] + D[length(D)] * P  # Net Present Value of all payments
    
    return(list(BondPrice = BondPrice, PV = NPVs))
}

# Value of a Forward Rate Agreement Si<Ti and both are the times of the yield
FRA = function(Rfix, yields, Si, Ti, P = 1) {
    
    DSi = ((yields[, 2][yields[, 1] == Si] * Si) + 1)^(-1)  # Discountfactor corresponding to Si
    DTi = ((yields[, 2][yields[, 1] == Ti] * Ti) + 1)^(-1)  # Discountfactor corresponding to Ti
    FRA = (DTi * (Ti - Si) * Rfix + DTi - DSi) * P  # Price of the FRA
    
    return(FRA)
}

# Simple compounding Forward Rates implied by the Yield Curve Si<Ti and both are
# the times of the yield
ForwardRates = function(yields, Si, Ti) {
    
    DSi = ((yields[, 2][yields[, 1] == Si] * Si) + 1)^(-1)  # Discountfactor corresponding to Si
    DTi = ((yields[, 2][yields[, 1] == Ti] * Ti) + 1)^(-1)  # Discountfactor corresponding to Ti
    FR  = (1/(Ti - Si)) * ((DSi/DTi) - 1)  # Forward Rate for time Si to Ti
    
    return(FR)
}

######## 2.2. Valuation in Terms of Bond Prizes

VSwapB   = as.numeric(Bfix(y, Rfix, P)$BondPrice - P)

######## 2.3. Valuation in Terms of FRA Prizes

VSwapFRA = sum(unlist(lapply(1:(nrow(y) - 1), function(x) FRA(Rfix, y, y[x, 1], y[x + 1, 1], P))))

######## 2.4. Valuation in Terms of discounted Cash Flows

DCF    = P * c(0, as.numeric((sapply(2:(nrow(y)), function(x) (y[x, 2] * y[x, 1] + 1)^(-1)) * 
         diff(y[, 1])) * (Rfix - sapply(1:(nrow(y) - 1), function(x) ForwardRates(y, y[x, 1], 
         y[x + 1, 1])))))
VSwapR = sum(DCF)

######## 3. Results

Results = data.frame(`Maturity` = c(format(round(Maturity,2), nsmall = 1), "Results:"), 
                     `Fixed Bond Cash Flows` = round(c(Bfix(y, Rfix, P)$PV, Bfix(y, Rfix, P)$BondPrice), 5),
                     `Floating Rate Note` = c(P, rep("", length(Maturity) - 1), P), 
                     `Bond based Value` = c(rep("", length(Maturity)), round(Bfix(y, Rfix, P)$BondPrice - P, 5)), 
                     `FRA Value` = round(c(0, unlist(lapply(1:(nrow(y) - 1), function(x) FRA(Rfix, y, y[x, 1], y[x + 1, 1], P))), VSwapFRA), 5), 
                     DCF = round(c(DCF, sum(DCF)), 5))


g          = tableGrob(head(Results, n = nrow(Results)), theme = ttheme_minimal(), rows = NULL)

separators = replicate(ncol(g) - 1, segmentsGrob(x1 = unit(0, "npc"), gp = gpar(lty = 2)), simplify = FALSE)

# add vertical lines on the left side of columns (after 2nd)
g = gtable::gtable_add_grob(g, grobs = separators, t = 1, b = nrow(g), l = seq_len(ncol(g) - 1) + 1)

g = gtable_add_grob(g, grobs = segmentsGrob(x0 = unit(0, "npc"), y0 = unit(0, "npc"), 
    x1 = unit(1, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)), t = 1, b = nrow(g) - 1, l = 1, r = ncol(g))

g = gtable_add_grob(g, grobs = segmentsGrob(x0 = unit(0, "npc"), y0 = unit(0, "npc"), 
    x1 = unit(1, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)), t = 1, b = 1, l = 1, r = ncol(g))

grid.draw(g)



```

automatically created on 2018-05-28

### PYTHON Code
```python

# SFM1 - Project -> Compare the three different ways to value an IRS

import numpy as np
import pandas as pd
from prettytable import PrettyTable



######## 1.Input Parameters

Rfix     = 0.075 # Fixed Rate
Maturity = [0, 0.5, 1, 1.5, 2]
Yields   = [0, 0.06185567, 0.06382979, 0.06593407, 0.07471264]

y = [Maturity, Yields]
P = 10 # Principal

######## 2. Valuation

######## 2.1. Helper Functions

def Bfix(yields, Coupon_Anually, P): # Calculates a Bondprice with fixed Coupon
    
    yields         = list(yields)
    Coupon_Anually = float(Coupon_Anually)
    P              = float(P)    
    
    # D calculates the Discount factors
    D       = list(map(lambda x: (yields[1][x] * yields[0][x] + 1) ** (-1), range(len(yields[0]))))   
    PayDiff = list(np.diff(yields[0]))
    
    PayDiff.insert(0,0)
    
    PayDiff   = [x * Coupon_Anually for x in PayDiff]
    BondPrice = (np.dot(D, PayDiff) + D[len(D) - 1]) * P
    
    NPVs              = [x * y * P for x,y in zip(D, PayDiff)]
    NPVs[len(NPVs)-1] = NPVs[len(NPVs) - 1] + D[len(D) - 1] * P
    
    # Bond Price is the single Price and NPVs are the Present Values of the Bond payments
    return(BondPrice,NPVs)

def FRA(Rfix, yields, Si, Ti, P): #Calculates the Value of an FRA, Si < Ti
    
    Rfix   = float(Rfix)     
    yields = list(yields)
    P      = float(P)
    Si     = float(Si)
    Ti     = float(Ti)
    
    # DSi is the discount factor of time point Si. Same holds for Ti and DTi
    DSi = ((yields[1][yields[0].index(Si)] * Si) + 1) ** (-1)
    DTi = ((yields[1][yields[0].index(Ti)] * Ti) + 1) ** (-1)
    FRA = P * (DTi * (Ti - Si) * Rfix + DTi - DSi) # Price of the FRA
    
    # FRA = Value of the FRA
    return(FRA)

def ForwardRates(yields, Si, Ti): # Calculates the Forward rate implicit in the yield curve, Si < Ti
    
    yields = list(yields)
    Si     = float(Si)
    Ti     = float(Ti)    
    
    # DSi is the discount factor of time point Si. Same holds for Ti and DTi
    DSi = ((yields[1][yields[0].index(Si)] * Si) + 1) ** (-1)
    DTi = ((yields[1][yields[0].index(Ti)] * Ti) + 1) ** (-1)
    FR  = (1/(Ti - Si)) * ((DSi/DTi) - 1)
    
    # FR is the Forward Rate for Period Si to Ti
    return(FR)

######## 2.2. Valuation in Terms of Bond Prizes

VSwapB = float(Bfix(y, Rfix, P)[0] - P)

######## 2.3. Valuation in Terms of FRA Prizes

VSwapFRA = sum(list(map(lambda x: FRA(Rfix, y, y[0][x], y[0][x + 1], P), range(len(y[0]) - 1))))

######## 2.4. Valuation in Terms of Valuation in Terms of discounted Cash Flows

ERate = [x - y for x, y in zip([Rfix] * (len(y[0]) - 1), list(map(lambda x: ForwardRates(y, y[0][x], y[0][x + 1]), range(len(y[0]) - 1))))]
L     = list(np.diff(y[0]))
D     = list(map(lambda x: (y[0][x] * y[1][x]+1)**(-1), range(len(y[0]))))[1:]
DCF   = [x * y * z for x, y, z in zip(ERate, L, D)]

DCF.insert(0, 0)

# DCF are the discounted Cash Flows
DCF    = [x * P for x in DCF]
VSwapR = sum(DCF)

######## 3. Results

# Creation of the Columns of the printed table
Maturity_tab    = [format(x, '.1f') for x in Maturity] + ['Results:']
FixBondCashFlow = [format(x, '.5f') for x in list(map(lambda x: round(x, 6), list(Bfix(y, Rfix, P)[1]) + [Bfix(y, Rfix, P)[0]]))]
FloatRateNote   = [P] + list(np.repeat('', len(Maturity) - 1)) + [P]
BondBasedVal    = list(np.repeat('', len(Maturity))) + [round(Bfix(y, Rfix, P)[0] - P, 6)]
FRAVal          = pd.Series(list(np.arange(0, len(y[0])-1))).apply(lambda x: FRA(Rfix, y, y[0][x], y[0][x+1], P))
FRAVal          = [format(x, '.5f') for x in list(map(lambda x: round(x, 6), [0] + FRAVal.tolist() + [VSwapFRA]))]
DCF_tab         = [format(x, '.5f') for x in list(map(lambda x: round(x, 6), DCF + [sum(DCF)]))]


# Creates the table based on the columns defined above
x       = PrettyTable()

x.add_column('Maturity', Maturity_tab)
x.add_column('Fixed Bond', FixBondCashFlow)
x.add_column('Floating Rate Note', FloatRateNote)
x.add_column('Bond based Value', BondBasedVal)
x.add_column('FRA based Value', FRAVal)
x.add_column('DCF based Value', DCF_tab)

x.align = "r"

print(x)

```

automatically created on 2018-05-28