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
