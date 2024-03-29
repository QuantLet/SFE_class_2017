def MA3():

    import os
    import sys

    import pandas as pd
    import pandas_datareader.data as web
    import numpy as np

    import statsmodels.formula.api as smf
    import statsmodels.tsa.api as smt
    import statsmodels.api as sm
    import scipy.stats as scs
    from arch import arch_model
    from statsmodels.tsa.arima.model import ARIMA
    import matplotlib.pyplot as plt
    import matplotlib as mpl
    get_ipython().magic('matplotlib inline')
    p = print

    end = '2017-01-01'
    start = '2008-01-01'
    get_px = lambda x: web.DataReader(x, 'yahoo', start=start, end=end)['Adj Close']

    symbols = ['SPY','TLT','MSFT']
    # raw adjusted close prices
    data = pd.DataFrame({sym:get_px(sym) for sym in symbols})
    # log returns
    lrets = np.log(data/data.shift(1)).dropna()

    def tsplot(y, lags=None, figsize=(10, 8), style='bmh'):
        if not isinstance(y, pd.Series):
            y = pd.Series(y)
        with plt.style.context(style):    
            fig = plt.figure(figsize=figsize)
            #mpl.rcParams['font.family'] = 'Ubuntu Mono'
            layout = (3, 2)
            ts_ax = plt.subplot2grid(layout, (0, 0), colspan=2)
            acf_ax = plt.subplot2grid(layout, (1, 0))
            pacf_ax = plt.subplot2grid(layout, (1, 1))
            qq_ax = plt.subplot2grid(layout, (2, 0))
            pp_ax = plt.subplot2grid(layout, (2, 1))

            y.plot(ax=ts_ax)
            ts_ax.set_title('Time Series Analysis Plots')
            smt.graphics.plot_acf(y, lags=lags, ax=acf_ax, alpha=0.5)
            smt.graphics.plot_pacf(y, lags=lags, ax=pacf_ax, alpha=0.5)
            sm.qqplot(y, line='s', ax=qq_ax)
            qq_ax.set_title('QQ Plot')        
            scs.probplot(y, sparams=(y.mean(), y.std()), plot=pp_ax)

            plt.tight_layout()
        return

    # Fit MA(3) to SPY returns

    max_lag = 30
    Y = lrets.SPY
    mdl = ARIMA(Y, order=(0, 0, 3)).fit()
    p(mdl.summary())
    _ = tsplot(mdl.resid, lags=max_lag)
