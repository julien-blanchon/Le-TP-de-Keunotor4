import numpy as np

def adjusted_r2_score(y, y_pred, k):
    n = y.shape[0]
    assert len(y.shape) == len(y_pred.shape) == 1
    RSS = np.sum((y-y_pred)**2)
    TSS = np.sum((y-np.mean(y))**2)
    return 1-RSS/TSS*(n-1)/(n-k-1)

def aic(y, y_pred, k):
    n = y.shape[0]
    RSS = np.sum((y-y_pred)**2)
    return n*np.log(RSS) + 2*k

def bic(y, y_pred, k):
    n = y.shape[0]
    RSS = np.sum((y-y_pred)**2)
    return n*np.log(RSS) + k*np.log(n)

def mallow(y, y_pred_sub, y_pred_tot, k):
    n = y.shape[0]
    RSS_tot = np.sum((y-y_pred_tot)**2)
    RSS_sub = np.sum((y-y_pred_sub)**2)
    return n*RSS_sub/RSS_tot - (n-2*k)
