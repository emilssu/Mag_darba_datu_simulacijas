from scipy.optimize import root_scalar #, minimize_scalar
import numpy as np

def root(f, ax, bx):
    try:
        result = root_scalar(f, bracket=[ax, bx])
        if np.abs(f(result.root)) <0.5: 
            return result.root
        else: return np.nan
    except ValueError:
        return np.nan


def root_delta(f, delta, crit_val, step):
    mind = delta
    maxd = delta
    while f(mind) < 0:
        mind = mind - step
    while f(maxd) < 0 :
        maxd = maxd + step
    return [root(f, mind, delta),root(f, delta, maxd)]




