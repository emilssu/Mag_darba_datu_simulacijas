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

#def split_interval(ax, bx, n):
#    interval_width = (bx - ax) / n
#    intervals = [(ax + i * interval_width, ax + (i + 1) * interval_width) for i in range(n)]
#    return intervals

#def root_mu(f, ax, bx, n):
#    intervals = split_interval(ax, bx, n)
#    roots = []
#
#    for interval in intervals:
#        try:
#            result = root_scalar(f, bracket=interval)
#            if result.converged:
#                roots.append(result.root)
#        except ValueError:
#            pass 
#    min = np.argmin(np.abs(f(roots)))
#    return roots[min]

#def root_mu(f, ax, bx):
#    res = minimize_scalar(f, [ax,bx])
#    return res.x

def root_delta(f, delta, crit_val, step):
    #delt = delta.astype(float)
    mind = delta
    maxd = delta
    #stepp = step.astype(float)
   #crit_vall = crit_val.astype(float)
    while f(mind) < 0:
        mind = mind - step

    while f(maxd) < 0 :
        maxd = maxd + step
    return [root(f, mind, delta),root(f, delta, maxd)]

#def root_delta(f, crit_val):
#   return fixed_point(f, crit_val)

#print(fixed_point(lambda x: x*x, -1))


