import numpy as np
import matplotlib.pyplot as plt
import scipy.stats as stats

mu = 5
sigma = 2
n = 1000


y = np.random.normal(mu, sigma, n)
stats.probplot(y, dist="norm", plot=plt)
plt.show()

