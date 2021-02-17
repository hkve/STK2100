import numpy as np

alpha = np.array([26.08,24.69,29.950,33.84])
beta = np.array([26.08, -1.39, 3.87, 7.76])
gamma = np.array([28.64, -2.56, -3.95, 1.31])

print("beta-alpha relation")
print(beta)
print(alpha-beta[0]) # b_j = a_j - b_0

print("gamma-alpha relation")
print(np.sum(alpha)/4, gamma[0]) # g_0 = mean(a_j)
print(gamma[0]+gamma[1:])
print(alpha)