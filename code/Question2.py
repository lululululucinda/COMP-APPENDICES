import pandas as pd
from sklearn.preprocessing import LabelEncoder
import numpy as np
import statsmodels.api as sm
from sklearn.metrics import mean_squared_error

data = pd.read_excel("D:\VScode\Python\F_data.xlsx", header = 0)
data.columns = data.columns.str.strip()
data.head()

le_country_full_name = LabelEncoder()
data['country_full_name_encoded'] = le_country_full_name.fit_transform(data['Country (Full Name)'])
data.head()

sem_data = data[['Legal', 'Technical', 'Organizational', 'Capacity', 'Cooperative', 'IP','country_full_name_encoded']]
print(sem_data.head())

y = sem_data["IP"]
y_log = np.log1p(y)  

X = sem_data[[ "Organizational",  "Cooperative","Legal", "Technical" , "Capacity"]]
X = sm.add_constant(X)

model = sm.GLM(y_log, X, family = sm.families.Gaussian())
results = model.fit()
print(results.summary())

glm_model = sm.GLM(y_log, X, family=sm.families.Gaussian()).fit()
aic = glm_model.aic
print(f"AIC: {aic}")

predictions = glm_model.predict(X)
mse = mean_squared_error(y_log, predictions)
print(f"MSE: {mse}")