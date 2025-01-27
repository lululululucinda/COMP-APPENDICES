import pandas as pd
from sklearn import preprocessing
import numpy as np
import statsmodels.api as sm
from sklearn.metrics import mean_squared_error
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import shapiro, probplot


# DATA processing
data_factor = pd.read_excel("D:\VScode\Python\F_third.xlsx", header = 0)


data_factor.columns = data_factor.columns.str.strip()


columns_to_normalize = ["Technology_Individuals_using_the_Internet",
                        "Technology_Fixed_Bandwidth", 
                        "Technology_Global_Bandwidth", 
                        "Technology_ISP", 
                        "Technology_IXP", 
                        "Economic_Gini", 
                        "Social_Population", 
                        "Governance_Government_Effectiveness", 
                        "Governance_Rule_of_Law"]

scaler = preprocessing.MinMaxScaler()
data_factor[columns_to_normalize] = scaler.fit_transform(data_factor[columns_to_normalize])

columes_to_percentile = ["Economic_unemployment", "Economic_GDP_Average", "Social_age_15_64", "GCI"]
data_factor[columes_to_percentile] = data_factor[columes_to_percentile] / 100


Y = data_factor["Crime"]
Y = np.log1p(Y)
X_factor = data_factor.iloc[:, 3 : 21]


if X_factor.isnull().any().any() or np.isinf(X_factor).any().any():
    X_factor = X_factor.fillna(0)  
    X_factor = X_factor.replace([np.inf, -np.inf], 0)  

Y = np.array(Y)
X_factor = sm.add_constant(X_factor)


# Fit GML Model
model = sm.GLM(Y, X_factor, family=sm.families.Gaussian())
results = model.fit()
print(results.summary())


glm_model = sm.GLM(Y, X_factor, family=sm.families.Gaussian()).fit()
aic = glm_model.aic
print(f"AIC: {aic}")

predictions = glm_model.predict(X_factor)
mse = mean_squared_error(Y, predictions)
print(f"MSE: {mse}")


# Backward
def backward_elimination(X, y, threshold_out = 0.05, verbose=True):
    included = list(X.columns)  
    while True:
        changed = False
        model = sm.GLM(y, sm.add_constant(pd.DataFrame(X[included])), 
                       family = sm.families.Gaussian()).fit()
        pvalues = model.pvalues.iloc[1:]  
        worst_pval = pvalues.max()  
        
        if worst_pval > threshold_out:  
            changed = True
            worst_feature = pvalues.idxmax()
            included.remove(worst_feature)
            if verbose:
                print(f'Drop {worst_feature} with p-value {worst_pval:.6f}')
        
        if not changed:  
            break
    
    return included


selected_variables_backward = backward_elimination(X_factor, Y)
print("向后回归选择的变量:", selected_variables_backward)


X_selected = X_factor[selected_variables_backward]
model = sm.GLM(Y, X_selected, family=sm.families.Gaussian()).fit()
print(model.summary())


aic = model.aic
print(f"AIC: {aic}")


predictions = model.predict(X_selected)
mse = mean_squared_error(Y, predictions)
print(f"MSE: {mse}")


# draw
params = model.params  
conf_int = model.conf_int()  
conf_int.columns = ['lower', 'upper']  


results = pd.concat([params, conf_int], axis=1)
results = results.drop('const')  


variable_rename = {
    'Technology_Individuals_using_the_Internet': 'Internet Users',
    'Technology_ISP': 'ISP Coverage',
    'Social_Subnational_HDI': 'HDI',
    'Social_Educational_index': 'Educational Index',
    'Technology_IXP': 'IXP Presence',
    'Economic_Gini': 'Gini Index',
    'Social_Population': 'Population',
    'Governance_political_stabilisation': 'Political Stability',
    'Economic_GDP_Average': 'GDP',
    'Technology_Global_Bandwidth': 'Global Bandwidth',
    'Technology_Fixed_Bandwidth': 'Fixed Bandwidth',
}


plt.figure(figsize=(10, 8))  
colors = plt.cm.inferno(np.linspace(0.25, 0.85, len(results)))  

for i, (index, row) in enumerate(results.iterrows()):
    label = variable_rename.get(index, index)  
    plt.errorbar(row['lower'], i, xerr=[[row[0] - row['lower']], [row['upper'] - row[0]]], 
                 fmt='o', color=colors[i], capsize=5, label=label)

plt.yticks(range(len(results)), [variable_rename.get(index, index) for index in results.index], fontsize=12)  
plt.gca().yaxis.set_tick_params(pad=10)  
plt.axvline(x=0, color='gray', linestyle='--', linewidth=1)  
plt.title('GLM Coefficient Estimates with 95% Confidence Intervals', fontsize=14, y=1.05)  
plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=10)
plt.tight_layout()
plt.show()


# Normal distribution
data = data_factor.iloc[:, 2:20]  

if data.isnull().any().any():
    data = data.fillna(data.mean())  


normal_test_results = {}
for column in data.columns:
    stat, p_value = shapiro(data[column])
    normal_test_results[column] = {'Statistic': stat, 'p-value': p_value}


print("正态性检验结果:")
for column, result in normal_test_results.items():
    print(f"{column}: 统计量={result['Statistic']:.4f}, p值={result['p-value']:.4f}")


def plot_normality_grid(data, normal_test_results):
    fig, axes = plt.subplots(3, 6, figsize=(30, 15))  
    axes = axes.flatten()  
    
    for i, column in enumerate(data.columns):
        sns.histplot(data[column], kde=True, color='blue', stat='density', ax=axes[i])
        axes[i].set_title(f'Histogram + KDE of {column}')
    
    plt.tight_layout()
    plt.show()

    fig, axes = plt.subplots(3, 6, figsize=(30, 15)) 
    axes = axes.flatten()  
    
    for i, column in enumerate(data.columns):
        probplot(data[column], dist="norm", plot=axes[i])
        axes[i].set_title(f'Q-Q Plot of {column}')
    
    plt.tight_layout()
    plt.show()

    fig, axes = plt.subplots(3, 6, figsize=(30, 15))  
    axes = axes.flatten()  
    
    for i, column in enumerate(data.columns):

        p_value = normal_test_results[column]['p-value']
        
        sns.kdeplot(data[column], color='red', label='Data KDE', ax=axes[i])
        sns.kdeplot(np.random.normal(loc=data[column].mean(), scale=data[column].std(), size=1000), 
                    color='green', label=f'Normal KDE\n(p={p_value:.4f})', ax=axes[i])
        axes[i].set_title(f'KDE of {column}')
        
        axes[i].legend(loc='upper right')
    
    plt.tight_layout()
    plt.show()


plot_normality_grid(data, normal_test_results)

# correlation
spearman_corr = data.corr(method='spearman')

plt.figure(figsize=(15, 12))
sns.heatmap(spearman_corr, annot=True, cmap='inferno', fmt=".2f", linewidths=0.5)
plt.title("Spearman Correlation Heatmap (Inferno)")
plt.show()

print("斯皮尔曼相关系数矩阵:")
print(spearman_corr)