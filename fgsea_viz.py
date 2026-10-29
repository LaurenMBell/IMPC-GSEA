import pandas as pd
import matplotlib.pyplot as plt 
import numpy as np 

results = pd.read_csv("top_level_mp_term_fgsea_results.csv")

results["padj"] = np.log10(results["padj"]) * -1

top = results.sort_values(by='padj', ascending=False).head(20)
top = top.iloc[::-1]

print(top.head())

plt.figure(figsize=(10, 8))
plt.barh(top["mp_term_query"], top["padj"])

plt.title("Top 20 Top Level MP Terms\nEnriched in CP Genes")
plt.ylabel("Top Level MP Term")
plt.xlabel("-log(p adj.)")
plt.tight_layout()
plt.show() 
