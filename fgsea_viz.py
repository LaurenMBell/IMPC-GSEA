import pandas as pd
import matplotlib.pyplot as plt 

results = pd.read_csv("fgsea_results.csv")

top = results.sort_values(by='padj', ascending=True).head(20)

print(top.head())

plt.figure(figsize=(10, 8))
plt.barh(top["mp_term_query"], top["padj"])

plt.title("Top 20 Top Level MP Terms\nEnriched in CP Genes")
plt.ylabel("MP Term")
plt.xlabel("p Adjusted")
plt.tight_layout()
plt.show() 
