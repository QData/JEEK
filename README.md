# JEEK

## Tool JEEK: A Fast and Scalable Joint Estimator for Integrating Additional Knowledge in Learning Multiple Related Sparse Gaussian Graphical Models

### Paper: [@Arxiv](https://arxiv.org/abs/1806.00548) | updated version at [Here](https://github.com/QData/JEEK/blob/master/icml18-JEEKarxivUpdated.pdf)

### accepted at  [2018 ICML](http://proceedings.mlr.press/v80/wang18f.html)


### R package: [URL](https://cran.r-project.org/web/packages/jeek/index.html)

### GitRepo for R package: [URL](https://github.com/QData/JEEK)


```R
install.packages("jeek")
library(jeek)
demo(jeek)
```


### Abstract
We consider the problem of including additional knowledge in estimating sparse Gaussian graphical models (sGGMs) from aggregated samples, arising often in bioinformatics and neuroimaging applications. Previous joint sGGM estimators either fail to use existing knowledge or cannot scale-up to many tasks (large $K$) under a high-dimensional (large $p$) situation.  In this paper, we propose a  novel \underline{J}oint \underline{E}lementary \underline{E}stimator incorporating additional \underline{K}nowledge (JEEK) to infer multiple related sparse Gaussian Graphical models from large-scale heterogeneous data. Using domain knowledge as weights, we design a novel hybrid norm as the minimization objective to enforce the superposition of two weighted sparsity constraints, one on the shared interactions and the other on the task-specific structural patterns. This enables JEEK to elegantly consider various forms of existing knowledge based on the domain at hand and avoid the need to design knowledge-specific optimization. JEEK is solved through a fast and entry-wise parallelizable solution that largely improves the computational efficiency of the state-of-the-art  $O(p^5K^4)$ to $O(p^2K^4)$. We conduct a rigorous statistical analysis showing that JEEK achieves the same  convergence rate $O(\log(Kp)/n_{tot})$ as the state-of-the-art estimators that are much harder to compute. 
Empirically, on multiple synthetic datasets and two real-world data, JEEK outperforms the speed of the state-of-arts significantly while achieving the same level of prediction accuracy.


### Citations

```latex
@conference{wang2018jeek,
  Author = {Wang, Beilun and Sekhon, Arshdeep and Qi, Yanjun},
  Booktitle = {Proceedings of The 35th International Conference on Machine Learning (ICML)},
  Title = {A Fast and Scalable Joint Estimator for Integrating Additional Knowledge in Learning Multiple Related Sparse Gaussian Graphical Models},
  Year = {2018}}
}
```


### Support or Contact

Having trouble with our tools? Please [contact Beilun](mailto:bw4mw@virginia.edu) and we’ll help you sort it out.
