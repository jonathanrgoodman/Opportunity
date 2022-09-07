The code for five models: basic Prisoner's Dilemma (PD), strong reciprocity (Model 1), mimic (Model 2), silent defection (Model 3), and silent defection with probabilistic sensitivity, for R, are included in this repository, and can be found in the <a href="https://github.com/jonathanrgoodman/Opportunity/tree/main/Models">models</a> folder. The models use basic R syntax; the ggplot2 package (Wickham, 2016; https://cran.r-project.org/web/packages/ggplot2/index.html) is required to plot the functions' output. See the <a href="https://github.com/jonathanrgoodman/Opportunity/tree/main/plot-functions">plot-functions</a> folder to plot outputs from these models. All plotting functions use generations as the x-axis.

The <a href="https://github.com/jonathanrgoodman/Opportunity/blob/main/Supplement.docx">Supplement.docx</a> file gives the models' description, which follows the standardized ODD protocol for agent-based models (ABMs; Grimm et al. 2006), as well as the supplementary figures from the main text and information about the models' parameters, including default parameters.

The Model functions generate outputs over generations as follows:

Basic PD: output [[1]] is number of cooperators and defectors; output [[2]] is percentage of cooperative actions

Model 1: output [[1]] is number of cooperators and defectors; output [[2]] is percentage of cooperative actions

Model 2: output [[1]] is number of cooperators and defectors; output [[2]] is number of honest signallers and mimics; output [[3]] is changes in mimicry and sensitivity rates; output [[4]] is percentage of cooperative actions

Model 3: output [[1]] is number of cooperators and defectors; output [[2]] is number of honest signallers and mimics; output [[3]] is number of agents with an overt and silent signalling strategy; output [[4]] is changes in mimicry and sensitivity rates; output [[5]] is percentage of cooperative actions

The R markdown file can also be found <a href="http://htmlpreview.github.io/?https://github.com/jonathanrgoodman/Opportunity/blob/main/Models-notebook.nb.html">here</a>.
