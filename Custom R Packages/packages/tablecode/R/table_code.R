# Attemp to create my own package for the Outsourcing project,
# focusing on saving tables (and figures)
# the way I want them in a standardized way

library(devtools)

use_git()

# These are strings that will be used a lot
# Folders of interest
raw_folder <- "../Raw Data/"
clean_folder <- "../Cleaned Data/"
table_folder <- "../Tables/"
figure_folder <- "../Figures/NLSY 79 Matching/"
d_table_folder <- "../Drafts/Draft Tables/"
s_table_folder <- "../Slides/Slide Tables/"

# Create a default table_top
table_top <- "\\documentclass[12pt]{article}
\\usepackage[margin=.5in]{geometry}
\\usepackage{booktabs}
\\usepackage{graphicx}
\\begin{document}
\\begin{table}
\\centering
\\resizebox{\\textwidth}{!}{ \n"

# Create a default top for Draft tables (no resizebox, sometimes use it for Slides)
d_table_top <- "\\begin{table}[t!]
\\centering
{ \n"

# Create a default top for Slide tables (but sometimes use it for Drafts)
s_table_top <- "\\begin{table}[h!]
\\centering
\\resizebox{\\textwidth}{!}{ \n"

# Create a default bottom for Slide tables (no details)
s_bot <- "\\bottomrule
\\end{tabular}
}
\\end{table}"

# For saving graphs
# aspect_ratio <- 1.62
aspect_ratio <- 1.77
height <- 6
width <- height * aspect_ratio
