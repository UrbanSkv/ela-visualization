# ela-visualization
The source code used for the paper "Understanding the problem space in single-objective numerical optimization using exploratory landscape analysis"


# Contents
The folder /R contains two R source files used for visualizations
-    visualize_simple.R Is the file that should be used if you want to run your own experiments. It contains a minimal example of using tsne to visualize two sets of landscape features using the function example(). To run this file, you first have to provide two files that contain ELA landscape features, calculated using the library flacco and saved using saveRDS
-    visualize.R contains the full source code that was used in our experiments, which includes a large amount of code that was used for saving and loading our data files, and will not be useable without modifications. It is provided only as a refference.
