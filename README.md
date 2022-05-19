# ela-visualization
The source code used for the paper "Understanding the problem space in single-objective numerical optimization using exploratory landscape analysis"


# Contents
The folder /R contains three R source files used for visualizations
-    visualize_simple.R Is the file that should be used if you want to run your own experiments. It contains a minimal example of using tsne to visualize two sets of landscape features using the function example(). To run this file, you first have to provide two files that contain ELA landscape features, calculated using the library flacco and saved using saveRDS
-    example.R provides an example of the visualization using two example feature sets, and from CEC 2014 and one from CEC 2015.
-    visualize.R contains the full source code that was used in our experiments, which includes a large amount of code that was used for saving and loading our data files, and will not be useable without modifications. It is provided only as a refference.

The folder /matlab contains an example code for sampling the problems from the CEC 2015 problem set. In order to run this function, teh CEC 2015 function definitions must first be extracted to the same folder. The definitions can be downloaded from https://github.com/P-N-Suganthan/CEC2015-Learning-Based


The folder /data contains two example RDS files that contain features corresponding to the 2014 and the 2015 CEC problem sets.
