STA250_Final
============

Code, report, and data for classifying a large number of images into two categories.


# ***** CODE FILES *****
dogcat_sim.R                # File submitted to Gauss for computation

dogcat_sim.sh               # File submitted to Gauss to handle the array job

dogcat_misc.R               # Misc. code such as initial data conversion & recombining

dogcat_class.R              # Classification methods used

data_matrix.RData           # The entire matrix of color values. WARNING: Very large!

dogcat_class_results.RData  # Contains variables from classification (time to run, glm objects)

# ***** PICTURES *****
glm_curve.png     # Represents the classification curve for penalized GLM

rf_error.png      # Shows error rate for random forests given the number of trees
