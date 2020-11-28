Reducing dimensions of mixed datasets
=====================================
This code was developed during my honours programme for the Enginering in Computer Science master's degree (attended at Sapienza University of Rome).
The programme's goal was to promote the intellectual development of talented students through extracurricular activities.

I chose to analyse the dimensionality reduction of mixed datasets (containing numerical, categorical and ordinal features).
I evaluated existing techniques with respect to the manifold hypothesis and performed a comparison of their implementation in R.

This repository includes only the code that was actually used in the final report.
Please refer to the document for an analysis of the findings.


Branches
--------
As usual, the ``master`` branch contains the final release of the code.
In addition, there are two other branches:

- ``principal`` contains an implementation of [Young+78]_.
  It was excluded from the final report due to its poor performance.
- ``out`` includes the raw execution data.

.. [Young+78] F. W. Young, Y. Takane and J. de Leeuw.
   *The principal components of mixed measurement level multivariate data: An alternating least squares method with optimal scaling features*.
   In: 'Psychometrika' 43.2 (1978), pp. 279–281.
   DOI: `10.1007/BF02293871`__.
__ https://doi.org/10.1007/BF02293871
