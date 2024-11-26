#include <Rcpp.h>
#include <RcppEigen.h>

// [[Rcpp::export]]
Eigen::VectorXd broadcasting_2d_array_multiply(
  Eigen::VectorXd x,
  Eigen::VectorXi dimX,
  Eigen::VectorXd y,
  Eigen::VectorXi dimY
) {
  Eigen::VectorXi dimMax = dimX.cwiseMax(dimY);
  Eigen::ArrayXd output(dimMax[0] * dimMax[1]);
  for (int j = 0; j < dimMax[1]; j++) {
    for (int i = 0; i < dimMax[0]; i++) {
      output[
        i + j * dimMax[0]
      ] = x[
        (i % dimX[0]) + (j % dimX[1]) * dimX[0]
      ] * y[
        (i % dimY[0]) + (j % dimY[1]) * dimY[0]
      ];
    }
  }
  return output;
}

// [[Rcpp::export]]
Eigen::VectorXd broadcasting_3d_array_multiply(
  Eigen::VectorXd x,
  Eigen::VectorXi dimX,
  Eigen::VectorXd y,
  Eigen::VectorXi dimY
) {
  Eigen::VectorXi dimMax = dimX.cwiseMax(dimY);
  Eigen::ArrayXd output(dimMax[0] * dimMax[1] * dimMax[2]);
  for (int k = 0; k < dimMax[2]; k++) {
    for (int j = 0; j < dimMax[1]; j++) {
      for (int i = 0; i < dimMax[0]; i++) {
        output[
          i + j * dimMax[0] + k * dimMax[0] * dimMax[1]
        ] = x[
          (i % dimX[0]) + (j % dimX[1]) * dimX[0] + (k % dimX[2]) * dimX[0] * dimX[1]
        ] * y[
          (i % dimY[0]) + (j % dimY[1]) * dimY[0] + (k % dimY[2]) * dimY[0] * dimY[1]
        ];
      }
    }
  }
  return output;
}
