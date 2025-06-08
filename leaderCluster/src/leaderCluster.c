#include <R.h>

#include "ctest.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define RADIAN_FACTOR 0.017453292519943295474
#define R 6378.1 // radius of the earth in kilometres

#define MIN(X, Y) (((X) < (Y)) ? (X) : (Y))
#define MAX(X, Y) (((X) > (Y)) ? (X) : (Y))

/* Distance type definitions */
#define Lp 0
#define L1 1
#define L2 2
#define Linf 3
#define HAVERSINE 4

void leader_cluster ( double * delta, double * points, double * weights,
                      int * cluster_id, int * nrow, int * ncol, int * type, double * p) {

  // Local variables
  double * cluster_centroid;
  double * cluster_weight;
  double new_weight;
  double d;
  double h;
  int i;
  int j;
  int k;
  int num_clusters;
  int row = *nrow;
  int col = *ncol;

  // Allocate variables
  cluster_centroid = (double *) malloc(col * row * sizeof(double));
  cluster_weight = (double *) malloc(row * sizeof(double));

  // Initalize first cluster:
  num_clusters = 0;
  cluster_id[0] = 0;
  for (i = 0; i < col; i++) cluster_centroid[i * row] = points[i * row];
  cluster_weight[num_clusters] = weights[0];
  num_clusters++;

  // Cycle through the points, allocating to a cluster; add
  // a new cluster when needed:
  for (i = 1; i < row; i++)
  {
    for (j = 0; j < num_clusters; j++)
    {
      d = 0;
      switch (*type) {
        case Lp:
          for (k = 0; k < col; k++) d += pow(fabs(points[i + k*row] - cluster_centroid[j + k*row]), *p);
          d = pow(d, 1 / *p);
          break;
        case L1:
          for (k = 0; k < col; k++) d += fabs(points[i + k*row] - cluster_centroid[j + k*row]);
          break;
        case L2:
          for (k = 0; k < col; k++) d += pow(points[i + k*row] - cluster_centroid[j + k*row],2);
          d = sqrt(d);
          break;
        case Linf:
          for (k = 0; k < col; k++) d += MAX(d, fabs(points[i + k*row] - cluster_centroid[j + k*row]));
          break;
        case HAVERSINE:
          h = sqrt(pow(sin(RADIAN_FACTOR * (points[i] - cluster_centroid[j])/2),2) +
                       cos(RADIAN_FACTOR * points[i]) * cos(RADIAN_FACTOR * cluster_centroid[j]) *
                       pow(sin(RADIAN_FACTOR * (points[i + row] - cluster_centroid[j + row])/2),2));
          if(h > 1) h = 1; // check for near-antipodal points
          d = 2 * R * asin(h);
          break;
      }

      if(d <= (*delta))
      {
        cluster_id[i] = j;
        new_weight = cluster_weight[j] + weights[i];
        for (k = 0; k < col; k++)
        {
          cluster_centroid[j + k*row] *= cluster_weight[j] / new_weight;
          cluster_centroid[j + k*row] += weights[i] / new_weight * points[i + k*row];
        }
        cluster_weight[j] = new_weight;
        break;
      }
    }
    if(j == num_clusters)
    {
      cluster_id[i] = j;
      for (k = 0; k < col; k++)
      {
        cluster_centroid[num_clusters + k*row] = points[i + k*row];
      }
      cluster_weight[num_clusters] = weights[i];
      num_clusters++;
    }
  }

  // Free variables
  free(cluster_centroid);
  free(cluster_weight);

}
