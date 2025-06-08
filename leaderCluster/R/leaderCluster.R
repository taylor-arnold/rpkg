#' Calculate clusters using Hartigan's Leader Algorithm
#'
#' Takes a matrix of coordinates and outputs cluster ids
#' from running the leader algorithm. The coordinates can either be on
#' points in the space R^n, or latitude/longitude pairs. A radius delta
#' must be provided.
#'
#' @param points         A matrix, or something which can be coerced into a
#'                       matrix, of coordinates with rows representing points
#'                       and columns representing dimensions. If using
#'                       \code{haversine} distance, this must be a two column
#'                       matrix with the first column containing latitudes in
#'                       decimal degrees and the second containing longitudes
#'                       in decimal degrees.
#'
#' @param radius         A scalar value giving the radius of the resulting
#'                       clusters; this is the main tuning parameter for the
#'                       algorithm. When using the \code{haversine} distance
#'                       this value should be in kilometres.
#'
#' @param weights        An vector of weights, one per row of points, to apply
#'                       to the clustering algorithm.
#'
#' @param max_iter       Maximum number of times to iterate the algorithm; can
#'                       safely set to 1 in many instances. See Details.
#'
#' @param distance       The method to be used for calculating distances between
#'                       points. If this is set to \code{haversine}, the
#'                       \code{points} must be a two column matrix. If
#'                       \code{Lp}, then the \code{p} input specifies which
#'                       norm is being used.
#'
#' @param p              When using \code{Lp} as the value for \code{distance},
#'                       this is a positive number specifing which Lp-norm to
#'                       implement. For p equal to 1,2, or Inf, a special
#'                       implementation will be used which is slightly more
#'                       efficent than the more general application.
#'
#' @return  A list containing a vector of cluster ids, a matrix of cluster
#'          centroids, the number of clusters, and the number iterations.
#'
#' @details
#'  The value for delta defines an approximate radius of each
#'  cluster. As the algorithm runs, a point within a distance
#'  delta from the centroid of a cluster will be labeled with
#'  the coorisponding cluster. As centroid clusters move, it
#'  is possible for the final radius of each cluster to be
#'  slightly larger than delta.
#'
#'  Unlike many other iterative clustering algorithms, the
#'  leader algorithm typically provides reasonable clusters
#'  after just a single pass. When speed is of concern, the
#'  max_iter value may be safely set to 1. However, the
#'  algorithm typically fully converges in only a few cycles;
#'  also, a convergent solution will usually have a smaller
#'  number of clusters than a solution with only one pass.
#'
#'  The algorithm scales nicely, and can fit a model with
#'  100s of columns and 100k's of rows in (on a relatively
#'  modest machine) under a minute. However, the processing
#'  time decays significantly if the radius is too small,
#'  since the number of clusters will be very high.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @references J. A. Hartigan. Clustering Algorithms. John Wiley & Sons,
#' New York, 1975.
#'
#'@examples
#'points <- 1:10
#'out <- leaderCluster(points, radius=2, distance="Lp", max_iter=1L)
#'
# A two-dimensional example
#'par(mar = c(0,0,0,0), mfrow = c(1,3))
#'set.seed(1)
#'points <- matrix(runif(100*2), ncol=2)
#'for(r in c(0.1, 0.2, 0.4)) {
#'  out <- leaderCluster(points = points, radius = r, distance="L2")$cluster_id
#'  cols <- rainbow(length(unique(out)))[out]
#'  plot(points, pch = 19, cex = 0.7, col = cols, axes = FALSE)
#'  points(points[!duplicated(out),,drop=FALSE], cex = 2, col = unique(cols))
#'  box()
#'}
#'
#' @export
leaderCluster <- function(points, radius, weights = rep(1, nrow(points)), max_iter = 10L,
                          distance = c("Lp", "L1", "L2", "Linf", "haversine"), p = 2)
{
  type_code = match(distance[[1]], c("Lp", "L1", "L2", "Linf", "haversine")) - 1L
  if (!is.matrix(points)) points = as.matrix(points)
  if (is.na(type_code))
  {
    stop(paste0("The distance method '", distance[[1]], "' is not supported."))
  }
  if (p <= 0) stop(paste0("The input p='", p, "' is invalid. Must be > 0."))
  if (type_code == 0L & p == 1) type_code = 1L
  if (type_code == 0L & p == 2) type_code = 2L
  if (type_code == 0L & p == Inf) type_code = 3L
  if (!is.finite(p)) p = 2
  if (type_code == 4L & ncol(points) != 2L)
  {
    stop("Haversine distance requires two dimensional input.")
  }

  cluster_id = rep(0, nrow(points))

  iter = 1L
  index = 1L:nrow(points)
  ord = order(weights, decreasing=TRUE)
  points = points[ord,,drop=FALSE]
  index = index[ord]

  while (iter <= max_iter )
  {

    out = .C(C_leader_cluster,
              delta = as.double(radius),
              points = as.double(points),
              weights = as.double(weights),
              cluster_id = as.integer(cluster_id),
              nrow = nrow(points),
              ncol = ncol(points),
              type = type_code,
              p = p,
              PACKAGE = "leaderCluster")$cluster_id + 1

    if(all(out == cluster_id)) break

    cluster_id = out
    ord = order(out)
    points = points[ord,,drop=FALSE]
    index = index[ord]
    iter = iter + 1
  }

  num_clusters = max(cluster_id)
  cluster_centroids = matrix(NA, ncol=ncol(points), nrow=num_clusters)
  for (i in 1:ncol(points)) cluster_centroids[,i] = tapply(points[,i], cluster_id, mean)

  return(list(cluster_id = out[match(1:length(index), index)],
              cluster_centroids = cluster_centroids,
              num_clusters = num_clusters,
              iter = iter))
}
