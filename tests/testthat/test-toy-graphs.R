test_that("toy graph data has stable dimensions and labels", {
  data(toy.graph)

  expect_equal(dim(toy.graph), c(11, 11))
  expect_equal(rownames(toy.graph), colnames(toy.graph))
  expect_true(all(diag(toy.graph) == 0))
})

test_that("toy graph is recognized as a chain graph", {
  data(toy.graph)

  result <- is.chaingraph(toy.graph)

  expect_true(result$result)
  expect_equal(sum(result$chain.size), nrow(toy.graph))
  expect_length(result$vert.order, nrow(toy.graph))
})

test_that("directed cycles are rejected as chain graphs", {
  cycle <- matrix(
    c(0, 1, 0,
      0, 0, 1,
      1, 0, 0),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(letters[1:3], letters[1:3])
  )

  expect_false(is.chaingraph(cycle)$result)
})

test_that("skeleton makes directed edges undirected", {
  graph <- matrix(
    c(0, 1, 0,
      0, 0, 1,
      0, 0, 0),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(letters[1:3], letters[1:3])
  )

  result <- skeleton(graph)

  expect_equal(result, t(result))
  expect_equal(result[1, 2], 1)
  expect_equal(result[2, 3], 1)
  expect_equal(dimnames(result), dimnames(graph))
})

test_that("a single arrow becomes undirected in the pattern", {
  graph <- matrix(
    c(0, 1,
      0, 0),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("a", "b"), c("a", "b"))
  )

  result <- pattern(graph)

  expect_equal(result, matrix(c(0, 1, 1, 0), nrow = 2,
                              dimnames = dimnames(graph)))
})

test_that("moralization joins the parents of a collider", {
  graph <- matrix(
    c(0, 0, 1,
      0, 0, 1,
      0, 0, 0),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(letters[1:3], letters[1:3])
  )

  result <- moralize(graph)

  expect_equal(result, t(result))
  expect_equal(result[1, 2], 1)
  expect_true(all(result[cbind(c(1, 2), c(3, 3))] == 1))
})

test_that("identical skeletons compare without differences", {
  data(toy.graph)
  graph_skeleton <- skeleton(toy.graph)
  result <- comp.skel(graph_skeleton, graph_skeleton)

  expect_equal(result$e.total, sum(graph_skeleton) / 2)
  expect_equal(result$e.missing, 0)
  expect_equal(result$e.extra, 0)
})

test_that("identical patterns have zero structural Hamming distance", {
  data(toy.graph)
  graph_pattern <- pattern(toy.graph)
  result <- comp.pat(graph_pattern, graph_pattern)

  expect_equal(result$a.missing, 0)
  expect_equal(result$a.extra, 0)
  expect_equal(result$shd, 0)
})

test_that("a four-cycle is not triangulated", {
  cycle <- matrix(
    c(0, 1, 0, 1,
      1, 0, 1, 0,
      0, 1, 0, 1,
      1, 0, 1, 0),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(letters[1:4], letters[1:4])
  )

  expect_false(maxcard.search(cycle)$is.triangulated)
})

test_that("triangulation produces a triangulated graph", {
  cycle <- matrix(
    c(0, 1, 0, 1,
      1, 0, 1, 0,
      0, 1, 0, 1,
      1, 0, 1, 0),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(letters[1:4], letters[1:4])
  )

  result <- tri.ug(cycle)

  expect_true(maxcard.search(result)$is.triangulated)
  expect_equal(result, t(result))
  expect_true(all(diag(result) == 0))
})

test_that("toy graph skeleton produces a valid separation tree", {
  data(toy.graph)
  result <- ug.to.jtree(skeleton(toy.graph))

  expect_s4_class(result, "sep.tree")
  expect_true(validObject(result, test = TRUE))
  expect_length(result@cliques, length(result@separators) + 1)
})

test_that("frequency tables can be compressed by variable", {
  observations <- matrix(
    c(0, 0,
      0, 1,
      1, 0,
      1, 1),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(NULL, c("x", "y"))
  )
  table <- as.freq.tb(observations)
  compressed <- compress.freq.tb(table, "x")

  expect_s4_class(table, "freq.tb")
  expect_s4_class(compressed, "freq.tb")
  expect_equal(sum(table@table[, "Freq"]), 4)
  expect_equal(sum(compressed@table[, "Freq"]), 4)
  expect_equal(colnames(compressed@table), c("x", "Freq"))
})
