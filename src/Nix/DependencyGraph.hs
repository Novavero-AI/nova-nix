-- | Dependency graph construction and topological sorting.
--
-- Given a root derivation, builds a graph of all transitive
-- dependencies by reading .drv files from the store.  The graph
-- is then topologically sorted so dependencies are built before
-- their dependents.
module Nix.DependencyGraph
  ( -- * Types
    DepNode (..),
    DepGraph (..),
    TopoResult (..),

    -- * Graph construction
    buildDepGraph,

    -- * Topological sort
    topoSort,

    -- * Queries
    transitiveDeps,
    directDeps,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Nix.Derivation (Derivation (..))
import Nix.Store.Path (StorePath)

-- | A node in the dependency graph.
data DepNode = DepNode
  { -- | The .drv store path for this derivation.
    dnDrvPath :: !StorePath,
    -- | The parsed derivation.
    dnDerivation :: !Derivation,
    -- | Direct dependency .drv paths (from drvInputDrvs keys).
    dnDeps :: ![StorePath]
  }
  deriving (Eq, Show)

-- | A complete dependency graph: maps .drv paths to their nodes.
newtype DepGraph = DepGraph {unDepGraph :: Map StorePath DepNode}
  deriving (Eq, Show)

-- | Result of topological sorting.
data TopoResult
  = -- | Successfully sorted: build order with leaves (no deps) first.
    TopoSorted ![StorePath]
  | -- | Cycle detected: the paths involved in the cycle.
    TopoCycle ![StorePath]
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Graph construction
-- ---------------------------------------------------------------------------

-- | Build the full dependency graph starting from a root derivation.
--
-- The @readDrv@ function reads a .drv file from the store and parses it.
-- Returns @Left@ if any .drv file cannot be read.
--
-- Uses a 'Seq' work-queue for O(1) enqueue\/dequeue instead of list append.
buildDepGraph ::
  (StorePath -> Either Text Derivation) ->
  Derivation ->
  StorePath ->
  Either Text DepGraph
buildDepGraph readDrv rootDrv rootPath =
  go Map.empty (Seq.singleton (rootPath, rootDrv))
  where
    go visited queue = case Seq.viewl queue of
      Seq.EmptyL -> Right (DepGraph visited)
      (sp, drv) Seq.:< rest
        | Map.member sp visited -> go visited rest
        | otherwise ->
            let deps = Map.keys (drvInputDrvs drv)
                node =
                  DepNode
                    { dnDrvPath = sp,
                      dnDerivation = drv,
                      dnDeps = deps
                    }
                visitedWithNode = Map.insert sp node visited
             in case resolveNewDeps readDrv visitedWithNode deps of
                  Left err -> Left err
                  Right newItems -> go visitedWithNode (foldl' (|>) rest newItems)

-- | Resolve unvisited dependencies by reading their .drv files.
resolveNewDeps ::
  (StorePath -> Either Text Derivation) ->
  Map StorePath DepNode ->
  [StorePath] ->
  Either Text [(StorePath, Derivation)]
resolveNewDeps readDrv visited = traverse resolve . filter unvisited
  where
    unvisited dep = not (Map.member dep visited)
    resolve dep = case readDrv dep of
      Left err -> Left err
      Right drv -> Right (dep, drv)

-- ---------------------------------------------------------------------------
-- Topological sort (Kahn's algorithm)
-- ---------------------------------------------------------------------------

-- | Topologically sort the dependency graph using Kahn's algorithm.
-- Returns leaves first (build order), or reports a cycle.
topoSort :: DepGraph -> TopoResult
topoSort (DepGraph graph) =
  let allNodes = Map.keysSet graph
      -- depCount: for each node, how many of its deps are in the graph
      depCount = Map.map (countGraphDeps allNodes) graph
      -- reverseAdj: for each dep, which nodes depend on it
      reverseAdj = buildReverseAdj graph allNodes
      -- Start with nodes that have zero deps (leaves)
      zeroQueue = Seq.fromList [sp | (sp, 0) <- Map.toList depCount]
      totalNodes = Map.size graph
   in kahnLoop reverseAdj depCount zeroQueue [] 0 totalNodes

-- | Count how many of a node's dependencies exist in the graph.
countGraphDeps :: Set StorePath -> DepNode -> Int
countGraphDeps allNodes node =
  length (filter (`Set.member` allNodes) (dnDeps node))

-- | Build reverse adjacency: maps each dep to the list of nodes that depend on it.
buildReverseAdj :: Map StorePath DepNode -> Set StorePath -> Map StorePath [StorePath]
buildReverseAdj graph allNodes =
  Map.foldlWithKey' addReverse (Map.fromSet (const []) allNodes) graph
  where
    addReverse acc sp node =
      let depsInGraph = filter (`Set.member` allNodes) (dnDeps node)
       in foldl' (flip (Map.adjust (sp :))) acc depsInGraph

-- | Kahn's algorithm main loop.
--
-- Tracks @sortedCount@ instead of calling @length@ on the accumulator
-- each iteration, keeping the loop O(V + E) total.
kahnLoop ::
  Map StorePath [StorePath] ->
  Map StorePath Int ->
  Seq StorePath ->
  [StorePath] ->
  Int ->
  Int ->
  TopoResult
kahnLoop _ _ queue sorted sortedCount totalNodes
  | Seq.null queue =
      if sortedCount == totalNodes
        then TopoSorted (reverse sorted)
        else TopoCycle (reverse sorted)
kahnLoop reverseAdj depCount queue sorted sortedCount totalNodes =
  case Seq.viewl queue of
    Seq.EmptyL -> TopoSorted (reverse sorted) -- unreachable, guarded above
    sp Seq.:< rest ->
      let dependents = fromMaybe [] (Map.lookup sp reverseAdj)
          (updatedDepCount, newZero) = decrementDependents depCount dependents
          extendedQueue = foldl' (|>) rest newZero
       in kahnLoop reverseAdj updatedDepCount extendedQueue (sp : sorted) (sortedCount + 1) totalNodes

-- | Decrement in-degree for each dependent; collect any that reach zero.
decrementDependents :: Map StorePath Int -> [StorePath] -> (Map StorePath Int, [StorePath])
decrementDependents dc = foldl' step (dc, [])
  where
    step (counts, zeros) dep =
      let newDeg = maybe 0 (subtract 1) (Map.lookup dep counts)
          updatedCounts = Map.insert dep newDeg counts
       in if newDeg == 0
            then (updatedCounts, dep : zeros)
            else (updatedCounts, zeros)

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

-- | All transitive dependencies of a store path (not including itself).
transitiveDeps :: DepGraph -> StorePath -> Set StorePath
transitiveDeps (DepGraph graph) root =
  Set.delete root (go (Set.singleton root) (Seq.singleton root))
  where
    -- visited marks ENQUEUED nodes - the root is seeded, so a
    -- self-dependent root is expanded once instead of recursing forever.
    go !visited queue = case Seq.viewl queue of
      Seq.EmptyL -> visited
      sp Seq.:< rest ->
        enqueueFresh visited rest (maybe [] dnDeps (Map.lookup sp graph))
    -- Mark and enqueue each dep not yet seen, then continue the walk.
    enqueueFresh !visited !queue deps = case deps of
      [] -> go visited queue
      (dep : more)
        | Set.member dep visited -> enqueueFresh visited queue more
        | otherwise -> enqueueFresh (Set.insert dep visited) (queue |> dep) more

-- | Direct dependencies of a store path.
directDeps :: DepGraph -> StorePath -> [StorePath]
directDeps (DepGraph graph) sp =
  maybe [] dnDeps (Map.lookup sp graph)
