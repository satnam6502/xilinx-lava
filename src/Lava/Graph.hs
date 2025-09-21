{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lava.Graph (Lava, Graph(..),
                   computeGraph, mkNewNodeNumber, mkNewEdgeNumber, addNode, Layout(..),
                   popBlock, popN, addLayoutBlock)
where
import Control.Monad.State.Lazy ( execState, State , get, put)
import GHC.Stack
import Control.Monad

data Show a => Layout a
  = Block  a
  | Beside  (Int, Int) (Layout a) (Layout a)
  | Below   (Int, Int) (Layout a) (Layout a)
  | Overlay (Int, Int) (Layout a) (Layout a)

deriving instance Show a => Show (Layout a)

data Graph t nodeType = Graph {
  nodeCount :: Int,
  netCount ::  Int,
  graphData :: t,
  nodes :: [Layout (Int, nodeType)]
}

type Lava t nodeType = State (Graph t nodeType)

initialState :: Show nodeType => t ->  Graph t nodeType
initialState initVal = Graph 0 0 initVal [] 

computeGraph :: Show nodeType => t -> Lava t nodeType a -> Graph t nodeType
computeGraph initData nl = execState nl (initialState initData)

mkNewNodeNumber :: Lava t nodeType Int
mkNewNodeNumber
  = do graph <- get
       let nc = nodeCount graph
       put (graph{nodeCount = nc +1})
       return nc

addNode :: Show nodeType => nodeType -> Lava t nodeType ()
addNode nodeValue
  = do n <- mkNewNodeNumber
       graph <- get
       let nodesList = nodes graph
       put (graph{nodes = Block (n, nodeValue) : nodesList})

mkNewEdgeNumber :: Lava t nodeType Int
mkNewEdgeNumber
  = do graph <- get
       let ec = netCount graph
       put (graph{netCount = ec +1})
       return ec

popBlock :: HasCallStack => Lava t a (Layout (Int, a))
popBlock
  = do graph <- get
       let nl = nodes graph
       when (null nl) $ error "popBlock called on empty netlist"
       put (graph{nodes = tail nl})
       return (head nl)

popN :: HasCallStack => Int -> Lava t a [Layout (Int, a)]
popN n
  = do graph <- get
       let nl = nodes graph
       put (graph{nodes = drop n nl})
       return (take n nl)

addLayoutBlock :: (HasCallStack, Show a) => Layout (Int, a) -> Lava t a ()
addLayoutBlock b
  = do graph <- get
       let nodesList = nodes graph
       put (graph{nodes = b : nodesList})



