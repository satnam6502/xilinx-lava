{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lava.Graph (Lava, Graph(..),
                   computeGraph, mkNewNodeNumber, mkNewEdgeNumber, mkNode, Layout(..),
                   pushGraph, popGraph, addLayoutBlock)
where
import Control.Monad.State.Lazy ( execState, State , get, put)
import GHC.Stack

data Show a => Layout a
  = Block  [a]
  | Beside (Int, Int) (Layout a) (Layout a)
  | Below  (Int, Int) (Layout a) (Layout a)

deriving instance Show a => Show (Layout a)

data Graph t nodeType = Graph {
  nodeCount :: Int,
  netCount ::  Int,
  graphData :: t,
  nodes :: [Layout (Int, nodeType)]
}

type Lava t nodeType = State (Graph t nodeType)

initialState :: Show nodeType => t ->  Graph t nodeType
initialState initVal = Graph 0 0 initVal [Block []]

computeGraph :: Show nodeType => t -> Lava t nodeType a -> Graph t nodeType
computeGraph initData nl = execState nl (initialState initData)

mkNewNodeNumber :: Lava t nodeType Int
mkNewNodeNumber
  = do graph <- get
       let nc = nodeCount graph
       put (graph{nodeCount = nc +1})
       return nc

mkNode :: Show nodeType => nodeType -> Lava t nodeType ()
mkNode nodeValue
  = do n <- mkNewNodeNumber
       graph <- get
       let nodesList = nodes graph
       put (graph{nodes = addNode nodesList (n, nodeValue)})

addNode :: Show a => [Layout a] -> a -> [Layout a]
addNode ((Block n):xs) node  = (Block (node:n)):xs
addNode _ _ = error "addNode: Adding node but head of instances is not Block"

mkNewEdgeNumber :: Lava t nodeType Int
mkNewEdgeNumber
  = do graph <- get
       let ec = netCount graph
       put (graph{netCount = ec +1})
       return ec

pushGraph :: Show nodeType => Lava t nodeType ()
pushGraph
  = do graph <- get
       let nodesList = nodes graph
       put (graph{nodes = Block [] : nodesList})

popGraph :: Show nodeType => Lava t nodeType (Layout (Int, nodeType))
popGraph
  = do graph <- get
       let nodesList = nodes graph
           (b, nl) = popBlock nodesList
       put (graph{nodes = nl})
       return b
       
popBlock :: Show a => [Layout a] -> (Layout a, [Layout a])
popBlock (blk@(Block _):bs) = (blk, bs)
popBlock other = error ("popBlock: top instance is not a Block: " ++ show other)

addLayoutBlock :: (HasCallStack, Show a) => Layout (Int, a) -> Lava t a ()
addLayoutBlock b
  = do graph <- get
       let nodesList = nodes graph
       put (graph{nodes = insertLayoutBlock b nodesList})

insertLayoutBlock :: (HasCallStack, Show a) => Layout a -> [Layout a] -> [Layout a]
insertLayoutBlock lb layouts = layouts ++ [lb]


