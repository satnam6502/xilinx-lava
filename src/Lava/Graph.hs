{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lava.Graph (Lava, Graph(..),
                   computeGraph, mkNewNodeNumber, mkNewEdgeNumber, mkNode, Layout(..),
                   pushGraph, popGraph, addLayoutBlock)
where
import Control.Monad.State.Lazy ( execState, State , get, put)

data Layout a
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

initialState :: t ->  Graph t nodeType
initialState initVal = Graph 0 0 initVal [Block []]

computeGraph :: t -> Lava t nodeType a -> Graph t nodeType
computeGraph initData nl = execState nl (initialState initData)

mkNewNodeNumber :: Lava t nodeType Int
mkNewNodeNumber
  = do graph <- get
       let nc = nodeCount graph
       put (graph{nodeCount = nc +1})
       return nc

mkNode :: nodeType -> Lava t nodeType ()
mkNode nodeValue
  = do n <- mkNewNodeNumber
       graph <- get
       let nodesList = nodes graph
       put (graph{nodes = addNode nodesList (n, nodeValue)})

addNode :: [Layout a] -> a -> [Layout a]
addNode ((Block n):xs) node  = (Block (node:n)):xs
addNode _ _ = error "addNode: Adding node but head of instances is not Block"

mkNewEdgeNumber :: Lava t nodeType Int
mkNewEdgeNumber
  = do graph <- get
       let ec = netCount graph
       put (graph{netCount = ec +1})
       return ec

pushGraph :: Lava t nodeType ()
pushGraph
  = do graph <- get
       let nodesList = nodes graph
       put (graph{nodes = Block [] : nodesList})

popGraph :: Lava t nodeType (Layout (Int, nodeType))
popGraph
  = do graph <- get
       let nodesList = nodes graph
           (b, nl) = popBlock nodesList
       put (graph{nodes = nl})
       return b
       
popBlock :: [Layout a] -> (Layout a, [Layout a])
popBlock (blk@(Block _):bs) = (blk, bs)
popBlock _ = error "popBlock: top instance is not a Block"

addLayoutBlock :: Layout (Int, a) -> Lava t a ()
addLayoutBlock b
  = do graph <- get
       let nodesList = nodes graph
       put (graph{nodes = insertLayoutBlock b nodesList})

insertLayoutBlock :: Layout a -> [Layout a] -> [Layout a]
insertLayoutBlock lb (blk@(Block _) : nl) = blk : lb :nl
insertLayoutBlock _ _ = error "insertLayoutBlock: top instance is not Block"

