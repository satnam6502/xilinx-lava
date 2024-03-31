{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Lava.RTL
where
import Lava.Graph
import Lava.Hardware
import Control.Monad.State.Lazy

data VecDir = UpTo | DownTo
              deriving (Eq, Show)

data NetKind = Bit | Vector NetKind
               deriving (Eq, Show)

data NetType
  = BitType
  | VecType Int VecDir Int NetType
  deriving (Eq, Show)

data Net (a::NetKind) = NamedNet String NetType
                      | LocalNet Int NetType
                      deriving (Eq, Show)

typeOfNet :: Net a -> NetType
typeOfNet (NamedNet _ typ) = typ
typeOfNet (LocalNet _ typ) = typ

type Bit = Net 'Bit

data Statement
   = PrimitiveInstanceStatement PrimitiveInstance
   | LocalNetDeclaration Int NetType
   | forall a. Delay Bit (Net a) (Net a)
   | forall a. Assignment (Net a) (Net a)

-- SystemVerilog primitive gates, many of which take a variable
-- number of inputs.
data PrimitiveInstance
   = BufPrim  Bit Bit
   | NotPrim  Bit Bit
   | AndPrim  [Bit] Bit
   | OrPrim   [Bit] Bit
   | NorPrim  [Bit] Bit
   | XorPrim  [Bit] Bit
   | XnorPrim [Bit] Bit
   deriving Show

data PortDirection = InputPort | OutputPort
                     deriving (Eq, Show)

data PortSpec = PortSpec PortDirection String NetType

-- A netlist is represented as a list of components, a list of
-- port specifications and a tally of how many components are used
-- and how how many nets have been declared.
data Netlist = Netlist {
  moduleName :: String,
  clockName :: Maybe String,
  ports :: [PortSpec]
  }

type RTL = Lava Netlist Statement

instance Hardware RTL Bit where
  inv = invRTL
  and2 = and2RTL

mkNet :: NetType -> RTL Bit
mkNet t
 = do e <- mkNewEdgeNumber
      return (LocalNet e t)

invRTL :: Bit -> RTL Bit
invRTL i
  = do o <- mkNet BitType
       mkNode (PrimitiveInstanceStatement (NotPrim i o))
       return o

and2RTL :: (Bit, Bit) -> RTL Bit
and2RTL (i0, i1)
  = do o <- mkNet BitType
       mkNode (PrimitiveInstanceStatement (AndPrim [i0, i1] o))
       return o

input :: String -> NetType -> RTL (Net a)
input name typ
  = do graph <- get
       let gD = graphData graph
           portList = ports gD
           port = PortSpec InputPort name typ
       put (graph {graphData = gD{ports = portList ++ [port]}})
       return (NamedNet name typ)

assignment :: Net a -> Net a -> RTL ()
assignment net expr = mkNode (Assignment net expr)

output :: String -> Net a -> RTL ()
output name net
  = do graph <- get
       let gD = graphData graph
           portList = ports gD
           port = PortSpec OutputPort name typ
       put (graph {graphData = gD{ports = portList ++ [port]}})
       assignment (NamedNet name typ) net
    where
    typ =typeOfNet net

setModuleName :: String -> RTL ()
setModuleName name
  = do graph <- get
       let gD = graphData graph
       when (moduleName gD /= "") $
         error ("Module name already defined as: " ++ moduleName gD)
       put (graph{graphData=gD{moduleName = name}})

