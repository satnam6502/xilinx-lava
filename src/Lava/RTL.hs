{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Lava.RTL
where
import Lava.Graph
import Lava.Hardware
import Control.Monad ( when )
import Control.Monad.State.Lazy (MonadState(put, get) )

data VecDir = UpTo | DownTo
              deriving (Eq, Show)

data NetKind = Bit | Vector
               deriving (Eq, Show)

data NetType (a::NetKind)
  = BitType
  | VecType Int VecDir Int (NetType a)
  | VoidType
  deriving (Eq, Show)

data Net (a::NetKind) = NamedNet String (NetType a)
                      | LocalNet Int (NetType a)
                      deriving (Eq, Show)

typeOfNet :: Net a -> NetType a
typeOfNet (NamedNet _ typ) = typ
typeOfNet (LocalNet _ typ) = typ

type Bit = Net 'Bit

data Statement
   = PrimitiveInstanceStatement PrimitiveInstance
   | forall a . LocalNetDeclaration Int (NetType a)
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
   | XorcyPrim Bit Bit Bit -- ci li o
   | MuxcyPrim Bit Bit Bit Bit -- ci di s o
   deriving Show

data PortDirection = InputPort | OutputPort
                     deriving (Eq, Show)

data PortSpec = forall (a::NetKind) . PortSpec PortDirection String (NetType a)

data LocalDec = forall (a::NetKind) . LocalDec Int (NetType a)

-- A netlist is represented as a list of components, a list of
-- port specifications and a tally of how many components are used
-- and how how many nets have been declared.
data Netlist = Netlist {
  moduleName :: String,    -- `moduleName` is the name to be used for the generated SystemVerilog module.
  clockName :: String,     -- `clockName` is the name of the clock net.
  clockUsed :: Bool,       -- `clockUsed` is true if the circuit to be generated is sequential (contains registers) so it needs a clock input.
  ports :: [PortSpec],     -- `ports` is a list of the input and output port definitions for the cirucit.
  localDecs :: [LocalDec]  -- `localDecs` is a list of local signal declarations.
  }

type RTL = Lava Netlist Statement

instance Hardware RTL (Net 'Bit) where
  inv :: Bit -> RTL Bit
  inv = invRTL
  and2 :: (Bit, Bit) -> RTL Bit
  and2 = binaryPrimitive AndPrim
  or2 = binaryPrimitive OrPrim
  xor2 = binaryPrimitive XorPrim
  nor2 = binaryPrimitive NorPrim
  xnor2 = binaryPrimitive XnorPrim
  delay = delayRTL
  xorcy = binaryPrimitive' XorcyPrim
  muxcy (s, (ci, di)) = input3Primitive MuxcyPrim (s, ci, di)


addLocalDec :: Int -> NetType a -> RTL ()
addLocalDec n typ
  = do graph <- get
       let gD = graphData graph
           lD =  localDecs gD
       put (graph{graphData = gD{localDecs = (LocalDec n typ):lD}})

mkNet :: NetType a -> RTL (Net a)
mkNet t
 = do e <- mkNewEdgeNumber
      addLocalDec e t
      return (LocalNet e t)

invRTL :: Bit -> RTL Bit
invRTL i
  = do o <- mkNet BitType
       mkNode (PrimitiveInstanceStatement (NotPrim i o))
       return o

binaryPrimitive :: ([Bit] -> Bit -> PrimitiveInstance) -> (Bit, Bit) -> RTL Bit
binaryPrimitive primitive (i0, i1)
  = do o <- mkNet BitType
       mkNode (PrimitiveInstanceStatement (primitive [i0, i1] o))
       return o

binaryPrimitive' :: (Bit -> Bit -> Bit -> PrimitiveInstance) -> (Bit, Bit) -> RTL Bit
binaryPrimitive' primitive (i0, i1)
  = do o <- mkNet BitType
       mkNode (PrimitiveInstanceStatement (primitive i0 i1 o))
       return o

input3Primitive :: (Bit -> Bit -> Bit -> Bit -> PrimitiveInstance) -> (Bit, Bit, Bit) -> RTL Bit
input3Primitive primitive (i0, i1, i2)
  = do o <- mkNet BitType
       mkNode (PrimitiveInstanceStatement (primitive i0 i1 i2 o))
       return o

getClockNet :: RTL Bit
getClockNet
  = do graph <- get
       let gD = graphData graph
       return (NamedNet (clockName gD) BitType)

setClockUsed :: RTL ()
setClockUsed
  = do graph <- get
       let gD = graphData graph
       put (graph{graphData = gD{clockUsed = True}})

delayRTL :: Net (a::NetKind) -> RTL (Net (a::NetKind))
delayRTL i
  = do o <- mkNet (typeOfNet i)
       clk <- getClockNet
       setClockUsed
       mkNode (Delay clk i o)
       return o

input :: String -> NetType (a::NetKind) -> RTL (Net a)
input name typ
  = do graph <- get
       let gD = graphData graph
           portList = ports gD
           port = PortSpec InputPort name typ
       put (graph {graphData = gD{ports = portList ++ [port]}})
       return (NamedNet name typ)

assignment :: Net (a::NetKind) -> Net (a::NetKind) -> RTL ()
assignment net expr = mkNode (Assignment net expr)

output :: String -> Net (a::NetKind) -> RTL ()
output name net
  = do graph <- get
       let gD = graphData graph
           portList = ports gD
           port = PortSpec OutputPort name typ
       put (graph {graphData = gD{ports = portList ++ [port]}})
       assignment (NamedNet name typ) net
    where
    typ = typeOfNet net

setModuleName :: String -> RTL ()
setModuleName name
  = do graph <- get
       let gD = graphData graph
       when (moduleName gD /= "") $
         error ("Module name already defined as: " ++ moduleName gD)
       put (graph{graphData=gD{moduleName = name}})

