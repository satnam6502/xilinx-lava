{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoStarIsType #-}

module Lava.RTL
where
import Lava.Graph
import Lava.Hardware
import Control.Monad ( when )
import Control.Monad.State.Lazy (MonadState(put, get) )
import Data.Array.Shaped
import GHC.TypeLits
import GHC.TypeNats
import Data.Proxy 

data NetKind
  = Bit
  | VectorKind [Natural] NetKind
  deriving (Eq, Show)

data NetType (a::NetKind) where
  BitType :: NetType Bit
  VecType :: [Int] -> NetType a -> NetType (VectorKind n a)

deriving instance Eq (NetType a)
deriving instance Show (NetType a)

data Net (a::NetKind) where
   Zero :: Net Bit
   One :: Net Bit
   NamedNet :: String -> NetType a -> Net a
   LocalNet :: Int -> NetType a -> Net a
   IndexedNet :: Int -> Net (VectorKind n a) -> NetType (VectorKind n a) -> Net a
   VecLiteral :: forall (n :: Nat) a . KnownNat n => Array '[n] (Net a) -> NetType (VectorKind '[n] a) -> Net (VectorKind '[n] a)

typeOfNet :: Net a -> NetType a
typeOfNet Zero = BitType
typeOfNet One = BitType
typeOfNet (NamedNet _ typ) = typ
typeOfNet (LocalNet _ typ) = typ
typeOfNet (IndexedNet _ _ (VecType _ typ)) = typ
typeOfNet (VecLiteral _ typ) = typ

data Statement
   = PrimitiveInstanceStatement PrimitiveInstance
   | forall a . LocalNetDeclaration Int (NetType a)
   | forall a. Delay (Net Bit) (Net a) (Net a)
   | forall a. Assignment (Net a) (Net a)

-- SystemVerilog primitive gates, many of which take a variable
-- number of inputs.
data PrimitiveInstance
   = BufPrim  (Net Bit) (Net Bit)
   | NotPrim  (Net Bit) (Net Bit)
   | AndPrim  [Net Bit] (Net Bit)
   | OrPrim   [Net Bit] (Net Bit)
   | NorPrim  [Net Bit] (Net Bit)
   | XorPrim  [Net Bit] (Net Bit)
   | XnorPrim [Net Bit] (Net Bit)
   | Xor2Prim (Net Bit) (Net Bit) (Net Bit)
   | XorcyPrim (Net Bit) (Net Bit) (Net Bit) -- ci li o
   | MuxcyPrim (Net Bit) (Net Bit) (Net Bit) (Net Bit) -- ci di s o
   | Lut2Prim Int (Net Bit) (Net Bit) (Net Bit)
   | Lut3Prim Int (Net Bit) (Net Bit) (Net Bit) (Net Bit)
   | Carry4Prim (Net Bit) (Net Bit) (Array '[4] (Net Bit)) (Array '[4] (Net Bit)) (Array '[4] (Net Bit)) (Array '[4] (Net Bit)) -- ci cyinit di s o co

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

instance Hardware RTL (Net Bit) where
  zero :: RTL (Net Bit)
  zero = return Zero
  one :: RTL (Net Bit)
  one = return One
  invGate :: Net Bit -> RTL (Net Bit)
  invGate = invRTL
  andGate :: [Net Bit] -> RTL (Net Bit)
  andGate = binaryPrimitive AndPrim
  orGate :: [Net Bit] -> RTL (Net Bit)
  orGate = binaryPrimitive OrPrim
  xorGate :: [Net Bit] -> RTL (Net Bit)
  xorGate = binaryPrimitive XorPrim
  norGate :: [Net Bit] -> RTL (Net Bit)
  norGate = binaryPrimitive NorPrim
  xnorGate :: [Net Bit] -> RTL (Net Bit)
  xnorGate = binaryPrimitive XnorPrim
  delay :: Net Bit -> RTL (Net Bit)
  delay = delayRTL
  xorcy :: (Net Bit, Net Bit) -> RTL (Net Bit)
  xorcy = binaryPrimitive' XorcyPrim
  muxcy :: (Net Bit, (Net Bit, Net Bit)) -> RTL (Net Bit)
  muxcy (s, (ci, di)) = input3Primitive MuxcyPrim (s, ci, di)
  lut2 :: (Bool -> Bool -> Bool) -> (Net Bit, Net Bit) -> RTL (Net Bit)
  lut2 = lut2RTL
  lut3 :: (Bool -> Bool -> Bool -> Bool) -> (Net Bit, Net Bit, Net Bit) -> RTL (Net Bit)
  lut3 = lut3RTL
  carry4 :: Net Bit -> Net Bit -> Array '[4] (Net Bit) -> Array '[4] (Net Bit) -> RTL (Array '[4] (Net Bit), Array '[4] (Net Bit))
  carry4 = carry4RTL

addLocalDec :: Int -> NetType a -> RTL ()
addLocalDec n typ
  = do graph <- get
       let gD = graphData graph
           lD =  localDecs gD
       put (graph{graphData = gD{localDecs = LocalDec n typ:lD}})

mkNet :: NetType a -> RTL (Net a)
mkNet t
 = do e <- mkNewEdgeNumber
      addLocalDec e t 
      return (LocalNet e t)

mkVecNet :: forall n a . KnownNat n => NetType a -> RTL (Array '[n] (Net a))
mkVecNet t
  = do e <- mkNewEdgeNumber
       addLocalDec e (VecType [n'] t)
       let localNet = LocalNet e (VecType [n'] t)
       return (fromList [IndexedNet i localNet (VecType [n'] t) | i <- [0..n'-1]])
    where
    n' = fromIntegral (GHC.TypeNats.natVal (Proxy @n))

invRTL :: Net Bit -> RTL (Net Bit)
invRTL i
  = do o <- mkNet BitType
       mkNode (PrimitiveInstanceStatement (NotPrim i o))
       return o

binaryPrimitive :: ([Net Bit] -> Net Bit -> PrimitiveInstance) -> [Net Bit] -> RTL (Net Bit)
binaryPrimitive primitive inputs
  = do o <- mkNet BitType
       mkNode (PrimitiveInstanceStatement (primitive inputs o))
       return o

binaryPrimitive' :: (Net Bit -> Net Bit -> Net Bit -> PrimitiveInstance) -> (Net Bit, Net Bit) -> RTL (Net Bit)
binaryPrimitive' primitive (i0, i1)
  = do o <- mkNet BitType
       mkNode (PrimitiveInstanceStatement (primitive i0 i1 o))
       return o

input3Primitive :: (Net Bit -> Net Bit -> Net Bit -> Net Bit -> PrimitiveInstance) -> (Net Bit, Net Bit, Net Bit) -> RTL (Net Bit)
input3Primitive primitive (i0, i1, i2)
  = do o <- mkNet BitType
       mkNode (PrimitiveInstanceStatement (primitive i0 i1 i2 o))
       return o

boolVecToInt :: [Bool] -> Int
boolVecToInt [] = 0
boolVecToInt (False:xs) = 2 * boolVecToInt xs
boolVecToInt (True:xs) =  1 + 2 * boolVecToInt xs

lut2RTL :: (Bool -> Bool -> Bool) -> (Net Bit, Net Bit) -> RTL (Net Bit)
lut2RTL f (i0, i1)
  = do o <- mkNet BitType
       mkNode (PrimitiveInstanceStatement (Lut2Prim (boolVecToInt progBits) i0 i1 o))
       return o
    where
    progBits = [f b a | a <- [False, True], b <- [False, True]]


lut3RTL :: (Bool -> Bool -> Bool -> Bool) -> (Net Bit, Net Bit, Net Bit) -> RTL (Net Bit)
lut3RTL f (i0, i1, i2)
  = do o <- mkNet BitType
       mkNode (PrimitiveInstanceStatement (Lut3Prim (boolVecToInt progBits) i0 i1 i2 o))
       return o
    where
    progBits = [f c b a | a <- [False, True], b <- [False, True], c <- [False, True]]

carry4RTL :: Net Bit -> Net Bit -> Array '[4] (Net Bit) -> Array '[4] (Net Bit) -> RTL (Array '[4] (Net Bit), Array '[4] (Net Bit))
carry4RTL ci cyinit di s
  = do o <- mkVecNet BitType
       co <- mkVecNet BitType
       mkNode (PrimitiveInstanceStatement (Carry4Prim ci cyinit di s o co))
       return (o, co)

getClockNet :: RTL (Net Bit)
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

inputVec :: forall n a . KnownNat n => String -> NetType (a::NetKind) -> RTL (Array '[n] (Net a))
inputVec name typ
  = do vecNet <- input name (VecType [n'] typ)
       return (fromList [IndexedNet i vecNet (VecType [n'] typ) | i <- [0..n'-1]])
    where
    n' = fromIntegral (GHC.TypeNats.natVal (Proxy @n))

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

outputVec :: forall n a . KnownNat n => String -> Array '[n] (Net a) -> NetType (a::NetKind) -> RTL ()
outputVec name a typ
  = output name (VecLiteral a (VecType [n'] typ))
    where
    n' = fromIntegral (GHC.TypeNats.natVal (Proxy @n))

setModuleName :: String -> RTL ()
setModuleName name
  = do graph <- get
       let gD = graphData graph
       when (moduleName gD /= "") $
         error ("Module name already defined as: " ++ moduleName gD)
       put (graph{graphData=gD{moduleName = name}})

