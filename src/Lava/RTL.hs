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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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

deriving instance (Show (NetType a)) => Show (Net a)

typeOfNet :: Net a -> NetType a
typeOfNet Zero = BitType
typeOfNet One = BitType
typeOfNet (NamedNet _ typ) = typ
typeOfNet (LocalNet _ typ) = typ
typeOfNet (IndexedNet _ _ (VecType _ typ)) = typ
typeOfNet (VecLiteral _ typ) = typ

data RLOC = RLOC Int Int
            deriving (Eq, Show)

data Statement
   = PrimitiveInstanceStatement PrimitiveInstance
   | UNISIM (Maybe RLOC) UNISIMInstance
   | forall a . LocalNetDeclaration Int (NetType a)
   | forall a. Delay (Net Bit) (Net a) (Net a)
   | forall a. Assignment (Net a) (Net a)

deriving instance (Show Statement)

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
   deriving Show

data UNISIMInstance
  =  XorcyPrim (Net Bit) (Net Bit) (Net Bit) -- ci li o
   | MuxcyPrim (Net Bit) (Net Bit) (Net Bit) (Net Bit) -- ci di s o
   | Lut1Prim Int (Net Bit) (Net Bit)
   | Lut2Prim Int (Net Bit) (Net Bit) (Net Bit)
   | Lut3Prim Int (Net Bit) (Net Bit) (Net Bit) (Net Bit)
   | Carry4Prim (Net Bit) (Net Bit) (Array '[4] (Net Bit)) (Array '[4] (Net Bit)) (Array '[4] (Net Bit)) (Array '[4] (Net Bit)) -- ci cyinit di s o co
   | FDCEPrim (Net Bit) (Net Bit) (Net Bit) (Net Bit) (Net Bit)
   | BufGPrim (Net Bit) (Net Bit)
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
  clockName :: String,     -- `clockName` is the name of the clock net, default clk.
  clockNet :: Net Bit,     -- `clockNet` is the output of the BUFG that drivers the internal clock net.
  clockUsed :: Bool,       -- `clockUsed` is true if the circuit to be generated is sequential (contains registers) so it needs a clock input.
  resetName :: String,     -- `resetName` is the name of the reset net, defualt "rst".
  resetNet :: Net Bit,     -- `resetNet` is the global reset net, which may be an inverted version of the reset input.
  ports :: [PortSpec],     -- `ports` is a list of the input and output port definitions for the circuit.
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
  xorcy = binaryUnism' XorcyPrim
  muxcy :: (Net Bit, (Net Bit, Net Bit)) -> RTL (Net Bit)
  muxcy (s, (ci, di)) = input3UNISM MuxcyPrim (s, ci, di)
  lut1 :: (Bool -> Bool) -> Net Bit -> RTL (Net Bit)
  lut1 = lut1RTL
  lut2 :: (Bool -> Bool -> Bool) -> (Net Bit, Net Bit) -> RTL (Net Bit)
  lut2 = lut2RTL
  lut3 :: (Bool -> Bool -> Bool -> Bool) -> (Net Bit, Net Bit, Net Bit) -> RTL (Net Bit)
  lut3 = lut3RTL
  carry4 :: Net Bit -> Net Bit -> Array '[4] (Net Bit) -> Array '[4] (Net Bit) -> RTL (Array '[4] (Net Bit), Array '[4] (Net Bit))
  carry4 = carry4RTL
  reg :: Net Bit -> RTL (Net Bit)
  reg = regRTL
  (>->) :: (a -> RTL b) -> (b -> RTL c) -> a -> RTL c
  (>->) = leftToRightSerialComposition

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

binaryUnism' :: (Net Bit -> Net Bit -> Net Bit -> UNISIMInstance) -> (Net Bit, Net Bit) -> RTL (Net Bit)
binaryUnism' primitive (i0, i1)
  = do o <- mkNet BitType
       mkNode (UNISIM Nothing (primitive i0 i1 o))
       return o

input3Primitive :: (Net Bit -> Net Bit -> Net Bit -> Net Bit -> PrimitiveInstance) -> (Net Bit, Net Bit, Net Bit) -> RTL (Net Bit)
input3Primitive primitive (i0, i1, i2)
  = do o <- mkNet BitType
       mkNode (PrimitiveInstanceStatement (primitive i0 i1 i2 o))
       return o

input3UNISM :: (Net Bit -> Net Bit -> Net Bit -> Net Bit -> UNISIMInstance) -> (Net Bit, Net Bit, Net Bit) -> RTL (Net Bit)
input3UNISM primitive (i0, i1, i2)
  = do o <- mkNet BitType
       mkNode (UNISIM Nothing(primitive i0 i1 i2 o))
       return o

boolVecToInt :: [Bool] -> Int
boolVecToInt [] = 0
boolVecToInt (False:xs) = 2 * boolVecToInt xs
boolVecToInt (True:xs) =  1 + 2 * boolVecToInt xs

lut1RTL :: (Bool -> Bool) -> Net Bit -> RTL (Net Bit)
lut1RTL f i
  = do o <- mkNet BitType
       mkNode (UNISIM Nothing (Lut1Prim (boolVecToInt progBits) i o))
       return o
    where
    progBits = [f a | a <- [False, True]]

lut2RTL :: (Bool -> Bool -> Bool) -> (Net Bit, Net Bit) -> RTL (Net Bit)
lut2RTL f (i0, i1)
  = do o <- mkNet BitType
       mkNode (UNISIM Nothing (Lut2Prim (boolVecToInt progBits) i0 i1 o))
       return o
    where
    progBits = [f b a | a <- [False, True], b <- [False, True]]

lut3RTL :: (Bool -> Bool -> Bool -> Bool) -> (Net Bit, Net Bit, Net Bit) -> RTL (Net Bit)
lut3RTL f (i0, i1, i2)
  = do o <- mkNet BitType
       mkNode (UNISIM Nothing (Lut3Prim (boolVecToInt progBits) i0 i1 i2 o))
       return o
    where
    progBits = [f c b a | a <- [False, True], b <- [False, True], c <- [False, True]]

carry4RTL :: Net Bit -> Net Bit -> Array '[4] (Net Bit) -> Array '[4] (Net Bit) -> RTL (Array '[4] (Net Bit), Array '[4] (Net Bit))
carry4RTL ci cyinit di s
  = do o <- mkVecNet BitType
       co <- mkVecNet BitType
       mkNode (UNISIM Nothing (Carry4Prim ci cyinit di s o co))
       return (o, co)

bufG :: Net Bit -> RTL (Net Bit)
bufG i
  = do o <- mkNet BitType
       mkNode (UNISIM Nothing (BufGPrim i o))
       return o

setClockNet :: String -> RTL ()
setClockNet name
  = do graph <- get
       let gD = graphData graph
       put (graph{graphData = gD{clockName = name, clockNet = (NamedNet name BitType)}})

getClockNet :: RTL (Net Bit)
getClockNet
  = do graph <- get
       let gD = graphData graph
       return (clockNet gD)

setClockUsed :: RTL ()
setClockUsed
  = do graph <- get
       let gD = graphData graph
       put (graph{graphData = gD{clockUsed = True}})


setResetNet :: String -> RTL ()
setResetNet name
  = do graph <- get
       let gD = graphData graph
       put (graph{graphData = gD{resetName = name, resetNet = NamedNet name BitType}})

setActiveLowResetNet :: String -> RTL ()
setActiveLowResetNet name
  = do let resetInput = NamedNet name BitType
       invReset <- invRTL resetInput
       graph <- get
       let gD = graphData graph
       put (graph{graphData = gD{resetName = name, resetNet = invReset}})

getResetNet :: RTL (Net Bit)
getResetNet
  = do graph <- get
       let gD = graphData graph
       return (resetNet gD)

delayRTL :: Net (a::NetKind) -> RTL (Net (a::NetKind))
delayRTL i
  = do o <- mkNet (typeOfNet i)
       clk <- getClockNet
       setClockUsed
       mkNode (Delay clk i o)
       return o

regRTL :: Net Bit-> RTL (Net Bit)
regRTL i
  = do o <- mkNet (typeOfNet i)
       clk <- getClockNet
       rst <- getResetNet
       setClockUsed
       mkNode (UNISIM Nothing (FDCEPrim i clk One rst o))
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
       return (smash vecNet)
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

smash :: forall n k . KnownNat n => Net (VectorKind '[n] k) -> Array '[n] (Net k)
smash a = fromList [IndexedNet i a (typeOfNet a) | i <- [0..n'-1]]
          where
          n' = fromIntegral (GHC.TypeNats.natVal (Proxy @n))

unsmash :: forall n k . KnownNat n => Array '[n] (Net k) -> Net (VectorKind '[n] k)
unsmash a = VecLiteral a (VecType [n'] (typeOfNet (unScalar (a `index` 0))))
            where
            n' = fromIntegral (GHC.TypeNats.natVal (Proxy @n))

unsmashA :: forall n m a . (KnownNat n, KnownNat m) => Array '[n] (Array '[m] (Net a)) -> Array '[n] (Net (VectorKind '[m] a))
unsmashA = mapA unsmash

computeLayout :: [Layout (Int, Statement)] -> [(Int, Statement)]
computeLayout nl
  = concatBlocks nl2
    where
    nl2 = computeLayout' nl -- Compute beside and below

concatBlocks :: [Layout (Int, Statement)] -> [(Int, Statement)]
concatBlocks [] = []
concatBlocks ((Block b):nl) = b ++ concatBlocks nl
concatBlocks other = error ("concatBlocks: expected Block at head instance: " ++ show other)

computeLayout' :: [Layout (Int, Statement)] -> [Layout (Int, Statement)]
computeLayout' [] = []
computeLayout' ((Block b):nl) = Block b : computeLayout' nl
computeLayout' ((Beside a b):nl) = computeLayout' [a, b] ++ computeLayout' nl
computeLayout' ((Below a b):nl)  = computeLayout' [a, b] ++ computeLayout' nl

leftToRightSerialComposition :: (a -> RTL b) -> (b -> RTL c) -> a -> RTL c
leftToRightSerialComposition a b x
  = do pushGraph
       y <- a x
       aBlock <- popGraph
       pushGraph
       r <- b y
       bBlock <- popGraph
       addLayoutBlock (Beside aBlock bBlock)
       return r

