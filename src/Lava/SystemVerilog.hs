{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lava.SystemVerilog
where
import Lava.Graph
import Lava.Hardware


data VecDir = UpTo | DownTo
              deriving (Eq, Show)

data NetKind = Bit | Vector NetKind
               deriving (Eq, Show)

data NetType (a::NetKind) where
   BitType :: NetType 'Bit
   VecType :: Int -> VecDir -> Int -> NetType a -> NetType ('Vector a)

deriving instance Show (NetType a)
deriving instance Eq (NetType a)

data Net (a::NetKind) = NamedNet String (NetType a)
                      | LocalNet Int
                      deriving (Eq, Show)

type Bit = Net 'Bit

data Statement
   = PrimitiveInstanceStatement PrimitiveInstance
   | forall a. LocalNetDeclaration Int (NetType a)
   | forall a. Delay Bit (Net a)
   | forall a. Assignment (Net a)

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

data PortSpec = forall a . PortSpec PortDirection String (NetType a)

-- A netlist is represented as a list of components, a list of
-- port specifications and a tally of how many components are used
-- and how how many nets have been declared.
data Netlist = Netlist {
  moduleName :: String,
  clockName :: Maybe String,
  ports :: [PortSpec]
  }

type SV = Lava Netlist Statement

instance Hardware SV Bit where
  inv = invSV
  and2 = and2SV

mkNet :: SV Bit
mkNet = LocalNet <$> mkEdge

invSV :: Bit -> SV Bit
invSV i
  = do o <- mkNet
       mkNode (PrimitiveInstanceStatement (NotPrim i o))
       return o

and2SV :: (Bit, Bit) -> SV Bit
and2SV (i0, i1)
  = do o <- mkNet
       mkNode (PrimitiveInstanceStatement (AndPrim [i0, i1] o))
       return o

