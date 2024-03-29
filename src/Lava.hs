-- | Xilinx Lava is a library for FPGA circuit design with a focus
--   on circuit layout.

module Lava
where

-- | Fork captures sharing.
fork :: Monad m => a -> m (a, a)
fork a = return (a, a)

