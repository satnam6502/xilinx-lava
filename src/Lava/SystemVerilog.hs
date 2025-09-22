{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Lava.SystemVerilog (writeSystemVerilog, writeSystemVerilogSimulation)
where
import Lava.Graph
import Lava.RTL
import Lava.SimVal
import qualified Data.BitVector as BV
import Data.List (transpose)
import Data.Array.Shaped
import GHC.TypeLits
import Numeric (showHex)

writeSystemVerilog :: RTL () -> IO ()
writeSystemVerilog topModule
  = do putStr ("Writing " ++ fileName ++ ".sv ...")
       writeFile (fileName ++ ".sv") (unlines (systemVerilogText fileName topModule))
       putStrLn ""
    where
    fileName = if moduleName nl == "" then
                 error "No module name set"
               else
                 moduleName nl
    graph = computeGraph (Netlist fileName "clk" Zero False "rst" (NamedNet "rst" BitType) [] []) topModule
    nl = graphData graph

systemVerilogText :: String -> RTL () -> [String]
systemVerilogText fileName topModule
  = ["module " ++ name ++ "("] ++
    declarePorts portList ++
    ["  );"
    ] ++
    ["(* DONT_TOUCH = \"true\" *)  " ++ showType typ ++ " net" ++ show n ++ ";" | LocalDec n typ <- localDecs gD] ++
    concatMap emitStatement components ++
    ["endmodule: " ++ name]
    where
    graph = computeGraph (Netlist name "clk" Zero False "rst" (NamedNet "rst" BitType) [] []) topModule
    gD = graphData graph
    components = computeLayout (nodes graph)
    portList = clkRstPorts ++ ports gD
    name = if moduleName gD == "" then fileName
           else moduleName gD
    clkRstPorts = if clockUsed gD then
                    [PortSpec InputPort (clockName gD) BitType, PortSpec InputPort (resetName gD) BitType]
                  else
                    []

declarePorts :: [PortSpec] -> [String]
declarePorts [] = []
declarePorts [p] = [declarePort "" p]
declarePorts (p:ps) = declarePort "," p : declarePorts ps

declarePort :: String -> PortSpec -> String
declarePort comma (PortSpec portDir name typ)
  = "  " ++ showPortDir portDir ++ " " ++ showType typ ++ " " ++ name ++ comma
    where
    showPortDir InputPort = "input"
    showPortDir OutputPort = "output"

showType :: NetType a -> String
showType net
  = case net of
       BitType -> "logic"
       VecType {} -> "logic" ++ showVecType net

showVecType :: NetType a ->  String
showVecType BitType = ""
showVecType (VecType idxs typ) = showVecIndexType idxs ++ showVecType typ

showVecIndexType :: [Int] -> String
showVecIndexType idxs = concat ["[" ++ show (hi-1) ++ ":0]" | hi <- idxs]

emitStatement :: (Int, Statement) -> [String]
emitStatement (n, PrimitiveInstanceStatement inst) = instantiateComponent n inst
emitStatement (n, UNISIM bel rloc inst) = [instantiateUNISIM n bel rloc inst]
emitStatement (_, LocalNetDeclaration n typ) = ["  " ++ showType typ ++ " net" ++ show n ++ ";"]
emitStatement (_, Delay clk lhs rhs) = ["  always_ff @(posedge " ++ showNet clk ++ ") " ++ showNet lhs ++ " <= " ++ showNet rhs ++ ";"]
emitStatement (_, Assignment lhs rhs) = ["  assign " ++ showNet lhs ++ " = " ++ showNet rhs ++ ";"]

intToHex :: Int -> String
intToHex n = showHex n ""

instantiateComponent :: Int -> PrimitiveInstance -> [String]
instantiateComponent ic component
  = case component of
      BufPrim i o      ->  ["  buf buf_" ++ show ic ++ " " ++ showArgs [o, i] ++ ";"]
      NotPrim i o      ->  ["  not not_" ++ show ic ++ " " ++ showArgs [o, i] ++ ";"]
      AndPrim inputs o ->  ["  and and_" ++ show ic ++ " " ++ showArgs (o:inputs) ++ ";"]
      OrPrim inputs o  ->  ["  or or_" ++ show ic ++ " " ++ showArgs (o:inputs) ++ ";"]
      NorPrim inputs o  -> ["  nor nor_" ++ show ic ++ " " ++ showArgs (o:inputs) ++ ";"]
      XorPrim inputs o  -> ["  xor xor_" ++ show ic ++ " " ++ showArgs (o:inputs) ++ ";"]
      XnorPrim inputs o  -> [" xnor xnor_" ++ show ic ++ " " ++ showArgs (o:inputs) ++ ";"]
      Xor2Prim cin part_sum o -> ["  XOR2 xor2_" ++ show ic ++ " " ++ showNamedArgs ["I0", "I1", "O"] [cin, part_sum, o] ++ ";"]

instantiateUNISIM :: Int -> Maybe BEL -> Maybe RLOC -> UNISIMInstance -> String
instantiateUNISIM ic bel rloc component = showLayout (showBEL bel ++ showRLOC rloc) ++ instantiateUNISIM' ic component ++ " // " ++ show rloc

instantiateUNISIM' :: Int -> UNISIMInstance -> String
instantiateUNISIM' ic component
  = case component of
      XorcyPrim cin part_sum o -> "  XORCY xorcy_" ++ show ic ++ " " ++ showNamedArgs ["CI", "LI", "O"] [cin, part_sum, o] ++ ";"
      MuxcyPrim s ci di o -> "  MUXCY muxcy_" ++ show ic ++ " " ++ showNamedArgs ["CI", "DI", "S", "O"] [ci, di, s, o] ++ ";"
      Lut1Prim config i o -> "  LUT1 #(.INIT(2'h" ++ intToHex config ++ ")) lut1_" ++ show ic ++ showNamedArgs ["I", "O"] [i, o] ++ ";"
      Lut2Prim config i0 i1 o -> "  LUT2 #(.INIT(4'h" ++ intToHex config ++ ")) lut2_" ++ show ic ++ showNamedArgs ["I0", "I1", "O"] [i0, i1, o] ++ ";"
      Lut3Prim config i0 i1 i2 o -> "  LUT3 #(.INIT(8'h" ++ intToHex config ++ ")) lut3_" ++ show ic ++ showNamedArgs ["I0", "I1", "I2", "O"] [i0, i1,i2,  o] ++ ";"
      Carry4Prim ci cyinit di s o co ->  "  CARRY4 carry4_" ++ show ic ++ " (" ++
                                         ".CI(" ++ showNet ci ++ "), " ++
                                         ".CYINIT(" ++ showNet cyinit ++ "), " ++
                                         ".DI(" ++ showArrayNet di ++ ")," ++
                                         ".S(" ++ showArrayNet s ++ ")," ++
                                         ".O(" ++ showArrayNet o ++ ")," ++
                                         ".CO(" ++ showArrayNet co ++ "));"
      FDCEPrim d c ce clr o -> "  FDCE  fdce_" ++ show ic ++ " " ++ showNamedArgs ["D", "C", "CE", "CLR", "Q"] [d, c, ce, clr, o] ++ ";"
      BufGPrim i o -> "  BUFG bufg_" ++ show ic ++ " " ++ showNamedArgs ["I", "O"] [i, o] ++ ";"               

showLayout :: [String] -> String
showLayout [] = ""
showLayout xs = "  (* " ++ insertString ", " xs ++ " *) "

showRLOC :: Maybe RLOC -> [String]
showRLOC Nothing = []
showRLOC (Just (RLOC x y)) = ["RLOC = \"X" ++ show x ++ "Y" ++ show (y `div` 4) ++ "\""]

showBEL :: Maybe BEL -> [String]
showBEL Nothing = []
showBEL (Just bel) = ["BEL = \"" ++ show bel ++ "\""]

showNet :: Net a -> String
showNet signal
  = case signal of
      Zero -> "1'b0"
      One -> "1'b1"
      NamedNet name _ -> name
      LocalNet n _ -> "net" ++ show n
      IndexedNet i v _ -> showNet v ++ "[" ++ show i ++ "]"
      VecLiteral a _ -> "{" ++ insertString "," (map showNet (reverse (toList a))) ++ "}"

showArrayNet :: KnownNat n => Array '[n] (Net a) -> String
showArrayNet a = showNet (VecLiteral a undefined)

showArgs :: [Net a] -> String
showArgs args = "(" ++ (insertCommas (map showNet args)) ++ ")"

showNamedArgs :: [String] -> [Net a] -> String
showNamedArgs names args =  "(" ++ insertCommas (map showNamedArg (zip names args)) ++ ")"

showNamedArg :: (String, Net a) -> String
showNamedArg (name, arg) = "." ++ name ++ "(" ++ showNet arg ++ ")"

insertString :: String -> [String] -> String
insertString _ [] = []
insertString _ [x] = x
insertString s (x:xs) = (x ++ s) ++ insertString s xs

insertCommas :: [String] -> String
insertCommas = insertString ", "

simTables :: [PortSpec] -> [[SimVal a]] -> [String]
simTables portSpecs simVals
  = concat [simTable p v | (p, v) <- zip portSpecs transposedVals]
    where
    transposedVals = Data.List.transpose simVals

showSimVal :: SimVal a -> String
showSimVal L = "1'b0"
showSimVal H = "1'b1"
showSimVal (BitVec v) = showSimVal (boolVec (BV.toBits v))
showSimVal (Vec vals) = "{" ++ insertString "," (map showSimVal vals) ++ "}"

simTable :: PortSpec -> [SimVal a] -> [String]
simTable (PortSpec _ name typ) simVals
  = ["  " ++ showType typ ++ " " ++ name ++ "_vectors[" ++ show n ++ "] = '{" ++ insertString "," values ++ "};",
     "  " ++ showType typ ++ " " ++ name ++ ";",
     "  assign " ++ name ++ " = " ++ name ++ "_vectors[cycle];"
    ]
    where
    n = length simVals
    values = map showSimVal simVals

writeOutput :: PortSpec -> String
writeOutput (PortSpec _ name BitType) = "    $fdisplay(fd, \"%0b\", " ++ name ++ ");"
writeOutput (PortSpec _ name (VecType _ _)) = "    $fdisplay(fd, \"%0h\", " ++ name ++ ");"

systemVerilogSimulationText :: Netlist -> [[SimVal a]] -> [String]
systemVerilogSimulationText nl simVals
  = ["module " ++ simName  ++ "("] ++
     outPorts ++
    ["  );",
     "",
     "  int fd;",
     "  initial begin",
     "    fd = $fopen(\"" ++ simName ++ ".txt\", \"w\");",
     "  end",
     "",
     "  logic clk;",
     "  initial clk = 0;",
     "  always #10 clk <= ~clk;",
     "  integer cycle;",
     "  initial cycle = 0;",
     "",
     "  always @(posedge " ++ clockName nl ++") begin: cycle_counter"] ++
     map writeOutput outputPorts ++
    ["    if (cycle == " ++ show (n-1) ++ ") begin",
     "      $fclose(fd);",
     "      $finish(1);",
     "    end",
     "    else begin",
     "      cycle <= cycle + 1;",
     "    end",
     "  end: cycle_counter;",
     "",
     "  " ++ name ++ " " ++ name ++ "_dut (.*);",
     ""] ++
     simTables inputPorts simVals ++
     ["",
     "  initial begin",
     "    $dumpfile(\"" ++ simName ++ ".vcd\");",
     "    $dumpvars;",
     "  end",
     ""] ++
     ["endmodule: " ++ simName]
    where
    n = length simVals
    name = moduleName nl
    simName = name ++ "_sim"
    inputPorts = [p | p@(PortSpec InputPort _ _) <- ports nl]
    outPorts = declarePorts outputPorts
    outputPorts = [p | p@(PortSpec OutputPort _ _) <- ports nl]

writeSystemVerilogSimulation :: RTL () -> [[SimVal a]] -> IO ()
writeSystemVerilogSimulation topModule simVals
  = writeFile simFilename (unlines (systemVerilogSimulationText nl simVals))
    where
    graph = computeGraph (Netlist fileName "clk" Zero False "rst" (NamedNet "rst" BitType) [] []) topModule
    nl = graphData graph
    fileName = moduleName nl
    simFilename = fileName ++ "_sim.sv"



