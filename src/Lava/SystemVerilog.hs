{-# LANGUAGE GADTs #-}

module Lava.SystemVerilog (writeSystemVerilog, writeSystemVerilogSimulation)
where
import Lava.Graph
import Lava.RTL
import Lava.SimVal
import qualified Data.BitVector as BV
import Data.List (transpose)

writeSystemVerilog :: RTL () -> IO ()
writeSystemVerilog topModule
  = writeFile (fileName ++ ".sv") (unlines (systemVerilogText fileName topModule))
    where
    fileName = if moduleName nl == "" then
                 error "No module name set"
               else
                 moduleName nl
    graph = computeGraph (Netlist fileName "clk" False [] []) topModule
    nl = graphData graph

systemVerilogText :: String -> RTL () -> [String]
systemVerilogText fileName topModule
  = ["module " ++ name ++ "("] ++
    declarePorts portList ++
    ["  );",
     "  // BEGIN: local net declarations"
    ] ++
    ["  " ++ showType typ ++ " net" ++ show n ++ ";" | LocalDec n typ <- localDecs gD] ++
    ["  // END: local net declarations"] ++
    concatMap emitStatement components ++
    ["endmodule: " ++ name]
    where
    graph = computeGraph (Netlist name "clk" False [] []) topModule
    gD = graphData graph
    components = nodes graph
    portList = ports gD
    name = if moduleName gD == "" then fileName
           else moduleName gD

declarePorts :: [PortSpec] -> [String]
declarePorts [] = []
declarePorts [p] = [declarePort "" p]
declarePorts (p:ps) = declarePort "," p : declarePorts ps

declarePort :: String -> PortSpec -> String
declarePort comma (PortSpec portDir name typ)
  = "  " ++ showPortDir portDir ++ " " ++ showType typ ++ " " ++ name ++ comma ++ lint_waiver
    where
    lint_waiver = if length name < 2 then
                    " // ri lint_check_waive MIN_NAME_LEN"
                  else
                    ""
    showPortDir InputPort = "input"
    showPortDir OutputPort = "output"

showType :: NetType a -> String
showType net
  = case net of
       BitType -> "logic"
       VecType {} -> "logic" ++  showVecType net
       VoidType -> error "attempt to generate SystemVerilog for void type"

showVecType :: NetType a ->  String
showVecType BitType = ""
showVecType (VecType hi dir lo typ) = showVecIndexType hi dir lo ++ showVecType typ
showVecType VoidType = error "attempt to generate SystemVerilog array type for void type"

showVecIndexType :: Int -> VecDir -> Int -> String
showVecIndexType hi DownTo lo = "[" ++ show hi ++ ":" ++ show lo ++ "]"
showVecIndexType lo UpTo hi = "[" ++ show lo ++ ":" ++ show hi ++ "]"

emitStatement :: (Int, Statement) -> [String]
emitStatement (n, PrimitiveInstanceStatement inst) = instantiateComponent n inst
emitStatement (_, LocalNetDeclaration n typ) = ["  " ++ showType typ ++ " net" ++ show n ++ ";"]
emitStatement (_, Delay clk lhs rhs) = ["  always_ff @(posedge " ++ showNet clk ++ ") " ++ showNet lhs ++ " <= " ++ showNet rhs ++ ";"]
emitStatement (_, Assignment lhs rhs) = ["  assign " ++ showNet lhs ++ " = " ++ showNet rhs ++ ";"]

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

showNet :: Net a -> String
showNet signal
  = case signal of
      NamedNet name _ -> name
      LocalNet n _ -> "net" ++ show n

showArgs :: [Net a] -> String
showArgs args = "(" ++ (insertCommas (map showNet args)) ++ ")"

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
    transposedVals = transpose simVals

showSimVal :: SimVal a -> String
showSimVal L = "1'b0"
showSimVal H = "1'b1"
showSimVal (BitVec v) = showSimVal (boolVec (BV.toBits v))
showSimVal (Vec vals) = "{" ++ insertString "," (map showSimVal vals) ++ "}"

simTable :: PortSpec -> [SimVal a] -> [String]
simTable (PortSpec _ name typ) simVals
  = ["  " ++ showType typ ++ " " ++ name ++ "_vectors[" ++ show n ++ "] = '{" ++ insertString "," values ++ "}; // ri lint_check_waive INIT_ASSIGN",
     "  " ++ showType typ ++ " " ++ name ++ ";",
     "  assign " ++ name ++ " = " ++ name ++ "_vectors[cycle];"
    ]
    where
    n = length simVals
    values = map showSimVal simVals

writeOutput :: PortSpec -> String
writeOutput (PortSpec _ name BitType) = "    $fdisplay(fd, \"%0b\", " ++ name ++ ");"
writeOutput (PortSpec _ name (VecType _ _ _ _)) = "    $fdisplay(fd, \"%0h\", " ++ name ++ ");"
writeOutput (PortSpec _ _ VoidType) = error "Attempt to display void type" 

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
     "  logic clk = 0;",
     "  always #10 clk <= ~clk;",
     "  integer cycle = 0;",
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
    graph = computeGraph (Netlist fileName "clk" False [] []) topModule
    nl = graphData graph
    fileName = moduleName nl
    simFilename = fileName ++ "_sim.sv"



  