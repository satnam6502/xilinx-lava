module twoSorterReg_tb();
  
  // Clock and Reset signals
  logic clk;
  logic rstN;
  
  // Test signals
  logic [3:0] a;      // First input value
  logic [3:0] b;      // Second input value
  logic [3:0] c;      // First output (should be min)
  logic [3:0] d;      // Second output (should be max)
  
  // Test control signals
  integer test_count = 0;
  integer pass_count = 0;
  integer fail_count = 0;
  
  // Clock generation - 10ns period (100MHz)
  initial begin
    clk = 0;
    forever #5 clk = ~clk;
  end
  
  // Instantiate the DUT (Device Under Test)
  twoSorterReg dut (
    .clk(clk),
    .rstN(rstN),
    .a(a),
    .b(b),
    .c(c),
    .d(d)
  );
  
  // Task to apply reset
  task apply_reset();
    begin
      $display("\n[RESET] Applying reset...");
      @(posedge clk);
      rstN = 0;
      a = 4'h0;
      b = 4'h0;
      repeat(2) @(posedge clk);
      rstN = 1;
      @(posedge clk);
      $display("[RESET] Reset complete\n");
    end
  endtask
  
  // Task to apply input and wait for output
  task apply_input(input logic [3:0] in_a, input logic [3:0] in_b);
    begin
      @(posedge clk);
      a = in_a;
      b = in_b;
      @(posedge clk); // Wait one clock cycle for output
    end
  endtask
  
  // Function to check if output is sorted and data preserved
  function automatic bit verify_sorter(
    input logic [3:0] in_a,
    input logic [3:0] in_b,
    input logic [3:0] out_c,
    input logic [3:0] out_d,
    output string failure_reason
  );
    // Calculate expected sorted values
    automatic logic [3:0] exp_c = (in_a <= in_b) ? in_a : in_b;
    automatic logic [3:0] exp_d = (in_a <= in_b) ? in_b : in_a;
    
    // Check if output matches expected
    if (out_c != exp_c || out_d != exp_d) begin
      failure_reason = $sformatf("Expected [%0d, %0d] but got [%0d, %0d]", 
                                  exp_c, exp_d, out_c, out_d);
      return 0;
    end
    
    // Verify sorting (c should be <= d)
    if (out_c > out_d) begin
      failure_reason = $sformatf("Output not sorted: c=%0d > d=%0d", out_c, out_d);
      return 0;
    end
    
    // Check data preservation (sum should be equal)
    if ((in_a + in_b) != (out_c + out_d)) begin
      failure_reason = $sformatf("Data not preserved: input_sum=%0d, output_sum=%0d", 
                                  (in_a + in_b), (out_c + out_d));
      return 0;
    end
    
    failure_reason = "";
    return 1;
  endfunction
  
  // Task to run a single test
  task run_test(
    input string test_name,
    input logic [3:0] in_a,
    input logic [3:0] in_b
  );
    automatic string failure_reason;
    automatic bit passed;
    
    test_count++;
    
    // Apply inputs
    apply_input(in_a, in_b);
    
    // Verify outputs
    passed = verify_sorter(in_a, in_b, c, d, failure_reason);
    
    if (passed) begin
      pass_count++;
      $display("[PASS] Test %0d: %s", test_count, test_name);
    end else begin
      fail_count++;
      $display("[FAIL] Test %0d: %s", test_count, test_name);
      $display("       Reason: %s", failure_reason);
    end
    $display("       Input:  a=%0d, b=%0d", in_a, in_b);
    $display("       Output: c=%0d, d=%0d", c, d);
    $display("");
  endtask
  
  // Task to test reset behavior
  task test_reset_behavior();
    begin
      $display("========================================");
      $display("Testing Reset Behavior");
      $display("========================================");
      
      // Apply some inputs
      apply_input(4'd10, 4'd5);
      $display("Before reset: c=%0d, d=%0d (inputs were a=10, b=5)", c, d);
      
      // Apply reset
      @(posedge clk);
      rstN = 0;
      @(posedge clk);
      $display("During reset: c=%0d, d=%0d", c, d);
      
      // Release reset and check
      rstN = 1;
      @(posedge clk);
      $display("After reset: c=%0d, d=%0d", c, d);
      
      // Verify normal operation resumes
      apply_input(4'd7, 4'd3);
      $display("After reset with new inputs (a=7, b=3): c=%0d, d=%0d", c, d);
      
      if (c == 3 && d == 7) begin
        $display("[PASS] Reset behavior test passed\n");
        pass_count++;
      end else begin
        $display("[FAIL] Reset behavior test failed\n");
        fail_count++;
      end
      test_count++;
    end
  endtask
  
  // Task to test pipeline behavior (back-to-back transactions)
  task test_pipeline_behavior();
    begin
      $display("========================================");
      $display("Testing Pipeline Behavior");
      $display("========================================");
      
      // Send multiple inputs back-to-back
      @(posedge clk);
      a = 4'd12; b = 4'd8;
      @(posedge clk);
      $display("Cycle 1: Input a=%0d, b=%0d -> Output c=%0d, d=%0d", 12, 8, c, d);
      
      a = 4'd3; b = 4'd9;
      @(posedge clk);
      $display("Cycle 2: Input a=%0d, b=%0d -> Output c=%0d, d=%0d", 3, 9, c, d);
      
      a = 4'd15; b = 4'd0;
      @(posedge clk);
      $display("Cycle 3: Input a=%0d, b=%0d -> Output c=%0d, d=%0d", 15, 0, c, d);
      
      a = 4'd7; b = 4'd7;
      @(posedge clk);
      $display("Cycle 4: Input a=%0d, b=%0d -> Output c=%0d, d=%0d", 7, 7, c, d);
      
      $display("");
    end
  endtask
  
  // Main test sequence
  initial begin
    $display("\n========================================");
    $display("Starting TwoSorterReg Testbench");
    $display("========================================\n");
    
    // Initialize signals
    rstN = 1;
    a = 4'h0;
    b = 4'h0;
    
    // Wait for clock to stabilize
    repeat(2) @(posedge clk);
    
    // Apply initial reset
    apply_reset();
    
    // Test 1: Equal values
    run_test("Equal values", 4'd5, 4'd5);
    
    // Test 2: Already sorted (a < b)
    run_test("Already sorted", 4'd3, 4'd7);
    
    // Test 3: Reverse sorted (a > b)
    run_test("Reverse sorted", 4'd10, 4'd2);
    
    // Test 4: Minimum and maximum values
    run_test("Min and max values", 4'd0, 4'd15);
    
    // Test 5: Maximum and minimum values (reversed)
    run_test("Max and min values", 4'd15, 4'd0);
    
    // Test 6: Adjacent values
    run_test("Adjacent values", 4'd7, 4'd8);
    
    // Test 7: Same high values
    run_test("Same high values", 4'd15, 4'd15);
    
    // Test 8: Same low values
    run_test("Same low values", 4'd0, 4'd0);
    
    // Test 9: Middle range values
    run_test("Middle range values", 4'd6, 4'd9);
    
    // Test 10: Boundary test
    run_test("Boundary test", 4'd1, 4'd14);
    
    // Random tests
    $display("========================================");
    $display("Random Tests");
    $display("========================================\n");
    
    for (int i = 0; i < 20; i++) begin
      automatic logic [3:0] rand_a = $urandom_range(0, 15);
      automatic logic [3:0] rand_b = $urandom_range(0, 15);
      run_test($sformatf("Random test %0d", i+1), rand_a, rand_b);
    end
    
    // Test reset behavior
    test_reset_behavior();
    
    // Test pipeline behavior
    test_pipeline_behavior();
    
    // Test reset during operation
    $display("========================================");
    $display("Testing Reset During Operation");
    $display("========================================");
    
    // Start a normal operation
    apply_input(4'd11, 4'd4);
    
    // Reset in the middle
    @(posedge clk);
    rstN = 0;
    a = 4'd13;  // These should be ignored during reset
    b = 4'd2;
    @(posedge clk);
    rstN = 1;
    @(posedge clk);
    
    // Check that sorter works after reset
    run_test("Operation after mid-cycle reset", 4'd8, 4'd12);
    
    // Display test summary
    $display("\n========================================");
    $display("Test Summary");
    $display("========================================");
    $display("Total Tests: %0d", test_count);
    $display("Passed:      %0d", pass_count);
    $display("Failed:      %0d", fail_count);
    
    if (fail_count == 0) begin
      $display("\n*** ALL TESTS PASSED! ***");
    end else begin
      $display("\n*** SOME TESTS FAILED! ***");
      $display("*** Please check your twoSorterReg implementation ***");
    end
    
    $display("\n========================================");
    $display("Testbench Complete");
    $display("========================================\n");
    
    // End simulation
    repeat(5) @(posedge clk);
    $finish;
  end
  
  // Timeout watchdog
  initial begin
    #10000;  // 10us timeout
    $display("\n[ERROR] Testbench timeout!");
    $finish;
  end
  
  // Optional: Add waveform dumping for debugging
  initial begin
    $dumpfile("twoSorterReg_tb.vcd");
    $dumpvars(0, twoSorterReg_tb);
  end
  
endmodule: twoSorterReg_tb