module sorter4_tb;
  
  // Test signals
  logic [3:0][3:0] a;  // Input vector of 4 unsigned 4-bit numbers
  logic [3:0][3:0] b;  // Output sorted vector
  
  // Test control signals
  integer test_count = 0;
  integer pass_count = 0;
  integer fail_count = 0;
  
  // Instantiate the DUT (Device Under Test)
  sorterComb4 dut (
    .a(a),
    .b(b)
  );
  
  // Function to check if output is sorted
  function automatic bit check_sorted(input logic [3:0][3:0] arr);
    for (int i = 0; i < 3; i++) begin
      if (arr[i] > arr[i+1]) begin
        return 0; // Not sorted
      end
    end
    return 1; // Sorted
  endfunction
  
  // Function to check if all input elements are preserved in output
  function automatic bit check_data_preserved(
    input logic [3:0][3:0] input_arr,
    input logic [3:0][3:0] output_arr
  );
    automatic bit [3:0] input_counts[16];
    automatic bit [3:0] output_counts[16];
    
    // Initialize counters
    for (int i = 0; i < 16; i++) begin
      input_counts[i] = 0;
      output_counts[i] = 0;
    end
    
    // Count occurrences in input and output
    for (int i = 0; i < 4; i++) begin
      input_counts[input_arr[i]]++;
      output_counts[output_arr[i]]++;
    end
    
    // Compare counts for each value
    for (int i = 0; i < 16; i++) begin
      if (input_counts[i] != output_counts[i]) begin
        return 0; // Data not preserved
      end
    end
    
    return 1; // All data preserved
  endfunction
  
  // Combined verification function
  function automatic bit verify_sorter(
    input logic [3:0][3:0] input_arr,
    input logic [3:0][3:0] output_arr,
    output string failure_reason
  );
    if (!check_data_preserved(input_arr, output_arr)) begin
      failure_reason = "Data not preserved - elements lost or corrupted";
      return 0;
    end
    if (!check_sorted(output_arr)) begin
      failure_reason = "Output not properly sorted";
      return 0;
    end
    failure_reason = "";
    return 1;
  endfunction
  
  // Function to display test results
  function automatic void display_test(
    input string test_name,
    input logic [3:0][3:0] input_val,
    input logic [3:0][3:0] output_val
  );
    automatic string failure_reason;
    automatic bit passed;
    
    test_count++;
    passed = verify_sorter(input_val, output_val, failure_reason);
    
    if (passed) begin
      pass_count++;
      $display("[PASS] Test %0d: %s", test_count, test_name);
    end else begin
      fail_count++;
      $display("[FAIL] Test %0d: %s", test_count, test_name);
      $display("       Reason: %s", failure_reason);
    end
    $display("       Input:  [%0d, %0d, %0d, %0d]", 
             input_val[0], input_val[1], input_val[2], input_val[3]);
    $display("       Output: [%0d, %0d, %0d, %0d]", 
             output_val[0], output_val[1], output_val[2], output_val[3]);
    $display("");
  endfunction
  
  // Main test sequence
  initial begin
    $display("========================================");
    $display("Starting Sorter4 Testbench");
    $display("========================================\n");
    
    // Test 1: Already sorted ascending
    a = '{4'd1, 4'd2, 4'd3, 4'd4};
    #10;
    display_test("Already sorted ascending", a, b);
    
    // Test 2: Reverse sorted (descending)
    a = '{4'd15, 4'd10, 4'd5, 4'd0};
    #10;
    display_test("Reverse sorted", a, b);
    
    // Test 3: All same values
    a = '{4'd7, 4'd7, 4'd7, 4'd7};
    #10;
    display_test("All same values", a, b);
    
    // Test 4: Random values 1
    a = '{4'd3, 4'd9, 4'd1, 4'd6};
    #10;
    display_test("Random values 1", a, b);
    
    // Test 5: Random values 2
    a = '{4'd12, 4'd8, 4'd14, 4'd2};
    #10;
    display_test("Random values 2", a, b);
    
    // Test 6: Maximum and minimum values
    a = '{4'd15, 4'd0, 4'd15, 4'd0};
    #10;
    display_test("Max and min values", a, b);
    
    // Test 7: Two pairs of duplicates
    a = '{4'd5, 4'd3, 4'd5, 4'd3};
    #10;
    display_test("Two pairs of duplicates", a, b);
    
    // Test 8: One duplicate
    a = '{4'd8, 4'd4, 4'd8, 4'd11};
    #10;
    display_test("One duplicate", a, b);
    
    // Test 9: Boundary values
    a = '{4'd0, 4'd1, 4'd14, 4'd15};
    #10;
    display_test("Boundary values", a, b);
    
    // Test 10: Partially sorted
    a = '{4'd2, 4'd4, 4'd1, 4'd3};
    #10;
    display_test("Partially sorted", a, b);
    
    // Test 11: Data integrity test (the case that was failing)
    a = '{4'd7, 4'd3, 4'd11, 4'd2};
    #10;
    display_test("Data integrity test", a, b);
    
    // Additional random tests
    for (int i = 0; i < 10; i++) begin
      // Generate random input
      a[0] = $urandom_range(0, 15);
      a[1] = $urandom_range(0, 15);
      a[2] = $urandom_range(0, 15);
      a[3] = $urandom_range(0, 15);
      #10;
      display_test($sformatf("Random test %0d", i+1), a, b);
    end
    
    // Additional edge case tests
    $display("========================================");
    $display("Edge Case Tests");
    $display("========================================\n");
    
    // Test with specific pattern that might reveal bugs
    a = '{4'd2, 4'd11, 4'd3, 4'd7};
    #10;
    display_test("Edge case: mixed values", a, b);
    
    // Test alternating high/low
    a = '{4'd15, 4'd0, 4'd14, 4'd1};
    #10;
    display_test("Edge case: alternating high/low", a, b);
    
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
      $display("*** Please check your sorter4 implementation ***");
    end
    
    $display("\n========================================");
    $display("Testbench Complete");
    $display("========================================");
    
    $finish;
  end
  
  // Optional: Add waveform dumping for debugging
  initial begin
    $dumpfile("sorter4_tb.vcd");
    $dumpvars(0, sorter4_tb);
  end
  
endmodule: sorter4_tb
