// Comprehensive SystemVerilog Testbench for adder4
module adder4_tb;

    // Testbench signals
    logic [3:0] a, b;
    logic [3:0] sumOut;
    
    // Expected results for checking
    logic [4:0] full_result;
    logic [3:0] expected_sumOut;
    
    // Test statistics
    int test_count = 0;
    int pass_count = 0;
    int fail_count = 0;
    int overflow_count = 0;

    // Instantiate the Device Under Test (DUT)
    adder4 dut (
        .a(a),
        .b(b),
        .sumOut(sumOut)
    );

    // Clock for sequential operations and coverage sampling
    logic clk = 0;
    always #5 clk = ~clk;

    // Test stimulus and checking
    initial begin
        $display("=== 4-bit Adder (adder4) Testbench ===");
        $display("Time\t\tA\tB\tSumOut\tExpected\tOverflow\tStatus");
        $display("----------------------------------------------------------------");
        
        // Test all possible combinations (16x16 = 256 tests)
        for (int i = 0; i < 16; i++) begin
            for (int j = 0; j < 16; j++) begin
                a = i;
                b = j;
                
                // Wait for combinational delay
                #1;
                
                // Calculate expected result
                full_result = a + b;
                expected_sumOut = full_result[3:0];
                
                // Check results
                check_result();
                
                // Display result
                $display("%0t\t\t%d\t%d\t%d\t%d\t\t%s\t\t%s", 
                       $time, a, b, sumOut, expected_sumOut,
                       (full_result > 15) ? "YES" : "NO",
                       (sumOut == expected_sumOut) ? "PASS" : "FAIL");
                
                // Small delay between tests
                #1;
            end
        end
        
        // Specific edge case tests
        $display("\n=== Edge Case Tests ===");
        
        // Zero tests
        test_case(4'd0, 4'd0, "Both inputs zero");
        test_case(4'd0, 4'd15, "One input zero, other max");
        test_case(4'd15, 4'd0, "One input max, other zero");
        
        // Maximum values (overflow cases)
        test_case(4'd15, 4'd15, "Both inputs maximum (overflow)");
        test_case(4'd15, 4'd1, "Max + 1 (overflow)");
        test_case(4'd8, 4'd8, "Half max + half max");
        
        // Power of 2 tests
        test_case(4'd1, 4'd1, "1 + 1");
        test_case(4'd2, 4'd2, "2 + 2");
        test_case(4'd4, 4'd4, "4 + 4");
        test_case(4'd8, 4'd8, "8 + 8");
        
        // Boundary tests around overflow
        test_case(4'd7, 4'd8, "7 + 8 (no overflow)");
        test_case(4'd7, 4'd9, "7 + 9 (overflow)");
        test_case(4'd8, 4'd7, "8 + 7 (no overflow)");
        test_case(4'd9, 4'd7, "9 + 7 (overflow)");
        
        // Random pattern tests
        test_case(4'b1010, 4'b0101, "Alternating patterns");
        test_case(4'b1100, 4'b0011, "Complementary patterns");
        test_case(4'b1111, 4'b0001, "All ones + one");
        
        // Final test summary
        $display("\n=== Test Summary ===");
        $display("Total Tests: %0d", test_count);
        $display("Passed: %0d", pass_count);
        $display("Failed: %0d", fail_count);
        $display("Overflow Cases: %0d", overflow_count);
        $display("Success Rate: %.2f%%", (real'(pass_count)/real'(test_count))*100);
        
        if (fail_count == 0) begin
            $display("✓ ALL TESTS PASSED!");
        end else begin
            $display("✗ %0d TESTS FAILED!", fail_count);
        end
        
        $display("\nNote: This adder truncates results to 4 bits.");
        $display("Results > 15 will wrap around (modulo 16 arithmetic).");
        
        $finish;
    end

    // Task to perform individual test case
    task test_case(input [3:0] test_a, input [3:0] test_b, input string description);
        a = test_a;
        b = test_b;
        #1; // Wait for combinational delay
        
        full_result = a + b;
        expected_sumOut = full_result[3:0];
        
        check_result();
        
        $display("%s: A=%d, B=%d -> SumOut=%d, Expected=%d, FullSum=%d [%s]",
                description, a, b, sumOut, expected_sumOut, full_result,
                (sumOut == expected_sumOut) ? "PASS" : "FAIL");
    endtask

    // Task to check and update test statistics
    task check_result();
        test_count++;
        
        // Check for overflow condition
        if (full_result > 15) begin
            overflow_count++;
        end
        
        if (sumOut == expected_sumOut) begin
            pass_count++;
        end else begin
            fail_count++;
            $error("MISMATCH at time %0t: A=%d, B=%d, Got SumOut=%d, Expected=%d",
                   $time, a, b, sumOut, expected_sumOut);
        end
    endtask

    // SystemVerilog Assertions for continuous checking
    property adder_correctness;
        @(posedge clk) 
        sumOut == (a + b);
    endproperty
    
    assert property (adder_correctness)
        else $error("Assertion failed: Adder output incorrect at time %0t", $time);

    // Waveform dumping for debugging
    initial begin
        $dumpfile("adder4.vcd");
        $dumpvars(0, tb_adder4);
    end

endmodule
