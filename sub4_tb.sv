module sub4_tb;

  logic[3:0] a, b;
  logic[3:0] subOut;
  logic carryOut;

  sub4 sub4_inst (a, b, subOut, carryOut);

  logic[3:0] expectedSub;
  logic expectedCarry;

  task test_case(input [3:0] test_a, input [3:0] test_b);
     a = test_a;
     b = test_b;
     expectedSub = a - b;
     expectedCarry = a >= b ? 1'b1 : 1'b0;
     #1;
     if (subOut != expectedSub) begin
       $error ("Subtraction mismatch at time %0t: subOut=%d expectedSub=%d", $time,  subOut, expectedSub);
     end
     if (carryOut != expectedCarry) begin
       $error ("Carry out mismatch at time %0t: carryOut=%b expectedCarry=%d", $time, carryOut, expectedCarry);
     end
     $display ("a=%d b=%d subOut=%d expectedSub=%d carryOut=%b expectedCarry=%d", a, b, subOut, expectedSub, carryOut, expectedCarry);
  endtask: test_case 

  initial begin: run_tests
    test_case (4, 0);
    test_case (4, 1);
    test_case (4, 2);
    test_case (4, 3);
    test_case (4, 4);
    test_case (2, 4);
    test_case (2, 5);
    test_case (3, 3);
    test_case (0, 0);
    $display ("sub4 test: PASS");
    $finish;
  end: run_tests

endmodule: sub4_tb
