module sub4_tb;

  logic[3:0] a, b;
  logic[3:0] subOut;
  logic carryOut;

  sub4 sub4_inst (a, b, subOut, carryOut);

  task test_case(input [3:0] test_a, input [3:0] test_b);
     a = test_a;
     b = test_b;
     #1;
     $display ("a=%d b=%d subOut=%d carryOut=%b", a, b, subOut, carryOut);
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
    $finish;
  end: run_tests

endmodule: sub4_tb
