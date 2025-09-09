module twoSorter_tb;

  logic[3:0] a, b;
  logic[3:0] c, d;

  twoSorter twoSorter_inst (a, b, c, d);

  task test_case(input [3:0] test_a, input [3:0] test_b);
     a = test_a;
     b = test_b;
     #1;
     if (c > d) begin
       $error("Output not sorted: a=%d b=%d c=%d d=%d", a, b,c, d);
     end
     $display ("a=%d b=%d c=%d d=%d", a, b,c, d);
  endtask: test_case 

  initial begin: run_tests
    test_case (3, 2);
    test_case (2, 3);
    test_case (6, 6);
    test_case (15, 3);
    test_case (8, 11);
    $display ("twoSorter test: PASS");
    $finish;
  end: run_tests

endmodule: twoSorter_tb
