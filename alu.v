`include "arithmetic.v"
`include "logic.v"

module mux2_1 (logicU, arithU, sel, out);
    input [31:0] logicU, arithU;
    input sel;
    output [31:0] out;
    assign out = sel ? arithU : logicU;
endmodule

module alu (a, b, f, y, zero);
    input[31:0] a, b;
    input [2:0] f;
    output [31:0] y;
    output zero;
    wire [31:0] outLogic, outArithmetic;

    logicUnit log(a, b, f, outLogic);
    arithmetic ari(a, b, f, outArithmetic);
    mux2_1 mux(outLogic, outArithmetic, f[1], y);
    assign zero = (y == 31'b0) ? 1 : 0;
endmodule