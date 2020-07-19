module logicUnit (A, B, opcode, out);
    input[31:0] A, B;
    input [2:0] opcode;
    output [31:0] out;
    reg [31:0] out;

    parameter and_ = 3'b000, or_ = 3'b001;

    always @ (A or B or opcode) begin
        casex (opcode)
            and_: out = A & B;
            or_: out = A | B;
            default: out = 31'b0;
        endcase
    end

endmodule