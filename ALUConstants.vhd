----------------------------------------------------------------------------------
--
-- Constants for ALU
--
-- ALUConstants.vhd
--
-- This file contains constants for the ALU entity and testbench.
--
--  Revision History:
--     01/29/19     Sophia Liu      initial revision
--     02/01/2019   Sundar Pandian  Debugged with testbench
--
----------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

use work.opcodes.all;

package ALUConstants is
	-- general constants 
    constant REGSIZE    : natural := 8; -- size of registers
    constant ZERO8     : std_logic_vector(7 downto 0) := "00000000";

    subtype ALU_FOPS is std_logic_vector(3 downto 0);
    -- F-block operands
    constant OP_XOR     : ALU_FOPS := "0110"; -- A xor B
    constant OP_AND     : ALU_FOPS := "1000"; -- A and B
    constant OP_OR      : ALU_FOPS := "1110"; -- A or B
    constant OP_NOTB    : ALU_FOPS := "0011"; -- not B (set for subtraction)

	-- F-block operands not used in instruction set
    constant OP_NOTA    : ALU_FOPS := "0101"; -- not A
    constant OP_ZERO    : ALU_FOPS := "0000"; -- zeros
    constant OP_NOR     : ALU_FOPS := "0001"; -- A nor B
    constant OP_ONE     : ALU_FOPS := "1111"; -- true
    constant OP_XNOR    : ALU_FOPS := "1001"; -- A xnor B
    constant OP_NAND    : ALU_FOPS := "0111"; -- A nand B

    subtype ALU_OPS is std_logic_vector(1 downto 0); -- ops for s/r, add
    -- Shifter/Rotator operands
    constant OP_LSR     : ALU_OPS := "00"; -- Logical shift right
    constant OP_ASR     : ALU_OPS := "01"; -- Arithmetic shift right
    constant OP_ROR     : ALU_OPS := "10"; -- Rotate right
    --constant OP_RORC     : ALU_OPS := "-110"; -- Rotate right (with carry)

	-- Adder/Subtractor operand indices and values
    constant SUBFLAG    : integer := 0;	-- index of subtract flag
	
	-- values of subtract/carry flags 
	constant OP_ADD : std_logic := '0';
	constant OP_SUB : std_logic := '1';

    -- Testing add/sub
    constant OP_ADDNC     : ALU_OPS := "00--";-- add no carry
    constant OP_SUBNC     : ALU_OPS := "11--";-- sub no carry

    -- SReg
    constant HALFCARRYBIT : natural := 3; -- half carry is carry out of bit 3

	-- ALUSel signals
    subtype ALU_SELECTS is std_logic_vector(2 downto 0);
	constant ADDSUBEN       : ALU_SELECTS := "000";
    constant FBLOCKEN       : ALU_SELECTS := "001";
    constant SHIFTEN        : ALU_SELECTS := "010";
    constant PASSTHRUEN     : ALU_SELECTS := "011";
    constant MULEN          : ALU_SELECTS := "100"; 

    subtype CARRY_SEL is std_logic_vector(1 downto 0); 
    constant CARRY_ZERO : CARRY_SEL := "00"; 
    constant CARRY_SET : CARRY_SEL := "01"; 
    constant CARRY_REG : CARRY_SEL := "10"; 
    constant CARRY_NREG: CARRY_SEL := "11"; 
end package ALUConstants;

----------------------------------------------------------------------------
--
--  1 Bit Full Adder
--
--  Implementation of a full adder. This entity takes the one bit 
--  inputs A and B with a carry in input and outputs the sum and carry 
--  out bits, using combinational logic. 
--
-- Inputs:
--      A: std_logic - 1 bit adder input
--      B: std_logic - 1 bit adder input
--      Cin: std_logic - 1 bit carry in input
--
-- Outputs:
--      Sum: std_logic - 1 bit sum or difference of A, B, and Cin
--      Cout: std_logic - 1 bit carry out value 
--
--  Revision History:
--      11/21/18  Sophia Liu    Initial revision.
--      11/22/18  Sophia Liu    Updated comments.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fullAdder is
    port(
        A           :  in      std_logic;  -- adder input 
        B           :  in      std_logic;  -- adder input 
        Cin         :  in      std_logic;  -- carry in value 
        Cout        :  out     std_logic;  -- carry out value 
        Sum         :  out     std_logic   -- sum of A, B with carry in
      );
end fullAdder;

architecture fullAdder of fullAdder is
    begin
        -- combinational logic for calculating A+B with carry in and out bits
        Sum <= A xor B xor Cin;
        Cout <= (A and B) or (A and Cin) or (B and Cin); 
end fullAdder;