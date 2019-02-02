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
    constant REGSIZE    : natural := 8;

    constant ZERO8     : std_logic_vector(7 downto 0) := "00000000";

    subtype ALU_OPS is std_logic_vector(3 downto 0);
    -- F-block operands
    constant OP_XOR     : ALU_OPS := "0110"; -- A xor B
    constant OP_AND     : ALU_OPS := "1000"; -- A and B
    constant OP_OR      : ALU_OPS := "1110"; -- A or B

	 -- F-block operands not used in instruction set
    constant OP_NOTA    : ALU_OPS := "0101"; -- not A
    constant OP_ZERO    : ALU_OPS := "0000"; -- zeros
    constant OP_NOR     : ALU_OPS := "0001"; -- A nor B
    constant OP_NOTB    : ALU_OPS := "0011"; -- not B
    constant OP_ONE     : ALU_OPS := "1111"; -- true
    constant OP_XNOR    : ALU_OPS := "1001"; -- A xnor B
    constant OP_NAND    : ALU_OPS := "0111"; -- A nand B

    -- Shifter/Rotator operands
    constant OP_LSR     : ALU_OPS := "--00"; -- Logical shift right
    constant OP_ASR     : ALU_OPS := "--01"; -- Arithmetic shift right
    constant OP_ROR     : ALU_OPS := "-010"; -- Rotate right (no carry)
    constant OP_RORC     : ALU_OPS := "-110"; -- Rotate right (with carry)


	 -- Adder/Subtractor operand indices and values
    constant SUBFLAG    : integer := 3;	-- index of subtract flag
    constant CARRYBIT   : integer := 2;	-- index of carry flag

	 constant OP_ADD : std_logic := '0';
	 constant OP_SUB : std_logic := '1';
	 constant OP_CARRY : std_logic:= '1';

    -- Testing add/sub
    constant OP_ADDNC     : ALU_OPS := "00--";-- add no carry
    constant OP_SUBNC     : ALU_OPS := "11--";-- sub no carry

    -- SReg
    constant HALFCARRYBIT : natural := 3; -- half carry is carry out of bit 3


    subtype ALU_selects is std_logic_vector(2 downto 0);

	 constant AddSubEn       : ALU_selects := "000";
    constant FBlockEn       : ALU_selects := "001";
    constant ShiftEn        : ALU_selects := "010";
    constant PassThruEn     : ALU_selects := "011";
    constant MulEn          : ALU_selects := "100";

end package ALUConstants;