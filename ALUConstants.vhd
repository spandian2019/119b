----------------------------------------------------------------------------------
--
-- Constants for ALU
-- 
-- ALUConstants.vhd
-- 
-- This file contains constants for the ALU entity and testbench.
--
--  Revision History:
--     01/29/19 Sophia Liu initial revision
-- 
----------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

library opcodes; 
use opcodes.opcodes.all; 

package ALUConstants is
    constant REGSIZE    : natural := 8; 
    
    constant ZERO8     : std_logic_vector(7 downto 0) := "00000000"; 

    subtype ALU_OPS is std_logic_vector(3 downto 0);
    -- F-block operands 
    constant OP_ZERO    : ALU_OPS := "0000"; -- zeros
    constant OP_NOR     : ALU_OPS := "0001"; -- A nor B
    constant OP_NOTA    : ALU_OPS := "0011"; -- not A
    constant OP_NOTB    : ALU_OPS := "0101"; -- not B
    constant OP_XOR     : ALU_OPS := "0110"; -- A xor B
    constant OP_NAND    : ALU_OPS := "0111"; -- A nand B
    constant OP_AND     : ALU_OPS := "1000"; -- A and B
    constant OP_XNOR    : ALU_OPS := "1001"; -- A xnor B 
    constant OP_OR      : ALU_OPS := "1110"; -- A or B
    constant OP_ONE     : ALU_OPS := "1111"; -- true     
    
    -- Shifter/Rotator operands 
    constant OP_LSR     : ALU_OPS := "00--"; -- Logical shift right 
    constant OP_ASR     : ALU_OPS := "01--"; -- Arithmetic shift right
    constant OP_ROR     : ALU_OPS := "100-"; -- Rotate right (no carry)
    constant OP_RORC     : ALU_OPS := "101-"; -- Rotate right (with carry)
    
    -- Adder/Subtractor operands 
    constant OP_ADD     : ALU_OPS := "0---";
    constant OP_SUB     : ALU_OPS := "1---";
    constant OP_CARRY   : ALU_OPS := "-1--"; 
    constant OP_NOCARRY : ALU_OPS := "-0--";
    
    -- SReg
    constant HALFCARRYBIT : natural := 3; -- half carry is carry out of bit 3
    
    -- TODO merge
    subtype ALU_selects is std_logic_vector(1 downto 0);
    
    constant AddSubEn       : ALU_selects := "00";
    constant FBlockEn       : ALU_selects := "01";
    constant ShiftEn        : ALU_selects := "10";

end package ALUConstants;