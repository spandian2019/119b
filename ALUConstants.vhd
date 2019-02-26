----------------------------------------------------------------------------------
--
-- Constants for ALU
--
-- ALUConstants.vhd
--
-- This file contains constants and definitions for the ALU entity. 
--
--  Revision History:
--     01/29/19     Sophia Liu      initial revision
--     02/01/2019   Sundar Pandian  Debugged with testbench
--     02/07/2019   Sundar Pandian  changed to reflect ALU.vhd changes
--     02/07/2019   Sundar Pandian  added support for 16 bit address adder
--
----------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;

package ALUConstants is
    constant HALFCARRYBIT : natural := 3; -- half carry is carry out of bit 3
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
--      Sum: std_logic - 1 bit sum of A, B, and Cin
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
