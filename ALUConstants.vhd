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
--     02/07/2019   Sundar Pandian  changed to reflect ALU.vhd changes
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
    constant ZERO8      : std_logic_vector(7 downto 0) := "00000000";

    subtype  ALU_FOPS is std_logic_vector(3 downto 0);
    -- F-block operands
    constant FOP_ONES   : ALU_FOPS := "1111"; -- all ones
    constant FOP_ZERO   : ALU_FOPS := "0000"; -- all zeros
    constant FOP_A      : ALU_FOPS := "1100"; -- A
    constant FOP_NOTA   : ALU_FOPS := "0011"; -- not A
    constant FOP_B      : ALU_FOPS := "1010"; -- B
    constant FOP_NOTB   : ALU_FOPS := "0101"; -- not B
    constant FOP_XOR    : ALU_FOPS := "0110"; -- A xor B
    constant FOP_XNOR   : ALU_FOPS := "1001"; -- A xnor B
    constant FOP_AND    : ALU_FOPS := "1000"; -- A and B
    constant FOP_NAND   : ALU_FOPS := "0111"; -- A nand B
    constant FOP_OR     : ALU_FOPS := "1110"; -- A or B
    constant FOP_NOR    : ALU_FOPS := "0001"; -- A nor B

    subtype  ALU_SR is std_logic_vector(1 downto 0); -- ops for s/r, add
    -- Shifter/Rotator operands
    constant SR_LSR     : ALU_SR := "00"; -- Logical shift right
    constant SR_ASR     : ALU_SR := "01"; -- Arithmetic shift right
    constant SR_ROR     : ALU_SR := "10"; -- Rotate right -- is this right? carry? 

	-- Adder/Subtractor operand indices and values
    constant SUBFLAG    : integer := 0;	-- index of subtract flag
	
	-- values of subtract/carry flags 
	constant OP_ADD     : std_logic := '0';
	constant OP_SUB     : std_logic := '1';

    -- nSUB bit values OR with carry select values to form ALU_ADDSUB
    subtype  ALU_ADDSUB is std_logic_vector(2 downto 0);
    -- subtract flag values
    constant ALU_ADD    : ALU_ADDSUB := "000"; -- add
    constant ALU_SUB    : ALU_ADDSUB := "001"; -- sub
    -- carry select values
    constant RST_CARRY  : ALU_ADDSUB := "000"; -- carry in = '0'
    constant SET_CARRY  : ALU_ADDSUB := "010"; -- carry in = '1'
    constant CARRY_IN   : ALU_ADDSUB := "100"; -- carry in = Cin
    constant NCARRY_IN  : ALU_ADDSUB := "110"; -- carry in = nCin

    subtype  ALU_COMNEG is std_logic_vector(1 downto 0);
    -- COM and NEG control signals for AND and OR gates
    constant OP_CN_ADD  : integer := 1;
    -- ALU_COMNEG(1) = control signal into OR gate
    --                    set to 0 for pass thru
    --                    set to 1 for setting high
    constant OP_CN_OR   : integer := 0;
    -- ALU_COMNEG(0) = control signal into AND gate
    --                    set to 1 for pass thru
    --                    set to 0 for setting low
    constant COMNEG_NONE : ALU_COMNEG := "01"; -- pass thru
    constant ALU_COM     : ALU_COMNEG := "00"; -- set to all 0s
    constant ALU_NEG     : ALU_COMNEG := "11"; -- set to all 1s

    -- SReg
    constant HALFCARRYBIT : natural := 3; -- half carry is carry out of bit 3
    constant NIBBLE       : integer := 4; -- nibble bit size

	-- ALUSel signals
    subtype ALU_SELECTS is std_logic_vector(2 downto 0);
	constant ADDSUBOUT      : ALU_SELECTS := "000";
    constant FBLOCKOUT      : ALU_SELECTS := "001";
    constant SHIFTOUT       : ALU_SELECTS := "010";
    constant SWAPOUT        : ALU_SELECTS := "011";
    constant MULOUT         : ALU_SELECTS := "100"; 
    constant BOUT           : ALU_SELECTS := "101"; 

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

----------------------------------------------------------------------------
--
--  n Bit Full Adder
--
--  Implementation of an n bit full adder. This entity takes generic n bit 
--  inputs A and B with a carry in input and outputs the sum and carry 
--  out n bits, using combinational logic. 
--
-- Inputs:
--      A: std_logic - n bit adder input
--      B: std_logic - n bit adder input
--      Cin: std_logic - 1 bit carry in input
--
-- Outputs:
--      Sum: std_logic - n bit sum of A, B, and Cin
--      Cout: std_logic - 1 bit carry out value 
--
--  Revision History:
--      02/06/19  Sundar Pandian    Initial revision.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Adder is generic ( bitsize : integer := 8 );
    port(
        A, B        :  in      std_logic_vector((bitsize-1) downto 0);  -- addends
        Cin         :  in      std_logic;  -- carry in value 
        Cout        :  out     std_logic;  -- carry out value 
        Sum         :  out     std_logic_vector((bitsize-1) downto 0)   -- sum of A, B with carry in
      );
end Adder;

architecture nAdder of Adder is
    component fullAdder is
        port(
            Ai          :  in      std_logic;  -- adder input 
            Bi          :  in      std_logic;  -- adder input 
            Cini        :  in      std_logic;  -- carry in value 
            Couti       :  out     std_logic;  -- carry out value 
            Sumi        :  out     std_logic   -- sum of A, B with carry in
          );
    end component;

    signal carry : std_logic_vector(bitsize downto 0); -- intermediate carries

    begin
        carry(0) <= Cin;        -- put carry in into our carry vector
        
        Adders: for i in bitsize generate -- generate bitsize full adders
        begin
            FAx: fullAdder port map (A(i), B(i), carry(i), Sum(i), carry(i + 1));
        end generate;

        Cout <= carry(bitsize); -- carry out is from carry vector
end nAdder;


