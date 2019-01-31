-----------------------------------------------------------------------------
--
--  Control Unit constants package
--
--  This package defines control unit constants for supported AVR
--  instructions
--
--  Revision History
--      1/30/19   Sundar Pandian    initial revision
--
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package constants is

    subtype  ALU_selects  is  std_logic_vector(2 downto 0);

--  ALU select opcode constants
<<<<<<< HEAD

    constant AddSubEn       : ALU_selects := "00";
    constant FBlockEn       : ALU_selects := "01";
    constant ShiftEn        : ALU_selects := "10";
=======
    
    constant AddSubEn       : ALU_selects := "000";
    constant FBlockEn       : ALU_selects := "001";
    constant ShiftEn        : ALU_selects := "010";
>>>>>>> 9d4f44e40c27cdee4688461e90e225b2f004d2f2


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
    constant OP_LSR     : ALU_OPS := "00-"; -- Logical shift right
    constant OP_ASR     : ALU_OPS := "01--"; -- Arithmetic shift right
    constant OP_ROR     : ALU_OPS := "100-"; -- Rotate right (no carry)
    constant OP_RORC     : ALU_OPS := "101-"; -- Rotate right (with carry)

    -- Adder/Subtractor operands
    constant OP_ADD     : ALU_OPS := "0---";
    constant OP_SUB     : ALU_OPS := "1---";
    constant OP_CARRY   : ALU_OPS := "-1--";
    constant OP_NOCARRY : ALU_OPS := "-0--";

end package;
