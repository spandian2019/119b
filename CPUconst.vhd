-----------------------------------------------------------------------------
--
--  Control Unit constants package
--
--  This package defines control unit constants for supported AVR
--  instructions
--
--  Revision History
--      1/30/19   Sundar Pandian    initial revision
--      02/01/19  Sundar Pandian    Debugged with testbench
--
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package constants is

    subtype  ALU_selects  is  std_logic_vector(2 downto 0);
    --  ALU select opcode constants
    constant AddSubEn       : ALU_selects := "000";
    constant FBlockEn       : ALU_selects := "001";
    constant ShiftEn        : ALU_selects := "010";
    constant PassThruEn     : ALU_selects := "011";
    constant MulEn          : ALU_selects := "100";

    -- Register Data In Select constants
    subtype  RegData_selects is std_logic_vector(3 downto 0);

    subtype  LoadIn_selects is std_logic_vector(1 downto 0);
    -- LoadIn select constants
    constant LdK       : LoadIn_selects := "00";
    constant LdALU     : LoadIn_selects := "01";
    constant LdIO      : LoadIn_selects := "10";
    constant LdRegA    : LoadIn_selects := "11";

    subtype  LoadReg_selects is std_logic_vector(1 downto 0);
    -- LoadReg select constants
    constant LoadNone   : LoadReg_selects := "00";
    constant LoadA      : LoadReg_selects := "01";
    constant LoadB      : LoadReg_selects := "10";
    constant LoadSwap   : LoadReg_selects := "11";

    subtype  SRegLd_selects is std_logic;
    -- LoadReg select constants
    constant LdSRCtrlU    : SRegLd_selects := '1';
    constant LdSRALU      : SRegLd_selects := '0';


    constant REGSIZE    : natural := 8;
    constant ZERO8      : std_logic_vector(7 downto 0) := "00000000";

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
    constant OP_LSR     : ALU_OPS := "--00"; -- Logical shift right
    constant OP_ASR     : ALU_OPS := "--01"; -- Arithmetic shift right
    constant OP_ROR     : ALU_OPS := "-010"; -- Rotate right (no carry)
    constant OP_RORC     : ALU_OPS := "-110"; -- Rotate right (with carry)


    -- Adder/Subber bit assignments
    constant subFlag    : integer := 3;
    constant carryBit   : integer := 2;

    type byte is array (7 downto 0) of std_logic;
    type word is array (15 downto 0) of std_logic;

    type reg_array is array (31 downto 0) of byte; -- difference between subtype and type?
    type IO_reg_array is array (63 downto 0) of byte; -- difference between subtype and type?

    constant SReg_addr : std_logic_vector(5 downto 0) := "111111";

	 -- flag masks
	 -- Sreg: I T H S V N Z C
	 subtype SREG_MASK is std_logic_vector(7 downto 0);
	 constant MASK_ADD : SREG_MASK:= "00111111"; -- add sub(except adiw, sbiw), including neg
	 constant MASK_CP : SREG_MASK:= "00111111"; -- compares
	 constant MASK_ADIW : SREG_MASK:= "00011111"; -- adiw, sbiw
	 constant MASK_DECINC : SREG_MASK:= "00011110"; -- dec, inc

	 constant MASK_ANDOR : SREG_MASK:= "00011110"; -- and, or
	 constant MASK_COM : SREG_MASK:= "00011111"; -- com
	 constant MASK_NEG : SREG_MASK:= "00111111"; -- neg
	 constant MASK_EOR : SREG_MASK:= "00011110"; -- eor

	 constant MASK_SHIFT : SREG_MASK:= "00011111"; -- asr, lsr, ror

	 constant MASK_BLD : SREG_MASK:= "00000000"; -- bld
	 constant MASK_BST : SREG_MASK:= "01000000"; -- bst
	 constant T_SREG : natural := 6; -- transfer bit number in sreg
	 constant T_IR : natural := 9; -- transfer bit number in IR

	 constant MASK_MUL : SREG_MASK:= "00000001"; -- mul
	 constant MASK_NONE : SREG_MASK:= "00000000"; -- change nothing

end package;