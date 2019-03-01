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
--      02/06/19  Sundar Pandian    Added the Mux entities
--
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package constants is

    -----------------------
    -- GENERAL CONSTANTS --
    -----------------------
    constant NIBBLE     : integer := 4;     -- number of bits in nibble
    constant REGSIZE    : natural := 8;     -- number of bits in register
    constant ADDRSIZE   : natural := 16;    -- number of bits in memory
    constant IRSIZE     : natural := 16;    -- number of bits in IR
    constant RADDRSIZE  : natural := 5;     -- number of bits in register addr
    constant IOADDRSIZE : natural := 6;     -- number of bits in IO reg addr
    constant INTREGSIZE : natural := 4;     -- number of bits in Int Event reg
    constant REG_LENGTH     : natural := 32;--
    constant IO_REG_LENGTH  : natural := 64;--
    constant ZERO8      : std_logic_vector(7 downto 0) := "00000000"; -- 8 bit zero
    type byte is array (7 downto 0) of std_logic;
    type word is array (15 downto 0) of std_logic;

    ---------------------------
    -- ControlUnit CONSTANTS --
    ---------------------------
    -- Cycle Num Constants
    subtype OP_CYCLE is std_logic_vector(1 downto 0);
    constant ZERO_CYCLES    : OP_CYCLE := "00";
    constant ONE_CYCLE      : OP_CYCLE := "01";
    constant TWO_CYCLES     : OP_CYCLE := "10";
    constant THREE_CYCLES   : OP_CYCLE := "11";

    -- PC Load Constants
    -- uses and gate to add or load in value
    constant PC_ADD         : std_logic := '1';
    constant PC_LOAD        : std_logic := '0';

    -- Interrupt Event Handling Constants
    -- cannot currently handle more than 15 interrupts
    subtype INT_EVENT is std_logic_vector(INTREGSIZE-1 downto 0);
    constant NO_INT         : INT_EVENT := "1111";
    constant RESET_INT      : INT_EVENT := "0000";
    constant INT0_INT       : INT_EVENT := "0001";
    constant INT1_INT       : INT_EVENT := "0010";
    constant T1CAP_INT      : INT_EVENT := "0011";
    constant T1CPA_INT      : INT_EVENT := "0100";
    constant T1CPB_INT      : INT_EVENT := "0101";
    constant T1OVF_INT      : INT_EVENT := "0110";
    constant T0OVF_INT      : INT_EVENT := "0111";
    constant IRQSPI_INT     : INT_EVENT := "1000";
    constant UARTRX_INT     : INT_EVENT := "1001";
    constant UARTRE_INT     : INT_EVENT := "1010";
    constant UARTTX_INT     : INT_EVENT := "1011";
    constant ANACMP_INT     : INT_EVENT := "1100";

    constant INT_ZERO_PAD : std_logic_vector(ADDRSIZE-INTREGSIZE-1 downto 0) := "000000000000";

    constant ACTIVE_LO : std_logic := '0';
    constant ACTIVE_HI : std_logic := '1';

    -------------------
    -- ALU CONSTANTS --
    -------------------
    constant CPC_SET : std_logic := '1';
    constant CPC_RST : std_logic := '0';
    -- ALU output select signals
    subtype ALU_SELECTS is std_logic_vector(3 downto 0);
    constant ADDSUBOUT      : ALU_SELECTS := "0000";
    constant FBLOCKOUT      : ALU_SELECTS := "0001";
    constant SHIFTOUT       : ALU_SELECTS := "0010";
    constant SWAPOUT        : ALU_SELECTS := "0011";
    constant MULOUTH        : ALU_SELECTS := "0100";
    constant MULOUTL        : ALU_SELECTS := "1100";
    constant BOUT           : ALU_SELECTS := "0101";
    constant BSET           : ALU_SELECTS := "0110";
    constant BCLR           : ALU_SELECTS := "0111";

    -- F-block operands
    subtype  ALU_FOPS is std_logic_vector(3 downto 0);
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

    -- Shifter/Rotator control signals
    subtype  ALU_SR is std_logic_vector(1 downto 0); -- ops for s/r, add
    constant SR_LSR     : ALU_SR := "00"; -- Logical shift right
    constant SR_ASR     : ALU_SR := "01"; -- Arithmetic shift right
    constant SR_ROR     : ALU_SR := "10"; -- Rotate right -- is this right? carry?

    -- Adder/Subber control signals
    subtype  ALU_ADDSUB is std_logic_vector(2 downto 0);
    -- Subflag operand indices and values
    constant SUBFLAG    : integer := 0;     -- index of subtract flag
    constant OP_ADD     : std_logic := '0'; -- value for adding
    constant OP_SUB     : std_logic := '1'; -- value for subbing
    -- carry operand indices
    constant CARRY_S0   : integer := 1;     -- index of lower bit of carry CTRL
    constant CARRY_S1   : integer := 2;     -- index of higher bit of carry CTRL
    -- subtract flag values
    constant ALU_ADD    : ALU_ADDSUB := "000"; -- add
    constant ALU_SUB    : ALU_ADDSUB := "001"; -- sub
    -- carry select values
    constant RST_CARRY  : std_logic_vector(1 downto 0) := "00"; -- carry in = '0'
    constant SET_CARRY  : std_logic_vector(1 downto 0) := "01"; -- carry in = '1'
    constant CARRY_IN   : std_logic_vector(1 downto 0) := "10"; -- carry in = Cin
    constant NCARRY_IN  : std_logic_vector(1 downto 0) := "11"; -- carry in = nCin

    -- COM and NEG control signals for AND and OR gates
    subtype  ALU_COMNEG is std_logic_vector(1 downto 0);
    constant OP_CN_OR   : integer := 1;
    -- ALU_COMNEG(1) = control signal into OR gate
    --                    set to 0 for pass thru
    --                    set to 1 for setting high
    constant OP_CN_AND  : integer := 0;
    -- ALU_COMNEG(0) = control signal into AND gate
    --                    set to 1 for pass thru
    --                    set to 0 for setting low
    constant COMNEG_NONE : ALU_COMNEG := "01"; -- pass thru
    constant ALU_COM     : ALU_COMNEG := "11"; -- set to all 0s
    constant ALU_NEG     : ALU_COMNEG := "00"; -- set to all 1s

    -----------------------
    -- DataMIU CONSTANTS --
    -----------------------
    -- pre and post inc/dec on the indirect addressing
    subtype PREPOST_ADDR is std_logic;
    constant PRE_ADDR   : PREPOST_ADDR := '1';
    constant POST_ADDR  : PREPOST_ADDR := '0';

    --subtype DBMUX is std_logic;
    constant MEM_ADDR : std_logic := '1';
    constant IND_ADDR : std_logic := '0';

    subtype  OFFSET_SEL is std_logic_vector(1 downto 0);
    -- Data Address Adder - Offset Mux In Control Signals
    constant ZERO_SEL       : OFFSET_SEL := "00"; -- Selects  0 to add
    constant INC_SEL        : OFFSET_SEL := "01"; -- Selects +1 to add
    constant DEC_SEL        : OFFSET_SEL := "10"; -- Selects -1 to add
    constant OFFS_SEL       : OFFSET_SEL := "11"; -- Selects q offset to add

    subtype OFFSET_CONST is std_logic_vector(ADDRSIZE-1 downto 0);
    -- number of bits in OFFSET_CONST needs to match ADDRSIZE-1 for a 16 bit address bus
    -- Data Address Adder - Offset constant values to add
    constant ZERO_OFFSET    : OFFSET_CONST := "0000000000000000"; --  0 constant to add
    constant INC_OFFSET     : OFFSET_CONST := "0000000000000001"; -- +1 constant to add
    constant DEC_OFFSET     : OFFSET_CONST := "1111111111111111"; -- -1 constant to add
                                                                      -- xFF is 2s complement of 1, so adding
                                                                  -- xFF is the same as subtracting -1

    -- ASKFAB
    constant Q_OFFSET_SIZE   : integer := 6;
    constant Q_OFFS_ZERO_PAD : std_logic_vector(ADDRSIZE-Q_OFFSET_SIZE-1 downto 0) := "0000000000";

    -----------------------
    -- ProgMIU CONSTANTS --
    -----------------------
    subtype  SOURCE_SEL is std_logic_vector(2 downto 0);
    -- Prog Address Adder - Source Mux In Control Signals
    constant IR_SRC     : SOURCE_SEL := "000"; -- Selects IR value to load or add
    constant Z_SRC      : SOURCE_SEL := "001"; -- Selects Z register value to add
    constant RST_SRC    : SOURCE_SEL := "010"; -- Selects RST_VECTOR to add, or RST_VECTOR to load
    constant NORMAL_SRC : SOURCE_SEL := "011"; -- Selects INC_VECTOR to add
    constant DB_LO_SRC  : SOURCE_SEL := "100"; -- Selects DataDB to load into low byte
    constant DB_HI_SRC  : SOURCE_SEL := "101"; -- Selects DataDB to load into high byte

    subtype SOURCE_CONST is std_logic_vector(ADDRSIZE-1 downto 0);
    -- number of bits in SOURCE_CONST needs to match ADDRSIZE-1 for a 16 bit address bus
    -- Prog Address Adder - Source constant values to add
    constant RST_VECTOR     : SOURCE_CONST := "0000000000000000"; --  0 constant to load or add
                                                                  --  loading points to reset vector
                                                                  --  adding does not change PC
    constant INC_VECTOR     : SOURCE_CONST := "0000000000000001"; -- +1 constant to add

    constant PC_INIT        : std_logic_vector(15 downto 0) := "1111111111111111"; -- initial PC value

    ------------------------
    -- Register CONSTANTS --
    ------------------------
    -- miscellaneous constants
    constant IMM_EN         : std_logic := '1';
    constant IMM_DIS        : std_logic := '0';
    constant WRITE_EN       : std_logic := '1';
    constant WRITE_DIS      : std_logic := '0';
    constant REG_A_OUT      : std_logic := '0';
    constant IO_OUTPUT      : std_logic := '1';
    constant SREG_ADDR      : std_logic_vector(IOADDRSIZE-1 downto 0) := "111111";  -- 32 offset
    constant SP_ADDR_L      : std_logic_vector(IOADDRSIZE-1 downto 0) := "111101";  -- base address for stack pointer
                                                                                    -- low byte located at addr 61 in IO
    constant SP_ADDR_H      : std_logic_vector(IOADDRSIZE-1 downto 0) := "111110";  -- address for stack pointer high byte
                                                                                    -- high byte located at addr 62 in IO
    constant X_ADDR_L       : std_logic_vector(RADDRSIZE-1 downto 0) := "11010";    -- base address for X pointer
                                                                                    -- low byte located at addr 26 in reg
    constant X_ADDR_H       : std_logic_vector(RADDRSIZE-1 downto 0) := "11011";    -- address for X pointer high byte
                                                                                    -- high byte located at addr 27 in reg
    constant Y_ADDR_L       : std_logic_vector(RADDRSIZE-1 downto 0) := "11100";    -- base address for Y pointer
                                                                                    -- low byte located at addr 28 in reg
    constant Y_ADDR_H       : std_logic_vector(RADDRSIZE-1 downto 0) := "11101";    -- address for Y pointer high byte
                                                                                    -- high byte located at addr 29 in reg
    constant Z_ADDR_L       : std_logic_vector(RADDRSIZE-1 downto 0) := "11110";    -- base address for Z pointer
                                                                                    -- low byte located at addr 30 in reg
    constant Z_ADDR_H       : std_logic_vector(RADDRSIZE-1 downto 0) := "11111";    -- address for Z pointer high byte
                                                                                    -- high byte located at addr 31 in reg

    -- Address Adder - Address Mux In Control Signals
    subtype  ADDR_SEL is std_logic_vector(1 downto 0);
    constant X_SEL          : ADDR_SEL := "11"; -- Selects X Indirect Register
    constant Y_SEL          : ADDR_SEL := "10"; -- Selects Z Indirect Register
    constant Z_SEL          : ADDR_SEL := "00"; -- Selects Y Indirect Register
    constant SP_SEL         : ADDR_SEL := "01"; -- Selects Stack Pointer to inc/dec

    -- Address Adder - Indirect Register Address constant values
    subtype ADDR_CONST is integer;
    constant X_CONST    : ADDR_CONST := 26; -- location of X indirect addressing mode register
    constant Y_CONST    : ADDR_CONST := 28; -- location of Y indirect addressing mode register
    constant Z_CONST    : ADDR_CONST := 30; -- location of Z indirect addressing mode register
    constant SP_CONST   : ADDR_CONST := 24; -- location of SP

    -- Register Data In Select constants
    subtype  RegData_selects is std_logic_vector(3 downto 0);

    subtype  LOADIN_SEL is std_logic_vector(2 downto 0);
    -- LoadIn select constants
    constant LD_IMM    : LOADIN_SEL := "000";
    constant LD_ALU    : LOADIN_SEL := "001";
    constant LD_DB     : LOADIN_SEL := "010";
    constant LD_REGA   : LOADIN_SEL := "011";
    constant LD_PROG_HI: LOADIN_SEL := "100";
    constant LD_PROG_LO: LOADIN_SEL := "101";

    --subtype  LoadReg_selects is std_logic_vector(1 downto 0);
    ---- LoadReg select constants
    --constant LoadNone   : LoadReg_selects := "00";
    --constant LoadA      : LoadReg_selects := "01";
    --constant LoadB      : LoadReg_selects := "10";
    --constant LoadSwap   : LoadReg_selects := "11";

    subtype  SRegLd_selects is std_logic;
    -- LoadReg select constants
    constant LdSRCtrlU    : SRegLd_selects := '1';
    constant LdSRALU      : SRegLd_selects := '0';


    ------------------------------
    -- Status Register Bitmasks --
    ------------------------------
    -- Sreg: I T H S V N Z C
    subtype BIT_MASK is std_logic_vector(7 downto 0);
    constant MASK_ADD   : BIT_MASK := "00111111"; -- add sub(except adiw, sbiw), including neg
    constant MASK_CP    : BIT_MASK := "00111111"; -- compares
    constant MASK_ADIW  : BIT_MASK := "00011111"; -- adiw, sbiw
    constant MASK_DECINC: BIT_MASK := "00011110"; -- dec, inc

    constant MASK_ANDOR : BIT_MASK := "00011110"; -- and, or
    constant MASK_COM   : BIT_MASK := "00011111"; -- com
    constant MASK_NEG   : BIT_MASK := "00111111"; -- neg
    constant MASK_EOR   : BIT_MASK := "00011110"; -- eor

    constant MASK_SHIFT : BIT_MASK := "00011111"; -- asr, lsr, ror

    constant MASK_BLD   : BIT_MASK := "00000000"; -- bld
    constant MASK_BST   : BIT_MASK := "01000000"; -- bst

    constant MASK_MUL   : BIT_MASK := "00000001"; -- mul
    constant MASK_NONE  : BIT_MASK := "00000000"; -- change nothing

    constant MASK_INT   : BIT_MASK := "10000000"; -- change interrupt flag

    constant T_SREG : natural := 6; -- transfer bit index in sreg
    constant I_SREG : natural := 7; -- global interrupt enable index in sreg
    constant T_IR : natural := 9; -- transfer bit number in IR

    constant INT_EN  : std_logic := '1'; -- interrupts enabled
    constant INT_DIS : std_logic := '0'; -- interrupts disabled


end package constants;

----------------------------------------------------------------------------
--
--  8:1 mux
--
--  Implementation of a 8:1 mux. Includes 3 control bits, 8 input signals,
--  and a selected output.
--
-- Inputs:
--      S0 - mux select bit 0
--      S1 - mux select bit 1
--      S2 - mux select bit 2
--      SIn0 - mux input 0
--      SIn1 - mux input 1
--      SIn2 - mux input 2
--      SIn3 - mux input 3
--      SIn4 - mux input 4
--      SIn5 - mux input 5
--      SIn6 - mux input 6
--      SIn7 - mux input 7
--
-- Outputs:
--      SOut - mux output
--
--  Revision History:
--      01/31/18  Sophia Liu        Initial revision.
--      02/01/18  Sophia Liu        Updated comments.
--      02/06/19  Sundar Pandian    Refitted for 8:1 Mux
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Mux8to1 is
    port(
        S0          :  in      std_logic;  -- mux sel(0)
        S1          :  in      std_logic;  -- mux sel(1)
        S2          :  in      std_logic;  -- mux sel(2)
        SIn0        :  in      std_logic;  -- mux inputs
        SIn1        :  in      std_logic;  -- mux inputs
        SIn2        :  in      std_logic;  -- mux inputs
        SIn3        :  in      std_logic;  -- mux inputs
        SIn4        :  in      std_logic;  -- mux inputs
        SIn5        :  in      std_logic;  -- mux inputs
        SIn6        :  in      std_logic;  -- mux inputs
        SIn7        :  in      std_logic;  -- mux inputs
        SOut        :  out     std_logic   -- mux output
      );
end Mux8to1;

architecture Mux8to1 of Mux8to1 is
    signal sel      : std_logic_vector(2 downto 0) := S2 & S1 & S0;
    begin
    process(SIn0, SIn1, SIn2, SIn3, SIn4, SIn5, SIn6, SIn7, sel)
    begin  -- choose Sout based on S0 & S1 & S2
        if    sel = "000" then
            SOut <= SIn0;
        elsif sel = "001" then
            SOut <= SIn1;
        elsif sel = "010" then
            SOut <= SIn2;
        elsif sel = "011" then
            SOut <= SIn3;
        elsif sel = "100" then
            SOut <= SIn4;
        elsif sel = "101" then
            SOut <= SIn5;
        elsif sel = "110" then
            SOut <= SIn6;
        elsif sel = "111" then
            SOut <= SIn7;
        else
            SOut <= 'X'; -- for simulation
        end if;
    end process;
end Mux8to1;

----------------------------------------------------------------------------
--
--  4:1 mux
--
--  Implementation of a 4:1 mux. Includes 2 control bits, 4 input signals,
--  and a selected output.
--
-- Inputs:
--      S0 - mux select bit 0
--      S1 - mux select bit 1
--      SIn0 - mux input 0
--      SIn1 - mux input 1
--      SIn2 - mux input 2
--      SIn3 - mux input 3
--
-- Outputs:
--      SOut - mux output
--
--  Revision History:
--      01/31/18  Sophia Liu    Initial revision.
--      02/01/18  Sophia Liu    Updated comments.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Mux4to1 is
    port(
        S0          :  in      std_logic;  -- mux sel (0)
        S1          :  in      std_logic;  -- mux sel(1)
        SIn0         :  in      std_logic;  -- mux inputs
        SIn1         :  in      std_logic;  -- mux inputs
        SIn2         :  in      std_logic;  -- mux inputs
        SIn3         :  in      std_logic;  -- mux inputs
        SOut        :  out     std_logic   -- mux output
      );
end Mux4to1;

architecture Mux4to1 of Mux4to1 is
    begin
    process(SIn0, SIn1, SIn2, SIn3, S0, S1)
    begin  -- choose Sout based on S0 & S1
        if S0 = '0' and S1 = '0' then
            SOut <= SIn0;
        elsif S0 = '1' and S1 = '0' then
            SOut <= SIn1;
        elsif S0 = '0' and S1 = '1' then
            SOut <= SIn2;
        elsif S0 = '1' and S1 = '1' then
            SOut <= SIn3;
        else
            SOut <= 'X'; -- for simulation
        end if;
    end process;
end Mux4to1;

----------------------------------------------------------------------------
--
--  2:1 mux
--
--  Implementation of a 2:1 mux. Includes 1 control bit, 2 input signals,
--  and a selected output.
--
-- Inputs:
--      S0 - mux select bit
--      SIn0 - mux input 0
--      SIn1 - mux input 1
--
-- Outputs:
--      SOut - mux output
--
--  Revision History:
--      01/31/18  Sophia Liu        Initial revision.
--      02/01/18  Sophia Liu        Updated comments.
--      02/06/19  Sundar Pandian    Refitted for 2:1 Mux
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Mux2to1 is
    port(
        S0          :  in      std_logic;  -- mux sel
        SIn0        :  in      std_logic;  -- mux inputs
        SIn1        :  in      std_logic;  -- mux inputs
        SOut        :  out     std_logic   -- mux output
      );
end Mux2to1;

architecture Mux2to1 of Mux2to1 is
    begin
    process(SIn0, SIn1, S0)
    begin  -- choose Sout based on S0
        if S0 = '0' then
            SOut <= SIn0;
        elsif S0 = '1' then
            SOut <= SIn1;
        else
            SOut <= 'X'; -- for simulation
        end if;
    end process;
end Mux2to1;
