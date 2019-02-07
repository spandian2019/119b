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
    process(SIn0, SIn1, SIn2, SIn3, SIn4, SIn5, SIn6, SIn7, S0, S1, S2)
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