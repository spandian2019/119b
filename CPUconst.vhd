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
    
    constant AddSubEn       : ALU_selects := "00";
    constant FBlockEn       : ALU_selects := "01";
    constant ShiftEn        : ALU_selects := "10";


end package;