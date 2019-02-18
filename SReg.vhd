----------------------------------------------------------------------------
--
--
-- Status Register
--
--
--
-- Ports:
--  Inputs:
--        SRegIn    - 8-bit input from the registers
--        Clk      - system clock
--
--  Outputs:
--        SRegOut  - 8-bit output register bus
--
-- Revision History:
-- 02/07/2019   Sophia Liu      Initial revision
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

use work.opcodes.all;

use work.constants.all;

entity SReg is
    port(
        SRegIn      : in    std_logic_vector(7 downto 0);   -- input register bus
        --SRegInEn    : in    std_logic_vector(7 downto 0)    -- SReg load signal
        --StatusIn    : in    std_logic_vector(7 downto 0);   -- input status flags
        Clk         : in    std_logic;                      -- system clock
        SRegOut     : out   std_logic_vector(7 downto 0)    -- status register bus
    );

end SReg;

architecture SReg_arc of SReg is
    signal SRegisters : std_logic_vector(7 downto 0);
begin
    -- writing to registers occurs synchronously
    write_reg : process (CLK)
    begin
        if (rising_edge(CLK)) then
            -- writes to register only if write enabled
            -- holds value otherwise
            --if RegInEn = '1' then
                SRegisters <= SRegIn; --???
            --else
                --SRegisters <= SRegisters;
            --end if;
        end if;
    end process write_reg;

    -- output SReg
    SRegOut <= SRegisters;

end SReg_arc;
