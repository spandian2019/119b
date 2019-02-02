----------------------------------------------------------------------------
-- 
-- 
-- I/O Registers 
--
-- I/O registers for the AVR CPU. There are 64 I/O ports that can be 
-- manipulated, located at addreses 32 to 95. The status registers 
-- are included in the I/O registers.
-- The inputs include the input data register, address values, 
-- and status register inputs. The output is the 8-bit data output.
-- 
--
-- Ports:
--  Inputs:
--        RegIn    - 8-bit input from the registers
--        StatusIn - 8-bit status flag input from ALU
--        Clk      - system clock
--        K        - 8-bit immediate value to use
--        SregInOut  - in/out control 
--        IOEn    - io register enable
--
--  Outputs:
--        RegOut  - 8-bit output register bus    
--
-- Revision History:
-- 01/24/2019   Sophia Liu      Initial revision
-- 01/30/2019   Sundar Pandian  Initial architecture writeup, remapping port
-- 02/01/2019   Sundar Pandian  Debugged with testbench
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

use work.opcodes.all; 

use work.constants.all;

entity IOReg is
    port(
        RegIn       : in    std_logic_vector(7 downto 0);   -- input register bus
        RegInSel    : in    std_logic_vector(5 downto 0);   -- IO register address bus
        StatusIn    : in    std_logic_vector(7 downto 0);   -- input status flags
        Clk         : in    std_logic;                      -- system clock
        RegInEn     : in    std_logic;                      -- input enable
        RegOutEn    : in    std_logic;                      -- output enable
        bitmask     : in    std_logic_vector(7 downto 0);   -- mask for setting values in SReg
        RegOut      : out   std_logic_vector(7 downto 0);   -- output register bus
        SRegOut     : out   std_logic_vector(7 downto 0)    -- status register bus
    );

end IOReg;

architecture IOReg_arc of IOReg is
    type IO_reg_array is array (63 downto 0) of std_logic_vector(7 downto 0);
    signal registers : IO_reg_array;
begin
    -- writing to registers occurs synchronously
    write_reg : process (CLK)
    begin
        if (rising_edge(CLK)) then
            -- writes to register only if write enabled
            if RegInEn = '1' then
                registers(conv_integer(RegInSel)) <= RegIn;
            end if;
        end if;
    end process write_reg;

    -- can always output IO reg to IO data bus since value needs to be selected
    --  by Control Unit to be used
    RegOut  <=  registers(conv_integer(RegInSel));
    -- SReg always outputted
    SRegOut <=  registers(conv_integer(SReg_addr));

    -- if bit set in bitmask, can assign corresponding bit in SReg
    SREG_write : process (CLK)
    begin
        if (rising_edge(CLK)) then
            BIT_OP : for i in 7 downto 0 loop
                if bitmask(i) = '1' then
                    registers(conv_integer(SReg_addr))(i) <= StatusIn(i);
                end if;
            end loop;
        end if;
    end process SREG_write;

end IOReg_arc;






