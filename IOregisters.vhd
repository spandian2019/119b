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
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

library opcodes; 
use opcodes.opcodes.all; 

library CPUconst;
use CPUconst.constants.all;

entity IOReg is
    port(
        RegIn       : in    std_logic_vector(7 downto 0);   -- input register bus
        RegInSel    : in    std_logic_vector(5 downto 0);   -- IO register address bus
        StatusIn    : in    std_logic_vector(7 downto 0);   -- input status flags
        Clk         : in    std_logic;                      -- system clock
        RegInEn     : in    std_logic;                      -- 
        RegOutEn    : in    std_logic;                      --
        bitmask     : in    std_logic_vector(7 downto 0);   --
        RegOut      : out   std_logic_vector(7 downto 0);   -- output register bus
        SRegOut     : out   std_logic_vector(7 downto 0)    --
    );

end IOReg;

architecture Reg_arc of Reg is

    signal registers : IO_reg_array;

begin

    -- maybe use for loop to make m:2^m decoder
    -- maybe using ORs and ANDs to set/reset SReg bits smaller?

    -- writing to registers occurs synchronously
    write_reg : process (CLK)
    begin
        if (rising_edge(CLK)) then
            -- writes to register if write enabled
            if RegInEn = '1' then
                registers(conv_integer(RegInSel)) <= RegIn;
            end if;
        end if;
    end process write_reg;

    -- can always output IO reg to IO data bus since value needs to be selected
    --  by Control Unit to be used
    RegOut  <=  registers(conv_integer(RegInSel));
    SRegOut <=  registers(conv_integer(SReg_addr));

    SREG_write : process (CLK) 
    begin
        if (rising_edge(CLK)) then
            BIT_OP : for i in 0 to 7 generate
                if bitmask(i) = '1' then
                    registers(conv_integer(RegInSel))(i) <= SRegIn(i);
                end if;
            end generate;
        end if;
    end process SREG_write;

end Reg_arc;






