----------------------------------------------------------------------------
--
--
-- I/O Registers
--
-- I/O registers for the AVR CPU. There are 64 I/O ports that can be
-- manipulated, located at addreses 32 to 95. The status register and stack
-- pointer are included in the I/O registers, located at 95 and 94:93.
-- The inputs include the input data register, address values,
-- and status register inputs. The outputs are the 8-bit data output and 16-bit
-- stack pointer.
--
--
-- Ports:
--  Inputs:
--    Clk           - system clock
--    Reset         - active low system reset, used to init SP to all 1s
--    RegIn         - 8 bit input register
--    IORegWEn      - 1 bit IO register write enable, from CU
--    IORegWSel     - 6 bit IO register address select line, from CU
--    IndDataIn     - 16 bit indirect Addr data in, from DataMIU
--    IndAddrIn 	- 6 bit indirect Addr value in, from RegUnit
--    IndWEn 		- 1 bit indirect Addr write enable, from CU
--
--  Outputs:
--    IORegOut    - 8 bit IO register bus out
--    SPRegOut    - 16 bit SP register bus out
--
-- Revision History:
-- 01/24/2019   Sophia Liu      Initial revision
-- 01/30/2019   Sundar Pandian  Initial architecture writeup, remapping port
-- 02/01/2019   Sundar Pandian  Debugged with testbench
-- 02/01/2019   Sundar Pandian  added indirect addressing modes
-- 02/27/2019   Sophia Liu      Added header documentation
-- 02/27/2019   Sundar Pandian  removed unnecessary second delay on reg outputs
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

use work.opcodes.all;
use work.constants.all;

entity IORegArray is
    port(
        Clk      :  in  std_logic;                                  -- system clock
        Reset    :  in  std_logic;                                  -- system reset, used to init SP to all 1s
        RegIn    :  in  std_logic_vector(REGSIZE-1 downto 0);       -- input register

        -- from CU
        IORegWEn    : in std_logic;                                 -- IO register write enable, from CU
        IORegWSel   : in std_logic_vector(IOADDRSIZE-1 downto 0);   -- IO register address select line, from CU

        IndDataIn   : in std_logic_vector(ADDRSIZE-1 downto 0);     -- Indirect Addr data in, from DataMIU
        IndAddrIn 	: in std_logic_vector(IOADDRSIZE-1 downto 0);   -- Indirect Addr value in, from RegUnit
        IndWEn 		: in std_logic;                                 -- Indirect Addr write enable, from CU

        SRegIn      : in std_logic_vector(REGSIZE-1 downto 0);      -- Status Register in from ALU
        SRegOut     : out std_logic_vector(REGSIZE-1 downto 0);     -- Status Register out to ALU

        IORegOut    :  out std_logic_vector(REGSIZE-1 downto 0);    -- IO register bus out
        SPRegOut    :  out std_logic_vector(ADDRSIZE-1 downto 0)    -- SP register bus out
    );
end IORegArray;

architecture regspace of IORegArray is

    type IO_reg_array is array (IO_REG_LENGTH-1 downto 0) of std_logic_vector(REGSIZE-1 downto 0);
    signal IOregisters : IO_reg_array;

begin


    -- writing to SP Register occurs synchronously
    write_addr_reg : process (CLK)
    begin
        if Reset = '0' then
            IOregisters(conv_integer(SP_ADDR_H)) <= (others => '1');
            IOregisters(conv_integer(SP_ADDR_L)) <= (others => '1');
        elsif (rising_edge(CLK)) then
            -- writes to register only if write enabled
            -- holds value otherwise
            if IORegWEn = WRITE_EN then
                IOregisters(conv_integer(IORegWSel)) <= RegIn;

                if IORegWSel /= SREG_ADDR then
                    IOregisters(conv_integer(SREG_ADDR)) <= SRegIn;
                end if;

            else
                IOregisters(conv_integer(SREG_ADDR)) <= SRegIn;
                -- only hold value here if it will not be held by the second block
                if IORegWSel /= SP_ADDR_L or IORegWSel /= SP_ADDR_H then
                    IOregisters(conv_integer(IORegWSel)) <= IOregisters(conv_integer(IORegWSel));
                end if;
            end if;

            -- writes to register only if write enabled and indirect address is SP_ADDR_L
            -- holds value otherwise
            if IndWEn = WRITE_EN and IndAddrIn = SP_ADDR_L then
                IOregisters(conv_integer(SP_ADDR_H)) <= IndDataIn((ADDRSIZE/2)-1 downto 0);
                IOregisters(conv_integer(SP_ADDR_L)) <= IndDataIn(ADDRSIZE-1 downto ADDRSIZE/2);
            else
                if IndAddrIn /= "XXXXX" then
                    IOregisters(conv_integer(SP_ADDR_H)) <= IOregisters(conv_integer(SP_ADDR_H));
                    IOregisters(conv_integer(SP_ADDR_L)) <= IOregisters(conv_integer(SP_ADDR_L));
                end if;
            end if;
        end if;
    end process write_addr_reg;

    -- can always output IO reg to IO data bus since value needs to be selected
    --  by Control Unit to be used
    IORegOut <= IOregisters(conv_integer(IORegWSel));
    SRegOut  <= IOregisters(conv_integer(SREG_ADDR));
    SPRegOut((ADDRSIZE/2)-1 downto 0)      <= IOregisters(conv_integer(SP_ADDR_H));
    SPRegOut(ADDRSIZE-1 downto ADDRSIZE/2) <= IOregisters(conv_integer(SP_ADDR_L));


end regspace;
