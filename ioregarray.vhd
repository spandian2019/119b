----------------------------------------------------------------------------
--
--
-- I/O Registers
--
-- I/O registers for the AVR CPU. There are 64 I/O ports that can be
-- manipulated, located at addreses 32 to 95. The status register and stack
-- pointer are included in the I/O registers, located at 95 and 94:93.
-- The inputs include the input data register, address values,
-- and status register inputs. The output is the 8-bit data output.
--
--
-- Ports:
--  Inputs:
--    Clk      - system clock
--    Reset    - system reset, used to init SP to all 1s
--    RegIn    - input register
--    IORegWEn    - IO register write enable, from CU
--    IORegWSel   - IO register address select line, from CU
--    IndDataIn   - Indirect Addr data in, from DataMIU
--    IndAddrIn 	- Indirect Addr value in, from RegUnit
--    IndWEn 		- Indirect Addr write enable, from CU
--
--  Outputs:
--    IORegOut    - IO register bus out
--    SPRegOut    - SP register bus out
--
-- Revision History:
-- 01/24/2019   Sophia Liu      Initial revision
-- 01/30/2019   Sundar Pandian  Initial architecture writeup, remapping port
-- 02/01/2019   Sundar Pandian  Debugged with testbench
-- 02/01/2019   Sundar Pandian  added indirect addressing modes
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

        IORegOut    :  out std_logic_vector(REGSIZE-1 downto 0);    -- IO register bus out
        SPRegOut    :  out std_logic_vector(ADDRSIZE-1 downto 0)    -- SP register bus out
    );
end IORegArray;

architecture regspace of IORegArray is

    type IO_reg_array is array (IO_REG_LENGTH-1 downto 0) of std_logic_vector(REGSIZE-1 downto 0);
    signal IOregisters : IO_reg_array;

begin


    -- writing to SP Register occurs synchronously
    write_addr_reg : process (CLK, Reset)
    begin
        if Reset = '0' then
            IOregisters(conv_integer(SP_ADDR_H)) <= (others => '1');
            IOregisters(conv_integer(SP_ADDR_L)) <= (others => '1');
        elsif (rising_edge(CLK)) then
            -- writes to register only if write enabled
            -- holds value otherwise
            if IORegWEn = WRITE_EN then
                IOregisters(conv_integer(IORegWSel)) <= RegIn;
            else
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

    -- synchronously output indirect address lines to avoid errors with DataMIU
    --  such as incrementing twice in one operation
    read_addr_reg : process (CLK)
    begin
        if (rising_edge(CLK)) then
        -- stack pointer also always gets outputted to addr line MUX and control unit
        --  selects which address line to be used
        SPRegOut((ADDRSIZE/2)-1 downto 0)      <= IOregisters(conv_integer(SP_ADDR_H));
        SPRegOut(ADDRSIZE-1 downto ADDRSIZE/2) <= IOregisters(conv_integer(SP_ADDR_L));
        end if;
    end process read_addr_reg;

end regspace;
