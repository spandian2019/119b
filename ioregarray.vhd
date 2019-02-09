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

entity IORegArray is
    port(
        Clk      :  in  std_logic;                          -- system clock
        Reset    :  in  std_logic;
        RegIn    :  in  std_logic_vector(REGSIZE-1 downto 0);       -- input register bus

        -- from CU
        IORegWEn    : in std_logic;                     -- register write enable
        IORegWSel   : in std_logic_vector(IOADDRSIZE-1 downto 0);   -- IO register address bus

        IndDataIn   : in std_logic_vector(ADDRSIZE-1 downto 0); -- 
        IndAddrIn 	: in std_logic_vector(IOADDRSIZE-1 downto 0); -- 
        IndWEn 		: in std_logic;

        IORegOut    :  out std_logic_vector(REGSIZE-1 downto 0);       -- register bus B out
        SPRegOut    :  out std_logic_vector(ADDRSIZE-1 downto 0)
    );

end IORegArray;

architecture regspace of IORegArray is

    type IO_reg_array is array (IO_REG_LENGTH-1 downto 0) of std_logic_vector(REGSIZE-1 downto 0);
    signal IOregisters : IO_reg_array;

begin


    -- writing to SP Register occurs synchronously
    write_addr_reg : process (CLK, Reset)
    begin
        if Reset = '1'' then
            IOregisters(conv_integer(SP_ADDR_H)) <= (others => '0');
            IOregisters(conv_integer(SP_ADDR_L)) <= (others => '0');
        elsif (rising_edge(CLK)) then
            -- writes to register only if write enabled
            -- holds value otherwise
            if IORegWEn = WRITE_EN then
                IOregisters(conv_integer(IORegWSel)) <= RegIn;
            else
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
                --if IORegWSel /= SP_ADDR_H then
                    IOregisters(conv_integer(SP_ADDR_H)) <= IOregisters(conv_integer(SP_ADDR_H));
                --end if;
                --if IORegWSel /= SP_ADDR_L then
                    IOregisters(conv_integer(SP_ADDR_L)) <= IOregisters(conv_integer(SP_ADDR_L));
                --end if;
            end if;
        end if;
    end process write_addr_reg;

    -- can always output IO reg to IO data bus since value needs to be selected
    --  by Control Unit to be used
    IORegOut <= IOregisters(conv_integer(IORegWSel));

    read_addr_reg : process (CLK)
    begin
        if (rising_edge(CLK)) then
        -- stack pointer also always gets outputted to addr line MUX and control unit
        --  selects which address line to be used
        SPRegOut((ADDRSIZE/2)-1 downto 0)      <= IOregisters(conv_integer(SP_ADDR_L));
        SPRegOut(ADDRSIZE-1 downto ADDRSIZE/2) <= IOregisters(conv_integer(SP_ADDR_H));
        end if;
    end process read_addr_reg;

end regspace;