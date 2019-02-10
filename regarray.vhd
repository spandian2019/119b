----------------------------------------------------------------------------
--
--
-- Registers
--
-- General purpose registers for the AVR CPU. There are 32 8-bit registers,
-- R0 to R31. Registers 26 and 27 form the 16-bit X register, 28 and 29
-- form the Y register, and 30 and 31 form the Z register. Registers
-- 24 and 25 may be used as a 16-bit value for some operations, and registers
-- 0 and 1 may be used for 16-bit results of some operations.
-- The register array consists of a 5:32 decoder, 32 DFFS, and a selecting
-- interface. It takes as an input the system clock, input data, and enable
-- and select control signals. It outputs 8 bit registers A and B to the ALU.
--
-- Ports:
--  Inputs:
--        RegIn    - 8 bit input register bus
--        Clk      - system clock
--        RegWEn   - register write enable
--        RegWSel  - 5 bit register write select
--        RegSelA  - 5 bit register A select
--        RegSelB  - 5 bit register B select
--        LoadIn   - 2 bit select line for pipelining A and B output
--
--  Outputs:
--        RegAOut  - 8 bit register bus A output
--        RegBOut  - 8 bit register bus B output
--
--
-- Revision History:
-- 01/24/2019   Sophia Liu      Initial revision
-- 01/30/2019   Sundar Pandian  Initial architecture writeup
-- 02/01/2019   Sundar Pandian  Debugged with testbench
-- 02/07/2019   Sundar Pandian  Added indirect addressing mux and support
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

use work.opcodes.all;
use work.constants.all;

entity RegArray is
    port(
        Clk      :  in  std_logic;                                  -- system clock
        RegIn    :  in  std_logic_vector(REGSIZE-1 downto 0);       -- input register bus

        -- from CU
        RegWEn      : in std_logic;                                 -- register write enable, from CU
        RegWSel     : in std_logic_vector(RADDRSIZE-1 downto 0);    -- register write select, from CU
        RegSelA     : in std_logic_vector(RADDRSIZE-1 downto 0);    -- register A select, from CU
        RegSelB     : in std_logic_vector(RADDRSIZE-1 downto 0);    -- register B select, from CU

        IndDataIn   : in std_logic_vector(ADDRSIZE-1 downto 0);     -- Indirect Addr data in, from DataMIU
        IndAddrIn   : in std_logic_vector(RADDRSIZE-1 downto 0);    -- Indirect Addr value in, from DataMIU
        IndWEn      : in std_logic;                                 -- Indirect Addr write enable, from CU

        RegAOut     : out std_logic_vector(REGSIZE-1 downto 0);     -- register bus A out
        RegBOut     : out std_logic_vector(REGSIZE-1 downto 0);    	-- register bus B out

        RegXOut 	: out std_logic_vector(ADDRSIZE-1 downto 0);	-- register bus X out
        RegYOut 	: out std_logic_vector(ADDRSIZE-1 downto 0);	-- register bus Y out
        RegZOut 	: out std_logic_vector(ADDRSIZE-1 downto 0) 	-- register bus Z out
    );
end RegArray;

architecture regspace of RegArray is

    type reg_array is array (REG_LENGTH-1 downto 0) of std_logic_vector(REGSIZE-1 downto 0);
    signal registers : reg_array;

begin

    -- writing to regular registers and X, Y, Z Registers occurs synchronously
    -- use same process block to avoid driving same registers simultaneously
    write_addr_reg : process (CLK)
    begin
        if (rising_edge(CLK)) then
            -- writes to register only if write enabled
            -- holds value otherwise

            if RegWEn = WRITE_EN then
                registers(conv_integer(RegWSel)) <= RegIn;
            else
                if RegWSel(RADDRSIZE-1 downto 1) /= IndAddrIn(RADDRSIZE-1 downto 1) then
                    registers(conv_integer(RegWSel)) <= registers(conv_integer(RegWSel));
                end if;
            end if;
            -- writes to register only if write enabled and indirect address is even
            --  possible values for indirect address are X, Y, Z, and SP base addresses.
            --  Only SP base address is odd.
            -- holds value otherwise
            if IndWEn = WRITE_EN and IndAddrIn(0) = '0' then
                registers(conv_integer(IndAddrIn))
                    <= IndDataIn((ADDRSIZE/2)-1 downto 0);
                registers(conv_integer(IndAddrIn(RADDRSIZE-1 downto 1) & '1'))
                    <= IndDataIn(ADDRSIZE-1 downto ADDRSIZE/2);
            else
                registers(conv_integer(IndAddrIn))
                    <= registers(conv_integer(IndAddrIn));
                registers(conv_integer(IndAddrIn(RADDRSIZE-1 downto 1) & '1'))
                    <= registers(conv_integer(IndAddrIn(RADDRSIZE-1 downto 1) & '1'));
            end if;
        end if;
    end process write_addr_reg;

    -- register outputs load value in address line
    RegAOut <=  registers(conv_integer(RegSelA));
    RegBOut <=  registers(conv_integer(RegSelB));

    read_addr_reg : process (CLK)
    begin
        if (rising_edge(CLK)) then
        -- X,Y,Z pointers also always gets outputted to addr line MUX and control unit
        --  selects which address line to be used
        RegXOut((ADDRSIZE/2)-1 downto 0)      <= registers(conv_integer(X_ADDR_L));
        RegXOut(ADDRSIZE-1 downto ADDRSIZE/2) <= registers(conv_integer(X_ADDR_H));
        RegYOut((ADDRSIZE/2)-1 downto 0)      <= registers(conv_integer(Y_ADDR_L));
        RegYOut(ADDRSIZE-1 downto ADDRSIZE/2) <= registers(conv_integer(Y_ADDR_H));
        RegZOut((ADDRSIZE/2)-1 downto 0)      <= registers(conv_integer(Z_ADDR_L));
        RegZOut(ADDRSIZE-1 downto ADDRSIZE/2) <= registers(conv_integer(Z_ADDR_H));
        end if;
    end process read_addr_reg;

end regspace;