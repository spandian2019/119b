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

entity Reg is
    port(
        RegIn    :  in  std_logic_vector(REGSIZE-1 downto 0);       -- input register bus
        Clk      :  in  std_logic;                          -- system clock

        -- from CU
        RegWEn      : in std_logic;                     -- register write enable
        RegWSel     : in std_logic_vector(RADDRSIZE-1 downto 0);  -- register write select
        RegSelA     : in std_logic_vector(RADDRSIZE-1 downto 0);  -- register A select
        RegSelB     : in std_logic_vector(RADDRSIZE-1 downto 0);  -- register B select

        IndRegWEn   : in std_logic;                             -- Update X, Y, Z, or SP
        AddrIn      : in std_logic_vector(ADDRSIZE-1 downto 0); -- 
        AddrSel     : in ADDR_SEL;

        RegAOut     :  out std_logic_vector(REGSIZE-1 downto 0);       -- register bus A out
        RegBOut     :  out std_logic_vector(REGSIZE-1 downto 0);       -- register bus B out
        AddrMuxOut  :  out std_logic_vector(ADDRSIZE-1 downto 0);

        LoadReg  : in std_logic_vector(1 downto 0)  -- for loading immediate values to out buses
    );

end Reg;

architecture Reg_arc of Reg is
    type reg_array is array (REG_LENGTH-1 downto 0) of std_logic_vector(REGSIZE-1 downto 0);
    type IO_reg_array is array (IO_REG_LENGTH-1 downto 0) of std_logic_vector(REGSIZE-1 downto 0);
    signal registers : reg_array;
    signal IOregisters : IO_reg_array;
    signal AddrMux   : integer;
begin

    -- writing to registers occurs synchronously
    write_reg : process (CLK)
    begin
        if (rising_edge(CLK)) then
            -- writes to register only if write enabled
            -- holds value otherwise
            if RegWEn = '1' then
                registers(conv_integer(RegWSel)) <= RegIn;
            else
                registers(conv_integer(RegWSel)) <= registers(conv_integer(RegWSel));
            end if;
        end if;
    end process write_reg;

    -- register outputs load value in address line
    RegAOut <=  registers(conv_integer(RegSelA));
    RegBOut <=  registers(conv_integer(RegSelB));

    -- Address Mux In
    AddrMuxIn:  for i in (2*REGSIZE)-1 downto 0 generate
      AddrMuxIni: Mux4to1
        port map(
            S0          => AddrSel(0),
            S1          => AddrSel(1),
            SIn0        => Z_CONST,
            SIn1        => SP_CONST,
            SIn2        => Y_CONST,
            SIn3        => X_CONST,
            SOut        => AddrMux
      );
      end generate AddrMuxIn;

    -- writing to X, Y, Z registers occurs synchronously
    write_indirect_reg : process (CLK)
    begin
        if (rising_edge(CLK)) then
            -- writes to register only if write enabled
            -- holds value otherwise
            if IndRegWEn = '1' then
                registers(AddrMux)   <= AddrIn((ADDRSIZE/2)-1 downto 0);
                registers(AddrMux+1) <= AddrIn(ADDRSIZE-1 downto ADDRSIZE/2);
            else
                registers(AddrMux)   <= registers(AddrMux);
                registers(AddrMux+1) <= registers(AddrMux+1);
            end if;
        end if;
    end process write_indirect_reg;

    AddrMuxOut((ADDRSIZE/2)-1 downto 0)      <= registers(AddrMux);
    AddrMuxOut(ADDRSIZE-1 downto ADDRSIZE/2) <= registers(AddrMux+1);

    -- split inputs among IO and reg
    -- mux outputs from IO and reg(A)
    -- implement Stack Pointer

    -- writing to registers occurs synchronously
    write_IO_reg : process (CLK)
    begin
        if (rising_edge(CLK)) then
            -- writes to register only if write enabled
            -- holds value otherwise
            if RegInEn = '1' then
                IOregisters(conv_integer(IORegInSel)) <= IORegIn;
            else
                IOregisters(conv_integer(IORegInSel)) <= IORegIn;
            end if;
        end if;
    end process write_IO_reg;

    -- can always output IO reg to IO data bus since value needs to be selected
    --  by Control Unit to be used
    IORegOut  <=  IOregisters(conv_integer(IORegInSel));

end Reg_arc;









