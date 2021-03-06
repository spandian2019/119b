----------------------------------------------------------------------------
--
--
-- Register Unit
--
-- Top-level entity for the 32 general registers and 64 IO registers,
-- including the stack pointer. Assigns control signals for the RegArray
-- and IORegArray entities.
-- It also selects the appropriate X, Y, Z, or SP input and output signals,
-- and selects RegAOut from either the general or io registers.
--
-- Ports:
--  Inputs:
--        RegIn    - 8 bit input register bus
--        Clk      - system clock
--        Reset    - active low reset
--        RegWEn   - 1 bit register write enable
--        RegWSel  - 5 bit register write select
--        RegSelA  - 5 bit register A select
--        RegSelB  - 5 bit register B select
--        IORegWEn    - 1 bit IO register write enable
--        IORegWSel   - 6 bit IO register address bus
--        IndDataIn   - 16 bit indirect Address Data In, from DataMIU
--        IndWEn      - 1 bit indirect Address write enable, from CU
--        IndAddrSel  - 2 bit indirect addr select, from CU
--        IOOutSel    - 1 bit MUX select line for outputting RegA or IO
--        SRegIn      - 8 bit status register input from ALU
--
--  Outputs:
--        SRegOut  - 8 bit status register to ALu
--        RegAOut  - 8 bit register bus A output
--        RegBOut  - 8 bit register bus B output
--        AddrMuxOut  - 16 bit indirect Address line out, to DataMIU
--
-- Revision History:
-- 01/24/2019   Sophia Liu      Initial revision
-- 01/30/2019   Sundar Pandian  Initial architecture writeup
-- 02/01/2019   Sundar Pandian  Debugged with testbench
-- 02/27/2019   Sophia Liu      Added header documentation
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

entity RegUnit is
    port(
        Clk         :  in  std_logic;                               -- system clock
        Reset       :  in  std_logic;                               -- system reset, used to init SP to all 1s
        RegIn       :  in  std_logic_vector(REGSIZE-1 downto 0);    -- input register

        -- from CU
        RegWEn      : in std_logic;                                 -- register write enable
        RegWSel     : in std_logic_vector(RADDRSIZE-1 downto 0);    -- register write select
        RegSelA     : in std_logic_vector(RADDRSIZE-1 downto 0);    -- register A select
        RegSelB     : in std_logic_vector(RADDRSIZE-1 downto 0);    -- register B select
        IORegWEn    : in std_logic;                                 -- IO register write enable
        IORegWSel   : in std_logic_vector(IOADDRSIZE-1 downto 0);   -- IO register address bus

        IndDataIn   : in std_logic_vector(ADDRSIZE-1 downto 0);     -- Indirect Address Data In, from DataMIU
        IndWEn      : in std_logic;                                 -- Indirect Address write enable, from CU
        IndAddrSel  : in ADDR_SEL;                                  -- Ind addr select, from CU
        IOOutSel    : in std_logic;                                 -- MUX select line for outputting RegA or IO

        SRegIn      : in std_logic_vector(REGSIZE-1 downto 0);      -- Status Register from ALU
        SRegOut     : out std_logic_vector(REGSIZE-1 downto 0);     -- Status Register out to ALU

        RegAOut     : out std_logic_vector(REGSIZE-1 downto 0);     -- register bus A out
        RegBOut     : out std_logic_vector(REGSIZE-1 downto 0);     -- register bus B out
        AddrMuxOut  : out std_logic_vector(ADDRSIZE-1 downto 0);    -- Indirect Address line out, to DataMIU

        ZAddrOut    : out std_logic_vector(ADDRSIZE-1 downto 0)
    );

end RegUnit;

architecture RegUnit_arc of RegUnit is

-- signals for port mapping
signal XMuxOut  : std_logic_vector(ADDRSIZE-1 downto 0);
signal YMuxOut  : std_logic_vector(ADDRSIZE-1 downto 0);
signal ZMuxOut  : std_logic_vector(ADDRSIZE-1 downto 0);
signal SPMuxOut : std_logic_vector(ADDRSIZE-1 downto 0);
signal SPReg    : std_logic_vector(ADDRSIZE-1 downto 0);
signal AMuxOut  : std_logic_vector(REGSIZE-1  downto 0);
signal SRegMuxOut : std_logic_vector(REGSIZE-1  downto 0);

signal IndAddrIn : std_logic_vector(IOADDRSIZE-1 downto 0);
signal XAddr : std_logic_vector(IOADDRSIZE-1 downto 0);
signal YAddr : std_logic_vector(IOADDRSIZE-1 downto 0);
signal ZAddr : std_logic_vector(IOADDRSIZE-1 downto 0);

component RegArray is
    port(
        Clk      :  in  std_logic;                                  -- system clock
        RegIn    :  in  std_logic_vector(REGSIZE-1 downto 0);       -- input register bus

        -- from CU
        RegWEn      : in std_logic;                                 -- register write enable, from CU
        RegWSel     : in std_logic_vector(RADDRSIZE-1 downto 0);    -- register write select, from CU
        RegSelA     : in std_logic_vector(RADDRSIZE-1 downto 0);    -- register A select, from CU
        RegSelB     : in std_logic_vector(RADDRSIZE-1 downto 0);    -- register B select, from CU

        IndDataIn   : in std_logic_vector(ADDRSIZE-1 downto 0);     -- Indirect Addr data in, from DataMIU
        IndAddrIn   : in std_logic_vector(RADDRSIZE-1 downto 0);   -- Indirect Addr value in, from DataMIU
        IndWEn      : in std_logic;                                 -- Indirect Addr write enable, from CU

        RegAOut     : out std_logic_vector(REGSIZE-1 downto 0);     -- register bus A out
        RegBOut     : out std_logic_vector(REGSIZE-1 downto 0);     -- register bus B out

        RegXOut     : out std_logic_vector(ADDRSIZE-1 downto 0);    -- register bus X out
        RegYOut     : out std_logic_vector(ADDRSIZE-1 downto 0);    -- register bus Y out
        RegZOut     : out std_logic_vector(ADDRSIZE-1 downto 0)     -- register bus Z out
    );
end component;

component Mux4to1 is
    port(
        S0          :  in      std_logic;  -- mux sel(0)
        S1          :  in      std_logic;  -- mux sel(1)
        SIn0        :  in      std_logic;  -- mux inputs
        SIn1        :  in      std_logic;  -- mux inputs
        SIn2        :  in      std_logic;  -- mux inputs
        SIn3        :  in      std_logic;  -- mux inputs
        SOut        :  out     std_logic   -- mux output
      );
end component;

component Mux2to1 is
    port(
        S0          :  in      std_logic;  -- mux sel(0)
        SIn0        :  in      std_logic;  -- mux inputs
        SIn1        :  in      std_logic;  -- mux inputs
        SOut        :  out     std_logic   -- mux output
      );
end component;

begin
    Reg: RegArray
    port map(
        Clk         => Clk,
        RegIn       => RegIn,

        -- from CU
        RegWEn      => RegWEn,
        RegWSel     => RegWSel,
        RegSelA     => RegSelA,
        RegSelB     => RegSelB,

        IndDataIn   => IndDataIn,
        IndAddrIn   => IndAddrIn(RADDRSIZE-1 downto 0),     -- only map in low RADDRSIZE bits
        IndWEn      => IndWEn,

        RegAOut     => AMuxOut,
        RegBOut     => RegBOut,

        RegXOut     => XMuxOut,
        RegYOut     => YMuxOut,
        RegZOut     => ZMuxOut
    );

    -- writing to SP Register occurs synchronously
    write_addr_reg : process (CLK)
    begin
        if Reset = '0' then
            SPReg <= (others => '1');
        elsif (rising_edge(CLK)) then
            -- writes to register only if write enabled
            -- holds value otherwise
            if IORegWEn = WRITE_EN then
                SRegMuxOut <= RegIn;
                if IORegWSel /= SREG_ADDR then
                    SRegMuxOut <= SRegIn;
                end if;
            else
                SRegMuxOut <= SRegIn;
            end if;

            -- writes to register only if write enabled and indirect address is SP_ADDR_L
            -- holds value otherwise
            if IndWEn = WRITE_EN and IndAddrIn = SP_ADDR_L then
                SPReg <= IndDataIn;
            else
                if IndAddrIn /= "XXXXX" then
                    SPReg <= SPReg;
                end if;
            end if;
        end if;
    end process write_addr_reg;

    SPMuxOut <= SPReg;

    ZAddr <= '0' & Z_ADDR_L; -- buffer for zero padding Z address line into IO reg array
    YAddr <= '0' & Y_ADDR_L; -- buffer for zero padding Y address line into IO reg array
    XAddr <= '0' & X_ADDR_L; -- buffer for zero padding X address line into IO reg array

    -- Address Mux In
    -- CU controls which indirect addressing value is written to
    InAddrMux:  for i in IOADDRSIZE-1 downto 0 generate
      InAddrMuxi: Mux4to1
        port map(
            S0          => IndAddrSel(0),
            S1          => IndAddrSel(1),
            SIn0        => ZAddr(i),
            SIn1        => SP_ADDR_L(i),
            SIn2        => YAddr(i),
            SIn3        => XAddr(i),
            SOut        => IndAddrIn(i)
      );
      end generate InAddrMux;

    -- Address Mux Out
    -- CU controls which indirect addressing value is outputted
    OutAddrMux:  for i in ADDRSIZE-1 downto 0 generate
      OutAddrMuxi: Mux4to1
        port map(
            S0          => IndAddrSel(0),
            S1          => IndAddrSel(1),
            SIn0        => ZMuxOut(i),
            SIn1        => SPMuxOut(i),
            SIn2        => YMuxOut(i),
            SIn3        => XMuxOut(i),
            SOut        => AddrMuxOut(i)
      );
      end generate OutAddrMux;

    -- Reg A Mux Out
    -- CU controls if RegA from register space, or IO reg is outputted
    OutAMux:  for i in REGSIZE-1 downto 0 generate
      OutAMuxi: Mux2to1
        port map(
            S0          => IOOutSel,
            SIn0        => AMuxOut(i),
            SIn1        => SRegMuxOut(i),
            SOut        => RegAOut(i)
      );
      end generate OutAMux;

      ZAddrOut <= ZMuxOut;
      SRegOut <= SRegMuxOut;

end RegUnit_arc;
