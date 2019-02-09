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

entity RegUnit is
    port(
        Clk         : in  std_logic;                          -- system clock
        RegIn       : in  std_logic_vector(REGSIZE-1 downto 0);       -- input register bus

        -- from CU
        RegWEn      : in std_logic;                     -- register write enable
        RegWSel     : in std_logic_vector(RADDRSIZE-1 downto 0);  -- register write select
        RegSelA     : in std_logic_vector(RADDRSIZE-1 downto 0);  -- register A select
        RegSelB     : in std_logic_vector(RADDRSIZE-1 downto 0);  -- register B select
        IORegWEn    : in std_logic;                     -- register write enable
        IORegWSel   : in std_logic_vector(IOADDRSIZE-1 downto 0);   -- IO register address bus

        IndDataIn   : in std_logic_vector(ADDRSIZE-1 downto 0); --
        IndWEn      : in std_logic;
        IndAddrSel  : in ADDR_SEL;
        IOOutSel    : in std_logic;

        RegAOut     : out std_logic_vector(REGSIZE-1 downto 0);       -- register bus A out
        RegBOut     : out std_logic_vector(REGSIZE-1 downto 0);       -- register bus B out
        AddrMuxOut  : out std_logic_vector(ADDRSIZE-1 downto 0)
    );

end RegUnit;

architecture RegUnit_arc of RegUnit is

signal AddrMux  : integer;
signal XMuxOut  : std_logic_vector(ADDRSIZE-1 downto 0);
signal YMuxOut  : std_logic_vector(ADDRSIZE-1 downto 0);
signal ZMuxOut  : std_logic_vector(ADDRSIZE-1 downto 0);
signal SPMuxOut : std_logic_vector(ADDRSIZE-1 downto 0);
signal AMuxOut  : std_logic_vector(REGSIZE-1  downto 0);
signal IOMuxOut : std_logic_vector(REGSIZE-1  downto 0);

signal IndAddrIn : std_logic_vector(IOADDRSIZE-1 downto 0);
signal XAddr : std_logic_vector(IOADDRSIZE-1 downto 0);
signal YAddr : std_logic_vector(IOADDRSIZE-1 downto 0);
signal ZAddr : std_logic_vector(IOADDRSIZE-1 downto 0);

component RegArray is
    port(
    Clk      :  in  std_logic;                          -- system clock
    RegIn    :  in  std_logic_vector(REGSIZE-1 downto 0);       -- input register bus

    -- from CU
    RegWEn      : in std_logic;                     -- register write enable
    RegWSel     : in std_logic_vector(RADDRSIZE-1 downto 0);  -- register write select
    RegSelA     : in std_logic_vector(RADDRSIZE-1 downto 0);  -- register A select
    RegSelB     : in std_logic_vector(RADDRSIZE-1 downto 0);  -- register B select

    IndDataIn   : in std_logic_vector(ADDRSIZE-1 downto 0); --
    IndAddrIn   : in std_logic_vector(RADDRSIZE-1 downto 0); --
    IndWEn      : in std_logic;

    RegAOut     : out std_logic_vector(REGSIZE-1 downto 0);     -- register bus A out
    RegBOut     : out std_logic_vector(REGSIZE-1 downto 0);     -- register bus B out

    RegXOut     : out std_logic_vector(ADDRSIZE-1 downto 0);    -- register bus X out
    RegYOut     : out std_logic_vector(ADDRSIZE-1 downto 0);    -- register bus Y out
    RegZOut     : out std_logic_vector(ADDRSIZE-1 downto 0)     -- register bus Z out
    );
end component;

component IORegArray is
    port(
        Clk      :  in  std_logic;                          -- system clock
        RegIn    :  in  std_logic_vector(REGSIZE-1 downto 0);       -- input register bus

        -- from CU
        IORegWEn    : in std_logic;                     -- register write enable
        IORegWSel   : in std_logic_vector(IOADDRSIZE-1 downto 0);   -- IO register address bus

        IndDataIn   : in std_logic_vector(ADDRSIZE-1 downto 0); --
        IndAddrIn   : in std_logic_vector(IOADDRSIZE-1 downto 0); --
        IndWEn      : in std_logic;

        IORegOut    :  out std_logic_vector(REGSIZE-1 downto 0);       -- register bus B out
        SPRegOut    :  out std_logic_vector(ADDRSIZE-1 downto 0)
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
        RegSelB     => RegSelA,

        IndDataIn   => IndDataIn,
        IndAddrIn   => IndAddrIn(RADDRSIZE-1 downto 0),
        IndWEn      => IndWEn,

        RegAOut     => AMuxOut,
        RegBOut     => RegBOut,

        RegXOut     => XMuxOut,
        RegYOut     => YMuxOut,
        RegZOut     => ZMuxOut
    );

    IOReg: IORegArray
    port map(
        Clk         => Clk,
        RegIn       => RegIn,

        -- from CU
        IORegWEn    => IORegWEn,
        IORegWSel   => IORegWSel,

        IndDataIn   => IndDataIn,
        IndAddrIn   => IndAddrIn,
        IndWEn      => IndWEn,

        IORegOut    => IOMuxOut,
        SPRegOut    => SPMuxOut
    );

    ZAddr <= '0' & Z_ADDR_L;
    YAddr <= '0' & Y_ADDR_L;
    XAddr <= '0' & X_ADDR_L;

    -- Address Mux In
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
    OutAMux:  for i in RADDRSIZE-1 downto 0 generate
      OutAMuxi: Mux2to1
        port map(
            S0          => IOOutSel,
            SIn0        => AMuxOut(i),
            SIn1        => IOMuxOut(i),
            SOut        => RegBOut(i)
      );
      end generate OutAMux;
end RegUnit_arc;