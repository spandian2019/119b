----------------------------------------------------------------------------
--
--
-- Data Memory Interface Unit
--
-- The data memory access unit generates the addresses and reads/writes
-- data for the data memory. The data is addressed as 8-bits with 16-bits
-- of address. The address may come from the instruction, the X, Y, Z, or
-- SP registers, and may be pre/post incremented/decremented or with
-- a 6-bit unsigned offset. The CU handles this with select signals for
-- the source, offset, and pre/post changes.
--
-- Ports:
--  Input:
--        DataAddr - 16-bit data address source from CU
--        XAddr    - 16-bit address source from register X
--        YAddr    - 16-bit address source from register Y
--        ZAddr    - 16-bit address source from register Z
--        AddrSel  - 2-bit address source select, from CU
--        QOffset  - 6-bit unsigned address offset source from CU
--        OffsetSel - 2-bit address offset source select from CU
--        PreSel   - pre/post address select from CU
--        DataRd   - indicates data memory is being read
--        DataWr   - indicates data memory is being
--
--  Outputs:
--        DataReg - 16-bit data address register source
--        DataAB  - 16-bit program address bus
--
-- Revision History:
-- 01/24/2019 Sophia Liu Initial revision
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

use work.opcodes.all;
use work.constants.all;
use work.ALUconstants.all;

entity DataMIU is
    port(
        DataAddr    : in std_logic_vector(ADDRSIZE-1 downto 0);         -- data indirect address source from registers
        RegIn       : in std_logic_vector(REGSIZE-1 downto 0);          -- outputted to DataDB when write enabled

        QOffset     : in std_logic_vector(Q_OFFSET_SIZE-1 downto 0);    -- unsigned address offset source from CU
        OffsetSel   : in OFFSET_SEL;                                    -- address offset source select from CU

        PreSel      : in PREPOST_ADDR;                                  -- pre/post address select from CU
        DataDBWEn   : in std_logic;                                     -- DataDB write enable from CU
        DataABMux   : in std_logic;                                     -- DataAB mux control signal

        ProgDB      : in std_logic_vector(ADDRSIZE-1 downto 0);         -- program memory data bus

        DataReg     : out std_logic_vector(ADDRSIZE-1 downto 0);        -- data address register source
        DataDB      : inout std_logic_vector(REGSIZE-1 downto 0);       -- data data bus
        DataAB      : out std_logic_vector(ADDRSIZE-1 downto 0)         -- data address bus
     );
end DataMIU;

architecture DataMIU_arc of DataMIU is

signal OffsetMuxOut : std_logic_vector(ADDRSIZE-1 downto 0); -- address adder - offset mux output
signal AddrAdderOut : std_logic_vector(ADDRSIZE-1 downto 0); -- address adder output

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

component fullAdder is
    port(
        A           :  in      std_logic;  -- adder input
        B           :  in      std_logic;  -- adder input
        Cin         :  in      std_logic;  -- carry in value
        Cout        :  out     std_logic;  -- carry out value
        Sum         :  out     std_logic   -- sum of A, B with carry in
      );
end component;

signal offset_buffer : std_logic_vector(ADDRSIZE-1 downto 0);   -- fully zero padded offset vector
signal IndAddrMuxOut : std_logic_vector(ADDRSIZE-1 downto 0);   -- Indirect Address to output to DataAB
signal CarryOut     : std_logic_vector(ADDRSIZE-1 downto 0);    -- carry for adder/subtracter
signal ProgDBLatch  : std_logic_vector(ADDRSIZE-1 downto 0);    -- latched ProgDB to output to DataAB for direct memory

begin

    offset_buffer <= Q_OFFS_ZERO_PAD & QOffset;         -- zero pad MSB of the QOffset input

    -- Offset Mux In
    OffsetMuxIn:  for i in ADDRSIZE-1 downto 0 generate
      OffsetMuxIni: Mux4to1
        port map(
            S0          => OffsetSel(0),
            S1          => OffsetSel(1),
            SIn0        => ZERO_OFFSET(i),
            SIn1        => INC_OFFSET(i),
            SIn2        => DEC_OFFSET(i),
            SIn3        => offset_buffer(i),
            SOut        => OffsetMuxOut(i)
      );
      end generate OffsetMuxIn;

    -- ADDRSIZE bit adder for indirect address manipulations
    adder0: fullAdder
    port map(
        A           => DataAddr(0),         -- indirect address to manipulate 
        B           => OffsetMuxOut(0),     -- only ever adding in offset values
        Cin         => '0',                 -- since only adding in offset, carry always '0'
        Cout        => Carryout(0),         -- set next carry
        Sum         => AddrAdderOut(0)      -- save to address adder buffer
    );
    -- other bits
    GenAdder:  for i in 1 to ADDRSIZE - 1 generate
    adderi: fullAdder
    port map(
        A           => DataAddr(i),         -- indirect address to manipulate
        B           => OffsetMuxOut(i),     -- only ever adding in offset values
        Cin         => CarryOut(i-1),       -- carry in from last bit add
        Cout        => Carryout(i),         -- set next carry
        Sum         => AddrAdderOut(i)      -- save to address adder buffer
    );
    end generate GenAdder;

    -- PrePost Indirect Addressing Mux
    -- used to output proper indirect address value to DataAB (pre or post operation)
    --  while still outputting the proper manipulated address to write to register space
    PrePostMux:  for i in ADDRSIZE-1 downto 0 generate
      OffsetMuxIni: Mux2to1
        port map(
            S0          => PreSel,          -- pre select control line
            SIn0        => DataAddr(i),
            SIn1        => AddrAdderOut(i),
            SOut        => IndAddrMuxOut(i)
      );
      end generate PrePostMux;

    DataReg <= AddrAdderOut;                -- output manipulated address to write to register space

    -- latch ProgDB when DataABMux signals to output ProgDB to DataAB
    latch_ProgDB : process (DataABMux)
    begin
        if (rising_edge(DataABMux)) then
            ProgDBLatch <= ProgDB;
        end if;
    end process latch_ProgDB;

    -- DataAB either outputs indirect address value or direct memory address value
    DataAB <= IndAddrMuxOut when DataABMux = IND_ADDR else
              ProgDBLatch;

    -- DataDB outputs RegIn when enabled, or high impedance so it can still be read
    DataDB <= RegIn  when DataDBWEn = WRITE_EN else
              "ZZZZZZZZ";

end DataMIU_arc;







