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

entity DataMIU is 
    port(
        DataAddr    : in std_logic_vector(ADDRSIZE-1 downto 0); -- data address source from registers
        
        QOffset     : in std_logic_vector(Q_OFFSET_SIZE-1 downto 0);  -- unsigned address offset source from CU 
        OffsetSel   : in OFFSET_SEL;-- address offset source select from CU 
        
        PreSel      : in PREPOST_ADDR; -- pre/post address select from CU 
        
        --DataRd      : in std_logic; -- indicates data memory is being read
        --DataWr      : in std_logic; -- indicates data memory is being written
        
        DataReg     : out std_logic_vector(ADDRSIZE-1 downto 0); -- data address register source 
        DataAB      : out std_logic_vector(ADDRSIZE-1 downto 0) -- program address bus
     );
end DataMIU; 

architecture DataMIU_arc of DataMIU is

signal OffsetMuxOut : std_logic_vector(ADDRSIZE-1 downto 0); -- address adder - offset mux output
signal AddrAdderOut : std_logic_vector(ADDRSIZE-1 downto 0); -- address adder output


begin

    -- Offset Mux In
    OffsetMuxIn:  for i in ADDRSIZE-1 downto 0 generate
      OffsetMuxIni: Mux4to1
        port map(
            S0          => OffsetSel(0),
            S1          => OffsetSel(1),
            SIn0        => ZERO_OFFSET,
            SIn1        => INC_OFFSET,
            SIn2        => DEC_OFFSET,
            SIn3        => Q_OFFS_ZERO_PAD & QOffset,
            SOut        => OffsetMuxOut(i)
      );
      end generate OffsetMuxIn;

    -- ADDRSIZE bit adder for addressing modes
    -- ASKFAB
    -- does making Cin = 0 simplify automatically or nah?
    Addradder: Adder
        generic map (bitsize => ADDRSIZE)
        port map(
            A           => DataAddr,
            B           => OffsetMuxOut,
            Cin         => '0',
            Cout        => '-',         -- might error ASKFAB
            Sum         => AddrAdderOut
      );

    -- PrePost Indirect Addressing Mux
    -- ASKFAB -- why is this 2:1 mux necessary, why not only add
    PrePostMux:  for i in ADDRSIZE-1 downto 0 generate
      OffsetMuxIni: Mux2to1
        port map(
            S0          => ALUOp(PREPOSTFLAG),
            SIn0        => AddrAdderOut(i),
            SIn1        => DataAddr(i),
            SOut        => DataAB(i)
      );
      end generate PrePostMux;

    DataReg <= AddrAdderOut;

end DataMIU_arc;







