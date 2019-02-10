----------------------------------------------------------------------------
--
--  Atmel AVR Data Memory Test Entity Declaration
--
--  This is the entity declaration which must be used for building the data
--  memory access portion of the AVR design for testing.
--
--  Revision History:
--     24 Apr 98  Glen George       Initial revision.
--     25 Apr 00  Glen George       Fixed entity name and updated comments.
--      2 May 02  Glen George       Updated comments.
--      3 May 02  Glen George       Fixed Reset signal type.
--     23 Jan 06  Glen George       Updated comments.
--     21 Jan 08  Glen George       Updated comments.
--     31 Jan 19  Sundar Pandian    Added support for CPU testing
--     09 Feb 19  Sophia Liu        Updated architecture
--
----------------------------------------------------------------------------


--
--  MEM_TEST
--
--  This is the data memory access testing interface.  It just brings all
--  the important data memory access signals out for testing along with the
--  Instruction Register and Program Data Bus.
--
--  Inputs:
--    IR     - Instruction Register (16 bits)
--    ProgDB - program memory data bus (16 bits)
--    Reset  - active low reset signal
--    clock  - the system clock
--
--  Outputs:
--    DataAB - data memory address bus (16 bits)
--    DataDB - data memory data bus (8 bits)
--    DataRd - data read (active low)
--    DataWr - data write (active low)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

use work.opcodes.all;
use work.constants.all;

entity  MEM_TEST  is

    port (
        IR      :  in     opcode_word;                      -- Instruction Register
        ProgDB  :  in     std_logic_vector(15 downto 0);    -- second word of instruction
        Reset   :  in     std_logic;                        -- system reset signal (active low)
        clock   :  in     std_logic;                        -- system clock
        DataAB  :  out    std_logic_vector(15 downto 0);    -- data address bus
        DataDB  :  inout  std_logic_vector(7 downto 0);     -- data data bus
        DataRd  :  out    std_logic;                        -- data read (active low)
        DataWr  :  out    std_logic                         -- data write (active low)
    );

end  MEM_TEST;

architecture behavioral of mem_test is
-- internal signal definitions
signal SReg        :  std_logic_vector(REGSIZE-1 downto 0);       -- status flags
signal load        :  std_logic;                              -- load output to tell IR register
                                                                --  when to fetch new instruction
signal Immed       :  std_logic_vector(REGSIZE-1 downto 0);  -- immediate value K
signal ImmedEn     :  std_logic;
signal RegWEn      :  std_logic;                    -- register write enable
signal RegWSel     :  std_logic_vector(RADDRSIZE-1 downto 0); -- register write select
signal RegSelA     :  std_logic_vector(RADDRSIZE-1 downto 0); -- register A select
signal RegSelB     :  std_logic_vector(RADDRSIZE-1 downto 0); -- register B select
signal IORegWEn    :  std_logic;                      -- IN command enable
signal IORegWSel   :  std_logic_vector(IOADDRSIZE-1 downto 0);   -- IO register address bus
signal IndWEn      :  std_logic;
signal IndAddrSel  :  ADDR_SEL;
signal IOOutSel    :  std_logic;
signal IORegOutEn  :  std_logic;                      -- OUT command enable
signal ALUaddsub   :  ALU_ADDSUB;
signal ALUsr       :  ALU_SR;
signal ALUfop      :  ALU_FOPS; -- operation control signals
signal ALUcomneg   :  ALU_COMNEG;
signal ALUSel      :  ALU_SELECTS; -- operation select
signal bitmask     :  BIT_MASK; -- mask for writing to flags (SReg)
signal CPC         :  std_logic;
signal LoadIn      :  LOADIN_SEL; -- selects data line into reg
signal SRegLd      :  std_logic;                      -- select line to mux status reg source
signal DataAddrSel     :  ADDR_SEL;  -- data address source select
signal DataOffsetSel   :  OFFSET_SEL;-- data address offset source select
signal PreSel          :  PREPOST_ADDR; -- data pre/post address select
signal QOffset         :  std_logic_vector(Q_OFFSET_SIZE-1 downto 0); -- address offset for data memory unit
signal DataDBWEn       :  std_logic;
signal DataABMux       :  std_logic;

signal RegIn        : std_logic_vector(REGSIZE-1 downto 0);

signal IndDataIn    : std_logic_vector(ADDRSIZE-1 downto 0);
signal RegAOut      : std_logic_vector(REGSIZE-1 downto 0);
signal RegBOut      : std_logic_vector(REGSIZE-1 downto 0);
signal AddrMuxOut   : std_logic_vector(ADDRSIZE-1 downto 0);

signal DataAddr : std_logic_vector( 15 downto 0);
signal RegIoFlag : std_logic;
signal RegIoSelFlag : std_logic;
signal DataCtrlU : std_logic_vector(5 downto 0);

begin

    CtrlU   : entity work.CU port map(ProgDB, IR, SReg, load, Immed, ImmedEn, RegWEn, RegWSel,
                                    RegSelA, RegSelB, IORegWEn, IORegWSel, IndWEn, IndAddrSel,
                                    IOOutSel, DataRd, DataWr, IORegOutEn, ALUaddsub, ALUsr, ALUfop,
                                    ALUcomneg, ALUSel, bitmask, CPC, LoadIn, SRegLd,
                                    DataOffsetSel, PreSel, QOffset, DataDBWEn, DataABMux, clock,
                                    RegIoFlag, RegIoSelFlag, DataCtrlU);

    RegIn <= Immed      when LoadIn = LD_IMM else   -- mux to select input to registers
             DataDB     when LoadIn = LD_DB else
             RegAOut    when LoadIn = LD_REGA else
             (others => 'X');

    RegU    : entity work.RegUnit port map(clock, Reset, RegIn, RegWEn, RegWSel, RegSelA, RegSelB, IORegWEn,
                                    IORegWSel, IndDataIn, IndWEn, IndAddrSel, IOOutSel,
                                    RegAOut, RegBOut, AddrMuxOut);

    DataMemU : entity work.DataMIU port map(AddrMuxOut, RegIn, QOffset, DataOffsetSel, PreSel, DataDBWEn,
                                    DataABMux, ProgDB, IndDataIn, DataDB, DataAddr);

    DataAB <= DataAddr;
    -- flags for remapping lowest addresses to registers
    DataCtrlU <= DataAddr(6) & DataAddr(4 downto 0); -- address for register or io register select
    -- use general regs or io regs instead of external memory for addresses 0 to 95 (total size of reg)
    RegIoFlag <= '1' when std_match(DataAddr, "0000000000------") or  std_match(DataAddr, "00000000010-----") else
                 '0';
    -- detect whether to use register or io register
    RegIoSelFlag <= REG_A_OUT when std_match(DataAddr, "00000000000-----") else -- use general regs if addr < reg size (32 bits)
                    IO_OUTPUT;

end architecture ; -- behavioral
