----------------------------------------------------------------------------
--
--  Atmel AVR ALU Test Entity Declaration
--
--  This is the entity declaration which must be used for building the ALU
--  portion of the AVR design for testing.
--
--  Revision History:
--     17 Apr 98  Glen George       Initial revision.
--     20 Apr 98  Glen George       Fixed minor syntax bugs.
--     18 Apr 04  Glen George       Updated comments and formatting.
--     21 Jan 06  Glen George       Updated comments.
--		 31 Jan 19  Sophia Liu        Updated architecture
--
----------------------------------------------------------------------------


--
--  ALU_TEST
--
--  This is the ALU testing interface.  It just brings all the important
--  ALU signals out for testing along with the Instruction Register.
--
--  Inputs:
--    IR       - Instruction Register (16 bits)
--    OperandA - first operand to ALU (8 bits) - looks like the output
--               of the register array
--    OperandB - second operand to ALU (8 bits) - looks like the output
--               of the register array
--    clock    - the system clock
--
--  Outputs:
--    Result   - result of the ALU operation selected by the Instruction
--               Register (8 bits)
--    StatReg  - Status Register contents (8 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

use work.opcodes.all;

use work.constants.all;

entity  ALU_TEST  is

    port(
        IR        :  in  opcode_word;                       -- Instruction Register
        OperandA  :  in  std_logic_vector(7 downto 0);      -- first operand
        OperandB  :  in  std_logic_vector(7 downto 0);      -- second operand
        clock     :  in  std_logic;                         -- system clock
        Result    :  out std_logic_vector(7 downto 0);      -- ALU result
        StatReg   :  out std_logic_vector(7 downto 0)       -- status register
    );

end  ALU_TEST;

architecture ALUTB of ALU_TEST is
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


signal IndDataIn    : std_logic_vector(ADDRSIZE-1 downto 0);
signal AddrMuxOut   : std_logic_vector(ADDRSIZE-1 downto 0);

signal DataAddr : std_logic_vector( 15 downto 0);
signal RegIoFlag : std_logic;
signal RegIoSelFlag : std_logic;
signal DataCtrlU : std_logic_vector(5 downto 0);

signal ALUOut : std_logic_vector(7 downto 0);
signal ProgDB : std_logic_vector(15 downto 0);

signal DataRd : std_logic;
signal DataWr : std_logic;

signal Reset : std_logic;

signal DataDB : std_logic_vector(7 downto 0);

signal MuxBOut : std_logic_vector(7 downto 0);

signal StatusBuff : std_logic_vector(7 downto 0);

begin

    ALUnit  : entity work.ALU port map(ALUaddsub, ALUsr, ALUfop, ALUcomneg, ALUSel, bitmask, CPC, OperandA, MuxBOut, SReg, Result, StatusBuff);

    MuxBOut <= Immed    when ImmedEn = IMM_EN else   -- mux to select input to operand B
               OperandB when ImmedEn = IMM_DIS;

    CtrlU   : entity work.CU port map(ProgDB, IR, SReg, load, Immed, ImmedEn, RegWEn, RegWSel,
                                    RegSelA, RegSelB, IORegWEn, IORegWSel, IndWEn, IndAddrSel,
                                    IOOutSel, DataRd, DataWr, IORegOutEn, ALUaddsub, ALUsr, ALUfop,
                                    ALUcomneg, ALUSel, bitmask, CPC, LoadIn, SRegLd,
                                    DataOffsetSel, PreSel, QOffset, DataDBWEn, DataABMux, clock,
                                    RegIoFlag, RegIoSelFlag, DataCtrlU);

    StatReg <= SReg;

    process(clock)
    begin
        if rising_edge(clock) then
            SReg <= StatusBuff;
        end if;
    end process;

end ALUTB;