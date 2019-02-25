----------------------------------------------------------------------------
--
--  Atmel AVR CPU Entity Declaration
--
--  This is the entity declaration for the complete AVR CPU.  The design
--  should implement this entity to make testing possible.
--
--  Revision History:
--     11 May 98  Glen George       Initial revision.
--      9 May 00  Glen George       Updated comments.
--      7 May 02  Glen George       Updated comments.
--     21 Jan 08  Glen George       Updated comments.
--
----------------------------------------------------------------------------


--
--  AVR_CPU
--
--  This is the complete entity declaration for the AVR CPU.  It is used to
--  test the complete design.
--
--  Inputs:
--    ProgDB - program memory data bus (16 bits)
--    Reset  - active low reset signal
--    INT0   - active low interrupt
--    INT1   - active low interrupt
--    clock  - the system clock
--
--  Outputs:
--    ProgAB - program memory address bus (16 bits)
--    DataAB - data memory address bus (16 bits)
--    DataWr - data write signal
--    DataRd - data read signal
--
--  Inputs/Outputs:
--    DataDB - data memory data bus (8 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

use work.opcodes.all;
use work.constants.all;


entity  AVR_CPU  is

    port (
        ProgDB  :  in     std_logic_vector(15 downto 0);   -- program memory data bus
        Reset   :  in     std_logic;                       -- reset signal (active low)
        INT0    :  in     std_logic;                       -- interrupt signal (active low)
        INT1    :  in     std_logic;                       -- interrupt signal (active low)
        clock   :  in     std_logic;                       -- system clock
        ProgAB  :  out    std_logic_vector(15 downto 0);   -- program memory address bus
        DataAB  :  out    std_logic_vector(15 downto 0);   -- data memory address bus
        DataWr  :  out    std_logic;                       -- data memory write enable (active low)
        DataRd  :  out    std_logic;                       -- data memory read enable (active low)
        DataDB  :  inout  std_logic_vector(7 downto 0)     -- data memory data bus
    );

end  AVR_CPU;

architecture behavioral of AVR_CPU is

-- internal signal definitions
signal SReg        :  std_logic_vector(REGSIZE-1 downto 0);       -- status flags
signal ZeroFlag    :  std_logic;
signal SBFlag      :  std_logic;
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

signal Load            : std_logic;
signal ProgSourceSel   : SOURCE_SEL;
signal ProgIRSource    : std_logic_vector(ADDRSIZE-1 downto 0);

signal StatReg         : std_logic_vector(REGSIZE-1 downto 0);

signal ALUResult       : std_logic_vector(REGSIZE-1 downto 0);

signal StatusBuff : std_logic_vector(7 downto 0);
signal MuxBOut : std_logic_vector(7 downto 0);

begin

    CtrlU   : entity work.CU port map(ProgDB, SReg, ZeroFlag, SBFlag, Immed, ImmedEn, RegWEn, RegWSel,
                                    RegSelA, RegSelB, IORegWEn, IORegWSel, IndWEn, IndAddrSel,
                                    IOOutSel, DataRd, DataWr, IORegOutEn, ALUaddsub, ALUsr, ALUfop,
                                    ALUcomneg, ALUSel, bitmask, CPC, LoadIn, SRegLd,
                                    DataOffsetSel, PreSel, QOffset, DataDBWEn, DataABMux, clock,
                                    RegIoFlag, RegIoSelFlag, DataCtrlU, Load, ProgSourceSel, ProgIRSource);

    -- mux to select input to registers
    RegIn <= Immed                                  when LoadIn = LD_IMM else
             DataDB                                 when LoadIn = LD_DB else
             RegAOut                                when LoadIn = LD_REGA else
             ALUResult                              when LoadIn = LD_ALU else
             ProgDB(ADDRSIZE-1 downto ADDRSIZE/2)   when LoadIn = LD_PROG_HI else
             ProgDB(ADDRSIZE/2-1 downto 0)          when LoadIn = LD_PROG_LO else
             "XXXXXXXX";

    -- mux to select input to Register B of ALU
    MuxBOut <= Immed    when ImmedEn = IMM_EN else   -- mux to select input to operand B
               RegBOut  when ImmedEn = IMM_DIS;

    ALUnit  : entity work.ALU port map(ALUaddsub, ALUsr, ALUfop, ALUcomneg, ALUSel, bitmask, CPC,
                                       RegAOut, MuxBOut, SReg, ALUResult, StatusBuff, SBFlag, ZeroFlag);

    RegU    : entity work.RegUnit port map(clock, Reset, RegIn, RegWEn, RegWSel, RegSelA, RegSelB, IORegWEn,
                                    IORegWSel, IndDataIn, IndWEn, IndAddrSel, IOOutSel,
                                    RegAOut, RegBOut, AddrMuxOut);

    DataMemU : entity work.DataMIU port map(AddrMuxOut, RegIn, QOffset, DataOffsetSel, PreSel, DataDBWEn,
                                    DataABMux, ProgDB, IndDataIn, DataDB, DataAddr);

    ProgMemU : entity work.ProgMIU port map(Reset, clock, load, ProgSourceSel, ProgIRSource, AddrMuxOut, DataDB, ProgAB);

    DataAB <= DataAddr;
    -- flags for remapping lowest addresses to registers
    DataCtrlU <= DataAddr(6) & DataAddr(4 downto 0); -- address for register or io register select
    -- use general regs or io regs instead of external memory for addresses 0 to 95 (total size of reg)
    RegIoFlag <= '1' when std_match(DataAddr, "0000000000------") or  std_match(DataAddr, "00000000010-----") else
                 '0';
    -- detect whether to use register or io register
    RegIoSelFlag <= REG_A_OUT when std_match(DataAddr, "00000000000-----") else -- use general regs if addr < reg size (32 bits)
                    IO_OUTPUT;

    StatReg <= SReg;

    process(clock)
    begin
        if rising_edge(clock) then
            SReg <= StatusBuff;
        end if;
    end process;


end architecture ; -- behavioral